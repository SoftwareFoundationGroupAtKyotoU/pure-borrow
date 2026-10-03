{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | A lock-free Chase-Lev work-stealing deque.

One thread, its owner, pushes with 'pushFront' and 'pushFronts' and pops with 'tryPopFront'; only the owner may call these three.
Any thread, the owner included, may steal from the back with 'tryPopBack' and 'stealHalf'.
'close' ends the deque: later pushes are dropped, and once it is empty, thieves get @Nothing@.
-}
module Control.Concurrent.Queue.ChaseLev (
  ChaseLevDeq,
  newDeq,
  pushFront,
  pushFronts,
  tryPopBack,
  stealHalf,
  StealResult (..),
  tryPopFront,
  estimateSize,
  capacity,
  close,
  isClosed,
) where

import Control.Monad (forM_, unless, when, (<$!>))
import Data.Atomics (loadLoadBarrier, storeLoadBarrier, writeBarrier)
import Data.Bits ((.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Primitive.Array (MutableArray)
import Data.Primitive.Array qualified as Array
import Data.Primitive.PrimVar (PrimVar, casInt, newPrimVar, readPrimVar, writePrimVar)
import GHC.Exts (RealWorld)
import Math.NumberTheory.Logarithms (intLog2')

data ChaseLevDeq a = CL
  { top :: {-# UNPACK #-} !(PrimVar RealWorld Int)
  , activeArray :: !(IORef (MutableArray RealWorld a))
  , bottom :: {-# UNPACK #-} !(PrimVar RealWorld Int)
  , closed :: {-# UNPACK #-} !(IORef Bool)
  , estimatedSize :: {-# UNPACK #-} !(PrimVar RealWorld Int)
  }

data Stat = Stat {top, bottom :: !Int}

{- Note [Growing the ring buffer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'top' and 'bottom' are logical indices, never reduced modulo the capacity, and every reader finds element @k@ at slot @k@ modulo the capacity of the array it loaded.
So a resize copies each live element @k@ from slot @k@ modulo the old capacity to slot @k@ modulo the new one.
Both capacities are powers of two, and fewer elements are live than the old capacity, so the live range wraps around each array at most once, and around both at the same @k@ when it wraps around the new one: at most two runs, each one 'Array.copyMutableArray'.

Up to 0.1.0.0 the copy put element @k@ at slot @k - capa * quot top capa@ of the new array, where @capa@ is the old capacity.
Readers look there only when @capa * quot top capa@ is a multiple of the new capacity, which for a doubling means when @quot top capa@ is even; otherwise every live element was in the wrong slot, and readers took the new array's filler, 'undefined', or another element.

The 'writeBarrier' orders the copy before the new array is published, for a thief that loads it.
With GHC 9.10 to 9.14 on ARM64, 'writeIORef' compiles to a store-release, which makes the barrier redundant there, but GHC does not document that ordering.
-}

{- Note [Barriers of a thief]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A thief reads 'top', then 'bottom', then the array and the slot of the element it takes, and claims the element with a CAS on 'top'.
The owner pushes by writing the slot, then 'writeBarrier', then 'bottom'; it pops by writing 'bottom', then 'storeLoadBarrier', then reading 'top', and uses a CAS only for the last element.

A full barrier separates the thief's reads of 'top' and 'bottom', as a seq_cst fence does in the C11 Chase-Lev deque of Lê et al. (PPoPP 2013).
A thief that took the previous element knows 'top' from its own CAS, whose write may not be visible yet.
If its read of 'bottom' overtook that write, the owner could write 'bottom', read the old 'top', and take the same element without a CAS, while the thief's next CAS succeeded too.
GHC documents 'casIntArray#' as a full barrier, but on ARM64 without the LSE atomics it compiles to a loop of exclusive loads and stores, after which a later load can still pass the store: a litmus test of such a loop on an Apple M4 saw a later plain load pass it 98,420 times in 5 million runs.
'stealHalf' puts the same barrier before each further read of 'bottom'.

After a thief reads 'bottom', 'takeBack' puts a load barrier before the reads of the array and the slot.
Without it, a weakly ordered machine such as ARM64 may read the slot or the array first, and pair the new 'bottom' with what the slot held before the owner's write, or with the array that a resize has replaced; the CAS on 'top' succeeds all the same.
On an Apple M4 that took one element twice and lost another in 3 of 6 runs of an owner pushing into a nearly empty deque while nine thieves stole, and in none of 10 runs with the barrier.

'stealHalf' takes its elements one at a time, each with a CAS of its own, because the owner pops without a CAS whenever more than one element is left.
A batch claimed with one CAS, as up to 0.1.0.0, could hold elements that the owner had popped, and even pushed again, after the thief read 'bottom': the elements it popped were taken twice, and those it pushed were lost.
-}

newDeq :: IO (ChaseLevDeq a)
newDeq = do
  !top <- newPrimVar 0
  !activeArray <- newIORef =<< Array.newArray 32 undefined
  !closed <- newIORef False
  !bottom <- newPrimVar 0
  !estimatedSize <- newPrimVar 0
  pure CL {..}

getStat :: ChaseLevDeq a -> IO Stat
{-# INLINE getStat #-}
getStat dq = Stat <$> readPrimVar dq.top <*> readPrimVar dq.bottom

{-# INLINE capacity #-}
capacity :: ChaseLevDeq a -> IO Int
capacity = fmap Array.sizeofMutableArray . readIORef . (.activeArray)

occupancy :: Stat -> Int
{-# INLINE occupancy #-}
occupancy = (-) <$> (.bottom) <*> (.top)

pushFront :: ChaseLevDeq a -> a -> IO ()
pushFront q = pushFronts q . (: [])

-- | Puts the last element on the front.
pushFronts :: ChaseLevDeq a -> [a] -> IO ()
pushFronts _ [] = pure ()
pushFronts q !a = do
  let !n = length a
  closed <- readIORef q.closed
  unless closed do
    -- One read of the array, so that the copy below is bounded by the array it reads.
    oldArr <- readIORef q.activeArray
    let !capa = Array.sizeofMutableArray oldArr
    !stat <- getStat q
    let !size = occupancy stat
    arr <-
      if size + n >= capa - 1
        then do
          let !newCapa = (2 * capa) `max` (2 ^ (intLog2' (size + n) + 1))
          newArr <- Array.newArray newCapa undefined
          -- See Note [Growing the ring buffer].
          let copyFrom !k = when (k < stat.bottom) do
                let !from = k .&. (capa - 1)
                    !to = k .&. (newCapa - 1)
                    !len = minimum [stat.bottom - k, capa - from, newCapa - to]
                Array.copyMutableArray newArr to oldArr from len
                copyFrom (k + len)
          copyFrom stat.top
          writeBarrier
          writeIORef q.activeArray newArr
          pure newArr
        else pure oldArr

    let !curCapa = Array.sizeofMutableArray arr
    forM_ (zip [0 ..] a) $ \(!i, !x) ->
      Array.writeArray arr ((stat.bottom + i) .&. (curCapa - 1)) x
    writeBarrier
    writePrimVar q.bottom $! stat.bottom + n
    writePrimVar q.estimatedSize $! size + n

{- | Pops the front element, the one pushed last.

  * @Just (Just a)@   — got an element
  * @Just Nothing@    — empty, or a thief took the last element first
  * @Nothing@         — a thief took the last element first, and the deque is closed
-}
tryPopFront :: ChaseLevDeq a -> IO (Maybe (Maybe a))
tryPopFront q = do
  !b <- subtract 1 <$> readPrimVar q.bottom
  writePrimVar q.bottom b
  storeLoadBarrier
  !t <- readPrimVar q.top

  !arr <- readIORef q.activeArray
  let !capa = Array.sizeofMutableArray arr

  -- NOTE: Do not force, otherwise undefined will hit
  task <- Array.readArray arr (b .&. (capa - 1))
  if
    | b == t -> do
        -- last one element - might be stolen!
        let !t' = t + 1
        !old <- casInt q.top t t'
        let !success = old == t
        writePrimVar q.bottom t'
        writePrimVar q.estimatedSize 0
        if success
          then pure $ Just $ Just task
          else do
            closed <- readIORef q.closed
            if closed
              then pure Nothing
              else pure $ Just Nothing
    | b > t -> do
        writePrimVar q.estimatedSize $! b - t
        pure $ Just $ Just task
    | otherwise -> do
        writePrimVar q.bottom t
        writePrimVar q.estimatedSize 0
        pure $ Just Nothing

data StealResult a = Found a | Empty | Race
  deriving (Show, Eq, Ord)

{- | Steals the back element.

  * @Nothing@         — closed (end-of-stream)
  * @Just Empty@      — open and empty (would block)
  * @Just Race@       — the owner or another thief took it first
  * @Just (Found a)@  — got an element
-}
tryPopBack :: ChaseLevDeq a -> IO (Maybe (StealResult a))
tryPopBack q = do
  !t <- readPrimVar q.top
  -- See Note [Barriers of a thief].
  storeLoadBarrier
  !b <- readPrimVar q.bottom
  if t >= b
    then do
      closed <- readIORef q.closed
      if closed
        then pure Nothing
        else pure $ Just Empty
    else
      takeBack q t >>= \case
        Just task -> do
          writePrimVar q.estimatedSize $! b - t - 1
          pure $! Just $ Found task
        Nothing -> pure $ Just Race

-- | Take element @t@, which a thief has seen below 'bottom', if 'top' is still @t@; see Note [Barriers of a thief].
takeBack :: ChaseLevDeq a -> Int -> IO (Maybe a)
{-# INLINE takeBack #-}
takeBack q t = do
  loadLoadBarrier
  arr <- readIORef q.activeArray
  let !capa = Array.sizeofMutableArray arr
  -- NOTE: we must not force, otherwise undefined will hit
  task <- Array.readArray arr (t .&. (capa - 1))
  !old <- casInt q.top t (t + 1)
  pure $! if old == t then Just task else Nothing

{- | Steals about half of the elements, the back element first.

It takes them one at a time, as 'tryPopBack' does, so it returns fewer when the owner or another thief takes the rest meanwhile (Note [Barriers of a thief]).
-}
stealHalf :: ChaseLevDeq a -> IO (Maybe (StealResult (NonEmpty a)))
stealHalf q = do
  !t <- readPrimVar q.top
  -- See Note [Barriers of a thief].
  storeLoadBarrier
  !b <- readPrimVar q.bottom
  if t >= b
    then do
      closed <- readIORef q.closed
      if closed
        then pure Nothing
        else pure $ Just Empty
    else do
      let !avail = b - t
          !count = if avail == 1 then 1 else avail `quot` 2
          more !k
            | k >= t + count = pure []
            | otherwise = do
                storeLoadBarrier
                !b' <- readPrimVar q.bottom
                if k >= b'
                  then pure []
                  else
                    takeBack q k >>= \case
                      Just task -> (task :) <$> more (k + 1)
                      Nothing -> pure []
      takeBack q t >>= \case
        Nothing -> pure $ Just Race
        Just task -> do
          rest <- more (t + 1)
          writePrimVar q.estimatedSize $! max 0 (avail - 1 - length rest)
          pure $! Just $ Found (task :| rest)

estimateSize :: ChaseLevDeq a -> IO Int
{-# INLINE estimateSize #-}
estimateSize q = readPrimVar q.estimatedSize

close :: ChaseLevDeq a -> IO ()
{-# INLINE close #-}
close q = writeIORef q.closed True

isClosed :: ChaseLevDeq a -> IO Bool
isClosed q = do
  closed <- readIORef q.closed
  if closed
    then do
      !size <- occupancy <$!> getStat q
      pure $! size == 0
    else pure False
