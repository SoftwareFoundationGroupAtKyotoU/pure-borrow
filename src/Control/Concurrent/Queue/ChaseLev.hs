{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Control.Concurrent.Queue.ChaseLev (
  ChaseLevDeq,
  newDeq,
  pushFront,
  tryPopBack,
  tryPopFront,
  estimateSize,
  close,
  isClosed,
) where

import Control.Concurrent (yield)
import Control.Monad (unless, (<$!>))
import Data.Atomics (loadLoadBarrier, storeLoadBarrier, writeBarrier)
import Data.Atomics.Counter (AtomicCounter, casCounter, newCounter, peekCTicket, readCounter, readCounterForCAS, writeCounter)
import Data.Bits ((.&.))
import Data.Function (fix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Primitive.Array (MutableArray)
import Data.Primitive.Array qualified as Array
import GHC.Exts (RealWorld)

data ChaseLevDeq a = CL
  { top :: {-# UNPACK #-} !AtomicCounter
  , activeArray :: !(IORef (MutableArray RealWorld (Maybe a)))
  , closed :: {-# UNPACK #-} !(IORef Bool)
  , bottom :: {-# UNPACK #-} !AtomicCounter
  }

data Stat = Stat {top, bottom :: !Int}

newDeq :: IO (ChaseLevDeq a)
newDeq = do
  !top <- newCounter 0
  !activeArray <- newIORef =<< Array.newArray 128 Nothing
  !closed <- newIORef False
  !bottom <- newCounter 0
  pure CL {..}

getStat :: ChaseLevDeq a -> IO Stat
{-# INLINE getStat #-}
getStat = liftA2 Stat <$> readCounter . (.top) <*> readCounter . (.bottom)

{-# INLINE capacity #-}
capacity :: ChaseLevDeq a -> IO Int
capacity = fmap Array.sizeofMutableArray . readIORef . (.activeArray)

occupancy :: Stat -> Int
{-# INLINE occupancy #-}
occupancy = (-) <$> (.bottom) <*> (.top)

pushFront :: ChaseLevDeq a -> a -> IO ()
pushFront q !a = do
  closed <- readIORef q.closed
  unless closed do
    !capa <- capacity q
    !stat <- getStat q
    let !size = occupancy stat
    arr <-
      if size == capa - 1
        then do
          let !start = stat.top .&. (capa - 1)
              !end = stat.bottom .&. (capa - 1)
              !newCapa = 2 * capa
          oldArr <- readIORef q.activeArray
          newArr <- Array.newArray newCapa Nothing
          if end >= start
            then do
              Array.copyMutableArray newArr start oldArr start size
            else do
              let !lhSize = capa - start
                  !rhSize = size - lhSize
              Array.copyMutableArray newArr start oldArr start lhSize
              Array.copyMutableArray newArr (start + lhSize) oldArr 0 rhSize
          writeIORef q.activeArray newArr
          pure newArr
        else readIORef q.activeArray

    let !curCapa = Array.sizeofMutableArray arr
    Array.writeArray arr (stat.bottom .&. (curCapa - 1)) (Just a)
    writeBarrier
    writeCounter q.bottom $! stat.bottom + 1

{- |
  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopFront :: ChaseLevDeq a -> IO (Maybe (Maybe a))
tryPopFront q = do
  !b <- subtract 1 <$> readCounter q.bottom
  writeCounter q.bottom b
  storeLoadBarrier
  !t <- readCounterForCAS q.top

  !arr <- readIORef q.activeArray
  let !capa = Array.sizeofMutableArray arr

  -- NOTE: Do not force, otherwise undefined will hit
  task <- Array.readArray arr (b .&. (capa - 1))
  if b == t
    then do
      -- last one element - might be stolen!
      let !t' = t + 1
      (!success, _) <- casCounter q.top t t'
      writeCounter q.bottom t'
      if success
        then pure $ Just $ task
        else do
          closed <- readIORef q.closed
          if closed
            then pure Nothing
            else pure $ Just Nothing
    else pure $ Just task

{- |
  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopBack :: ChaseLevDeq a -> IO (Maybe (Maybe a))
tryPopBack q = fix \self -> do
  !t <- readCounterForCAS q.top
  loadLoadBarrier
  b <- readCounter q.bottom
  if t == b
    then do
      closed <- readIORef q.closed
      if closed
        then pure Nothing
        else pure $ Just Nothing
    else do
      arr <- readIORef q.activeArray
      let !capa = Array.sizeofMutableArray arr
      -- NOTE: we must not force, otherwise undefined will hit
      task <- Array.readArray arr (t .&. (capa - 1))
      let !t' = t + 1
      (!success, _) <- casCounter q.top t t'
      if success
        then pure $! Just task
        else yield *> self -- TODO: perhaps we can return 'Abort' to continue to other queue?

estimateSize :: ChaseLevDeq a -> IO Int
{-# INLINE estimateSize #-}
estimateSize = fmap (max 0 . occupancy) . getStat

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
