{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

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
  close,
  isClosed,
) where

import Control.Monad (forM_, unless, (<$!>))
import Data.Atomics (loadLoadBarrier, storeLoadBarrier, writeBarrier)
import Data.Bits ((.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Primitive.Array (MutableArray)
import Data.Primitive.Array qualified as Array
import Data.Primitive.PrimVar (PrimVar, casInt, newPinnedPrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Vector qualified as V
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

newDeq :: IO (ChaseLevDeq a)
newDeq = do
  !top <- newPrimVar 0
  !activeArray <- newIORef =<< Array.newArray 128 undefined
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
    !capa <- capacity q
    !stat <- getStat q
    let !size = occupancy stat
    arr <-
      if size + n >= capa - 1
        then do
          let !start = stat.top .&. (capa - 1)
              !end = stat.bottom .&. (capa - 1)
              !newCapa = (2 * capa) `max` (2 ^ (intLog2' (size + n) + 1))
          oldArr <- readIORef q.activeArray
          newArr <- Array.newArray newCapa undefined
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
    forM_ (zip [0 ..] a) $ \(!i, !x) ->
      Array.writeArray arr ((stat.bottom + i) .&. (curCapa - 1)) x
    writeBarrier
    writePrimVar q.bottom $! stat.bottom + n
    writePrimVar q.estimatedSize $! size + n

{- |
  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
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
        if success
          then pure $ Just $ Just task
          else do
            closed <- readIORef q.closed
            if closed
              then pure Nothing
              else pure $ Just Nothing
    | b > t -> do
        writePrimVar q.estimatedSize $! b - t - 1
        pure $ Just $ Just task
    | otherwise -> do
        writePrimVar q.bottom t
        pure $ Just Nothing

data StealResult a = Found a | Empty | Race
  deriving (Show, Eq, Ord)

{- |
  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopBack :: ChaseLevDeq a -> IO (Maybe (StealResult a))
tryPopBack q = do
  !t <- readPrimVar q.top
  loadLoadBarrier
  !b <- readPrimVar q.bottom
  if t >= b
    then do
      closed <- readIORef q.closed
      if closed
        then pure Nothing
        else pure $ Just Empty
    else do
      arr <- readIORef q.activeArray
      let !capa = Array.sizeofMutableArray arr
      -- NOTE: we must not force, otherwise undefined will hit
      task <- Array.readArray arr (t .&. (capa - 1))
      let !t' = t + 1
      !old <- casInt q.top t t'
      let !success = old == t
      if success
        then do
          writePrimVar q.estimatedSize $! b - t - 1
          pure $! Just $ Found task
        else pure $ Just Race

-- | back-element first
stealHalf :: ChaseLevDeq a -> IO (Maybe (StealResult (NonEmpty a)))
stealHalf q = do
  !t <- readPrimVar q.top
  loadLoadBarrier
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
          !t' = t + count
      arr <- readIORef q.activeArray
      let !capa = Array.sizeofMutableArray arr
      -- NOTE: we must not force, otherwise undefined will hit
      tasks <- V.generateM count \i ->
        Array.readArray arr $ (t + i) .&. (capa - 1)
      !old <- casInt q.top t t'
      let !success = old == t
      if success
        then do
          writePrimVar q.estimatedSize $! avail - count
          pure $! Just $ Found $ NE.fromList $ V.toList tasks
        else pure $ Just Race

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
