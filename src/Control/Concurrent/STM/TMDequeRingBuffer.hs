{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoLinearTypes #-}

module Control.Concurrent.STM.TMDequeRingBuffer (
  -- * The TMDeque type
  TMDeque,

  -- * Construction
  newTMDeque,
  newTMDequeIO,

  -- * Push operations
  pushFrontTMDeque,

  -- * Pop operations (blocking)
  popFrontTMDeque,
  popBackTMDeque,

  -- * Pop operations (non-blocking)
  tryPopFrontTMDeque,
  tryPopBackTMDeque,

  -- * Closing & queries
  closeTMDeque,
  isClosedTMDeque,
  isClosedTMDequeIO,
  isEmptyTMDeque,
  estimateSizeTMDequeIO,
  sizeTMDeque,
) where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Array.Base (newArray_, readArray, writeArray)
import Data.Function (fix, (&))

{- | 0 | 1 | 2 | ... | i | ... | N - 1 |
      ^             ^
      |             |
     back         front
-}
data TMDeque a = TMDeque
  { closed :: TVar Bool
  , ringBuffer :: TVar (TArray Int a)
  , capacity :: TVar Int
  , front :: TVar Int
  , back :: TVar Int
  }

newtype UniqIx = UniqIx Int

initialCapacity :: Int
initialCapacity = 64

-- | Create a new empty 'TMDeque'.
newTMDeque :: STM (TMDeque a)
newTMDeque =
  TMDeque
    <$> newTVar False
    <*> (newTVar =<< newArray_ (0, initialCapacity - 1))
    <*> newTVar initialCapacity
    <*> newTVar 0
    <*> newTVar 0

-- | IO variant of 'newTMDeque', which is faster without STM transaction overhead.
newTMDequeIO :: IO (TMDeque a)
newTMDequeIO =
  TMDeque
    <$> newTVarIO False
    <*> (newTVarIO =<< newArray_ (0, initialCapacity - 1))
    <*> newTVarIO initialCapacity
    <*> newTVarIO 0
    <*> newTVarIO 0

growThreshold :: Int
growThreshold = 16

{- | Push an element to the front of the deque.  Silently ignored if the
deque is closed.
-}
pushFrontTMDeque :: TMDeque a -> a -> STM ()
pushFrontTMDeque deq v = do
  growIfNeeded deq
  capa <- readTVar deq.capacity
  UniqIx dest <- stateTVar deq.front \i ->
    let !j = i + 1
     in (UniqIx $ i `rem` capa, j)
  buf <- readTVar deq.ringBuffer
  writeArray buf dest v

growIfNeeded :: TMDeque a -> STM ()
{-# INLINE growIfNeeded #-}
growIfNeeded deq = do
  capa <- readTVar deq.capacity
  size <- sizeTMDeque deq
  when (capa - size - 1 <= growThreshold) do
    ring <- doubleDeq capa deq
    writeTVar deq.ringBuffer ring
    writeTVar deq.capacity (capa * 2)

sizeTMDeque :: TMDeque a -> STM Int
sizeTMDeque deq = do
  front <- readTVar deq.front
  back <- readTVar deq.back
  pure $ front - back

doubleDeq :: Int -> TMDeque a -> STM (TArray Int a)
{-# INLINE doubleDeq #-}
doubleDeq oldSize deq = do
  let !newSize = oldSize * 2
  back <- (`rem` oldSize) <$> readTVar deq.back
  front <- (`rem` oldSize) <$> readTVar deq.front
  arr <- readTVar deq.ringBuffer
  dest <- newArray_ (0, newSize - 1)
  if back <= front
    then -- linear copy on [back, front]
      back & fix \go !i -> when (i < front) do
        e <- readArray arr i
        writeArray dest i e
        go (i + 1)
    else do
      -- first copy [0, front), then copy [back, oldSize)
      0 & fix \go !i -> when (i < front) do
        e <- readArray arr i
        writeArray dest i e
        go (i + 1)
      back & fix \go !i -> when (i < oldSize) do
        e <- readArray arr i
        writeArray dest (i + oldSize) e
        go (i + 1)
  pure dest

{- | Pop an element from the front.  Blocks if the deque is open and empty.
Returns @Nothing@ when the deque is closed and empty (back-of-stream).
-}
popFrontTMDeque :: TMDeque a -> STM (Maybe a)
popFrontTMDeque deq = do
  may <- tryPopFrontTMDeque deq
  maybe retry pure may

{- | Pop an element from the back.  Blocks if the deque is open and empty.
Returns @Nothing@ when the deque is closed and empty (back-of-stream).
-}
popBackTMDeque :: TMDeque a -> STM (Maybe a)
popBackTMDeque deq = do
  may <- tryPopBackTMDeque deq
  maybe retry pure may

{- | Non-blocking pop from the front.

  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopFrontTMDeque :: TMDeque a -> STM (Maybe (Maybe a))
tryPopFrontTMDeque deq = do
  size <- sizeTMDeque deq
  if size == 0
    then do
      closed <- readTVar deq.closed
      if closed
        then pure Nothing
        else pure (Just Nothing)
    else do
      capa <- readTVar deq.capacity
      UniqIx dest <- stateTVar deq.front \i ->
        let !j = i - 1
         in (UniqIx $ j `rem` capa, j)
      buf <- readTVar deq.ringBuffer
      e <- readArray buf dest
      pure (Just (Just e))

{- | Non-blocking pop from the back.

  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopBackTMDeque :: TMDeque a -> STM (Maybe (Maybe a))
tryPopBackTMDeque deq = do
  back <- readTVar deq.back
  front <- readTVar deq.front
  if back == front
    then do
      closed <- readTVar deq.closed
      if closed
        then pure Nothing
        else pure (Just Nothing)
    else do
      capa <- readTVar deq.capacity
      UniqIx dest <- stateTVar deq.back \i ->
        let !j = i + 1
         in (UniqIx $ i `rem` capa, j)
      buf <- readTVar deq.ringBuffer
      e <- readArray buf dest
      pure (Just (Just e))

-- | Close the deque.  After this, all push operations will be ignored, and all pop operations will return @Nothing@ once the deque is empty.
closeTMDeque :: TMDeque a -> STM ()
closeTMDeque deq = writeTVar deq.closed True

-- | Check if the deque is closed.
isClosedTMDeque :: TMDeque a -> STM Bool
isClosedTMDeque deq = readTVar deq.closed

-- | IO variant of 'isClosedTMDeque'.
isClosedTMDequeIO :: TMDeque a -> IO Bool
isClosedTMDequeIO deq = readTVarIO deq.closed

-- | Check if the deque is empty.  Note that an open deque may become non-empty after this returns.
isEmptyTMDeque :: TMDeque a -> STM Bool
isEmptyTMDeque deq = (==) <$> readTVar deq.front <*> readTVar deq.back

-- | IO variant of 'countTMDeque'.
estimateSizeTMDequeIO :: TMDeque a -> IO Int
estimateSizeTMDequeIO deq =
  (-) <$> readTVarIO deq.front <*> readTVarIO deq.back
