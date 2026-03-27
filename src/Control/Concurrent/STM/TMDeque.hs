{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoLinearTypes #-}

{- | A closable, concurrent double-ended queue backed by STM, with worst-case
O(1) push\/pop operations.  The underlying implementation uses a circular
ring buffer of 'TVar' slots, so individual push and pop operations never
trigger an O(n) list reversal.

In the common work-stealing pattern ('pushFrontTMDeque' on the owner thread,
'popBackTMDeque' on a stealer thread), the two sides write to disjoint
'TVar's, minimising STM contention.

Closing semantics follow @stm-chans@ conventions:

  * __Closed + empty__ → read returns @Nothing@ (end-of-stream)
  * __Closed + non-empty__ → read returns @Just a@ (drain remaining)
  * __Open + empty__ → read blocks (@retry@)
  * __Open + non-empty__ → read returns @Just a@
  * __Write to closed__ → silently ignored
-}
module Control.Concurrent.STM.TMDeque (
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
  countTMDeque,
  countTMDequeIO,
) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Monad (unless)
import Data.Array (Array, listArray, (!))

------------------------------------------------------------------------
-- Ring-buffer STM deque
------------------------------------------------------------------------

-- | Default initial capacity (must be >= 1).
initialCapacity :: Int
initialCapacity = 64

{- | A closable, STM-backed double-ended queue with worst-case O(1) operations
(amortized O(1) accounting for rare resizes).

Internally this is a circular ring buffer of @TVar (Maybe a)@ slots.
The @head@ index points to the next free slot at the front (owner side),
and @tail@ points to the oldest element at the back (stealer side).
Valid elements occupy indices @[tail .. head)@ modulo the capacity.
-}
data TMDeque a
  = TMDeque
      {-# UNPACK #-} !(TVar Bool) -- closed flag (monotonic: False → True)
      {-# UNPACK #-} !(TVar Int) -- head (front index; next free slot)
      {-# UNPACK #-} !(TVar Int) -- tail (back index; oldest element)
      {-# UNPACK #-} !(TVar (Array Int (TVar (Maybe a)))) -- circular buffer
      {-# UNPACK #-} !(TVar Int) -- capacity

-- | Create a new empty 'TMDeque'.
newTMDeque :: STM (TMDeque a)
newTMDeque = do
  closedVar <- newTVar False
  headVar <- newTVar 0
  tailVar <- newTVar 0
  slots <- mapM (\_ -> newTVar Nothing) [0 .. initialCapacity - 1]
  let buf = listArray (0, initialCapacity - 1) slots
  bufVar <- newTVar buf
  capVar <- newTVar initialCapacity
  pure (TMDeque closedVar headVar tailVar bufVar capVar)

-- | IO variant of 'newTMDeque'.
newTMDequeIO :: IO (TMDeque a)
newTMDequeIO = do
  closedVar <- newTVarIO False
  headVar <- newTVarIO 0
  tailVar <- newTVarIO 0
  slots <- mapM (\_ -> newTVarIO Nothing) [0 .. initialCapacity - 1]
  let buf = listArray (0, initialCapacity - 1) slots
  bufVar <- newTVarIO buf
  capVar <- newTVarIO initialCapacity
  pure (TMDeque closedVar headVar tailVar bufVar capVar)

------------------------------------------------------------------------
-- Internal: resize
------------------------------------------------------------------------

{- | Double the buffer capacity, copying existing elements to the new buffer.
After resize, tail = 0 and head = size.
-}
resize :: TVar Int -> TVar Int -> TVar (Array Int (TVar (Maybe a))) -> TVar Int -> Int -> Int -> Int -> STM ()
resize headVar tailVar bufVar capVar h t cap = do
  let size = h - t
      newCap = cap * 2
  oldBuf <- readTVar bufVar
  -- Allocate new slots
  newSlots <- mapM (\_ -> newTVar Nothing) [0 .. newCap - 1]
  let newBuf = listArray (0, newCap - 1) newSlots
  -- Copy elements from old buffer to new buffer at positions [0..size-1]
  mapM_
    ( \i -> do
        val <- readTVar (oldBuf ! ((t + i) `mod` cap))
        writeTVar (newBuf ! i) val
    )
    [0 .. size - 1]
  writeTVar bufVar newBuf
  writeTVar capVar newCap
  writeTVar tailVar 0
  writeTVar headVar size

------------------------------------------------------------------------
-- Push
------------------------------------------------------------------------

{- | Push an element to the front of the deque.  Silently ignored if the
deque is closed.  O(1) worst-case (amortized O(1) accounting for rare
buffer resizes).
-}
pushFrontTMDeque :: TMDeque a -> a -> STM ()
pushFrontTMDeque (TMDeque closedVar headVar tailVar bufVar capVar) x = do
  closed <- readTVar closedVar
  unless closed do
    h <- readTVar headVar
    t <- readTVar tailVar
    cap <- readTVar capVar
    let size = h - t
    -- Resize if full (we keep one slot unused to distinguish full from empty)
    if size >= cap - 1
      then do
        resize headVar tailVar bufVar capVar h t cap
        -- After resize: tail=0, head=size, cap=cap*2
        h' <- readTVar headVar
        buf' <- readTVar bufVar
        writeTVar (buf' ! h') (Just x)
        writeTVar headVar (h' + 1)
      else do
        buf <- readTVar bufVar
        writeTVar (buf ! (h `mod` cap)) (Just x)
        writeTVar headVar (h + 1)

------------------------------------------------------------------------
-- Pop (blocking)
------------------------------------------------------------------------

{- | Pop an element from the front.  Blocks if the deque is open and empty.
Returns @Nothing@ when the deque is closed and empty (end-of-stream).
O(1) worst-case.
-}
popFrontTMDeque :: TMDeque a -> STM (Maybe a)
popFrontTMDeque (TMDeque closedVar headVar tailVar bufVar capVar) = do
  h <- readTVar headVar
  t <- readTVar tailVar
  if h == t
    then do
      -- Empty
      closed <- readTVar closedVar
      if closed then pure Nothing else retry
    else do
      cap <- readTVar capVar
      buf <- readTVar bufVar
      let h' = h - 1
          slot = buf ! (h' `mod` cap)
      val <- readTVar slot
      writeTVar slot Nothing
      writeTVar headVar h'
      pure val

{- | Pop an element from the back.  Blocks if the deque is open and empty.
Returns @Nothing@ when the deque is closed and empty (end-of-stream).
O(1) worst-case.
-}
popBackTMDeque :: TMDeque a -> STM (Maybe a)
popBackTMDeque (TMDeque closedVar headVar tailVar bufVar capVar) = do
  h <- readTVar headVar
  t <- readTVar tailVar
  if h == t
    then do
      closed <- readTVar closedVar
      if closed then pure Nothing else retry
    else do
      cap <- readTVar capVar
      buf <- readTVar bufVar
      let slot = buf ! (t `mod` cap)
      val <- readTVar slot
      writeTVar slot Nothing
      writeTVar tailVar (t + 1)
      pure val

------------------------------------------------------------------------
-- Pop (non-blocking)
------------------------------------------------------------------------

{- | Non-blocking pop from the front.

  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopFrontTMDeque :: TMDeque a -> STM (Maybe (Maybe a))
tryPopFrontTMDeque (TMDeque closedVar headVar tailVar bufVar capVar) = do
  h <- readTVar headVar
  t <- readTVar tailVar
  if h == t
    then do
      closed <- readTVar closedVar
      if closed then pure Nothing else pure (Just Nothing)
    else do
      cap <- readTVar capVar
      buf <- readTVar bufVar
      let h' = h - 1
          slot = buf ! (h' `mod` cap)
      val <- readTVar slot
      writeTVar slot Nothing
      writeTVar headVar h'
      pure (Just val)

{- | Non-blocking pop from the back.

  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopBackTMDeque :: TMDeque a -> STM (Maybe (Maybe a))
tryPopBackTMDeque (TMDeque closedVar headVar tailVar bufVar capVar) = do
  h <- readTVar headVar
  t <- readTVar tailVar
  if h == t
    then do
      closed <- readTVar closedVar
      if closed then pure Nothing else pure (Just Nothing)
    else do
      cap <- readTVar capVar
      buf <- readTVar bufVar
      let slot = buf ! (t `mod` cap)
      val <- readTVar slot
      writeTVar slot Nothing
      writeTVar tailVar (t + 1)
      pure (Just val)

------------------------------------------------------------------------
-- Closing & queries
------------------------------------------------------------------------

{- | Close the deque.  After closing, writes are silently ignored and reads
will drain remaining elements before signalling end-of-stream.  Closing
is idempotent.
-}
closeTMDeque :: TMDeque a -> STM ()
closeTMDeque (TMDeque closedVar _ _ _ _) = writeTVar closedVar True

-- | Check whether the deque has been closed.
isClosedTMDeque :: TMDeque a -> STM Bool
isClosedTMDeque (TMDeque closedVar _ _ _ _) = readTVar closedVar

-- | Check whether the deque has been closed (IO variant).
isClosedTMDequeIO :: TMDeque a -> IO Bool
isClosedTMDequeIO (TMDeque closedVar _ _ _ _) = readTVarIO closedVar

-- | Check whether the deque is currently empty.
isEmptyTMDeque :: TMDeque a -> STM Bool
isEmptyTMDeque (TMDeque _ headVar tailVar _ _) = do
  h <- readTVar headVar
  t <- readTVar tailVar
  pure (h == t)

-- | Return the number of elements currently in the deque. O(1).
countTMDeque :: TMDeque a -> STM Int
countTMDeque (TMDeque _ headVar tailVar _ _) = do
  h <- readTVar headVar
  t <- readTVar tailVar
  pure (h - t)

-- | IO variant of 'countTMDeque'. O(1). Uses 'atomically' for an atomic snapshot.
countTMDequeIO :: TMDeque a -> IO Int
countTMDequeIO q = atomically (countTMDeque q)
