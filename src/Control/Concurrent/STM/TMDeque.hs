{-# LANGUAGE NoLinearTypes #-}

{- | A closable, concurrent double-ended queue backed by STM, with amortized
O(1) operations. The underlying implementation uses a two-stack design with
separate 'TVar's for the front and rear, reducing STM contention: in the
common case, @pushFront@ and @popBack@ touch disjoint variables and do not
conflict.

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
  isEmptyTMDeque,
) where

import Control.Concurrent.STM (STM, TVar, newTVar, newTVarIO, readTVar, retry, writeTVar)

{- | Reverse a non-empty list and split into head and tail.
Precondition: the input list is non-empty.
-}
unconsReverse :: [a] -> (a, [a])
unconsReverse xs = case reverse xs of
  y : ys -> (y, ys)
  [] -> error "TMDeque.unconsReverse: impossible – called on empty list"

------------------------------------------------------------------------
-- STM two-stack queue
------------------------------------------------------------------------

-- | A closable, STM-backed double-ended queue with amortized O(1) operations.
data TMDeque a
  = TMDeque
      {-# UNPACK #-} !(TVar Bool) -- closed flag (monotonic: False → True)
      {-# UNPACK #-} !(TVar [a]) -- front (push end)
      {-# UNPACK #-} !(TVar [a]) -- rear (pop end)

-- | Create a new empty 'TMDeque'.
newTMDeque :: STM (TMDeque a)
newTMDeque = TMDeque <$> newTVar False <*> newTVar [] <*> newTVar []

-- | IO variant of 'newTMDeque'.
newTMDequeIO :: IO (TMDeque a)
newTMDequeIO = TMDeque <$> newTVarIO False <*> newTVarIO [] <*> newTVarIO []

{- | Push an element to the front of the deque.  Silently ignored if the
deque is closed.
-}
pushFrontTMDeque :: TMDeque a -> a -> STM ()
pushFrontTMDeque (TMDeque closedVar frontVar _rearVar) x = do
  closed <- readTVar closedVar
  if closed
    then pure ()
    else do
      f <- readTVar frontVar
      writeTVar frontVar (x : f)

{- | Pop an element from the front.  Blocks if the deque is open and empty.
Returns @Nothing@ when the deque is closed and empty (end-of-stream).
-}
popFrontTMDeque :: TMDeque a -> STM (Maybe a)
popFrontTMDeque (TMDeque closedVar frontVar rearVar) = do
  f <- readTVar frontVar
  case f of
    x : f' -> do
      writeTVar frontVar f'
      pure (Just x)
    [] -> do
      r <- readTVar rearVar
      case r of
        _ : _ -> do
          let (x, f') = unconsReverse r
          writeTVar rearVar []
          writeTVar frontVar f'
          pure (Just x)
        [] -> do
          closed <- readTVar closedVar
          if closed
            then pure Nothing
            else retry

{- | Non-blocking pop from the front.

  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopFrontTMDeque :: TMDeque a -> STM (Maybe (Maybe a))
tryPopFrontTMDeque (TMDeque closedVar frontVar rearVar) = do
  f <- readTVar frontVar
  case f of
    x : f' -> do
      writeTVar frontVar f'
      pure (Just (Just x))
    [] -> do
      r <- readTVar rearVar
      case r of
        _ : _ -> do
          let (x, f') = unconsReverse r
          writeTVar rearVar []
          writeTVar frontVar f'
          pure (Just (Just x))
        [] -> do
          closed <- readTVar closedVar
          if closed
            then pure Nothing
            else pure (Just Nothing)

{- | Pop an element from the back.  Blocks if the deque is open and empty.
Returns @Nothing@ when the deque is closed and empty (end-of-stream).
-}
popBackTMDeque :: TMDeque a -> STM (Maybe a)
popBackTMDeque (TMDeque closedVar frontVar rearVar) = do
  r <- readTVar rearVar
  case r of
    x : r' -> do
      writeTVar rearVar r'
      pure (Just x)
    [] -> do
      f <- readTVar frontVar
      case f of
        _ : _ -> do
          let (x, r') = unconsReverse f
          writeTVar frontVar []
          writeTVar rearVar r'
          pure (Just x)
        [] -> do
          closed <- readTVar closedVar
          if closed
            then pure Nothing
            else retry

{- | Non-blocking pop from the back.

  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopBackTMDeque :: TMDeque a -> STM (Maybe (Maybe a))
tryPopBackTMDeque (TMDeque closedVar frontVar rearVar) = do
  r <- readTVar rearVar
  case r of
    x : r' -> do
      writeTVar rearVar r'
      pure (Just (Just x))
    [] -> do
      f <- readTVar frontVar
      case f of
        _ : _ -> do
          let (x, r') = unconsReverse f
          writeTVar frontVar []
          writeTVar rearVar r'
          pure (Just (Just x))
        [] -> do
          closed <- readTVar closedVar
          if closed
            then pure Nothing
            else pure (Just Nothing)

{- | Close the deque.  After closing, writes are silently ignored and reads
will drain remaining elements before signalling end-of-stream.  Closing
is idempotent.
-}
closeTMDeque :: TMDeque a -> STM ()
closeTMDeque (TMDeque closedVar _ _) = writeTVar closedVar True

-- | Check whether the deque has been closed.
isClosedTMDeque :: TMDeque a -> STM Bool
isClosedTMDeque (TMDeque closedVar _ _) = readTVar closedVar

-- | Check whether the deque is currently empty.
isEmptyTMDeque :: TMDeque a -> STM Bool
isEmptyTMDeque (TMDeque _ frontVar rearVar) = do
  f <- readTVar frontVar
  case f of
    _ : _ -> pure False
    [] -> do
      r <- readTVar rearVar
      pure (null r)
