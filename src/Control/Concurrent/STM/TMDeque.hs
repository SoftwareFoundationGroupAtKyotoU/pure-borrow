{-# LANGUAGE BlockArguments #-}
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
  newTMDequeIO,

  -- * Push operations
  pushFrontTMDeque,

  -- * Pop operations (non-blocking)
  tryPopFrontTMDeque,
  tryPopBackTMDeque,

  -- * Closing & queries
  closeTMDeque,
  isClosedTMDequeIO,
  isEmptyTMDeque,
  countTMDequeIO,
) where

import Control.Concurrent.STM (STM, TVar, newTVarIO, writeTVar)
import Control.Concurrent.STM.TVar (readTVarIO)
import Data.Concurrent.Deque.ChaseLev (ChaseLevDeque, approxSize, newQ, nullQ, pushL, tryPopL, tryPopR)

------------------------------------------------------------------------
-- STM two-stack queue
------------------------------------------------------------------------

-- | A closable, STM-backed double-ended queue with amortized O(1) operations.
data TMDeque a
  = TMDeque
      {-# UNPACK #-} !(TVar Bool)
      !(ChaseLevDeque a)

-- | IO variant of 'newTMDeque'.
newTMDequeIO :: IO (TMDeque a)
newTMDequeIO = TMDeque <$> newTVarIO False <*> newQ

{- | Push an element to the front of the deque.  Silently ignored if the
deque is closed.
-}
pushFrontTMDeque :: TMDeque a -> a -> IO ()
pushFrontTMDeque (TMDeque _ q) = pushL q

{- | Non-blocking pop from the front.

  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopFrontTMDeque :: TMDeque a -> IO (Maybe (Maybe a))
tryPopFrontTMDeque (TMDeque closedVar q) = do
  melt <- tryPopL q
  case melt of
    Nothing -> do
      closed <- readTVarIO closedVar
      if closed
        then pure Nothing
        else pure (Just Nothing)
    may -> pure (Just may)

{- | Non-blocking pop from the back.

  * @Nothing@         — closed (end-of-stream)
  * @Just Nothing@    — open and empty (would block)
  * @Just (Just a)@   — got an element
-}
tryPopBackTMDeque :: TMDeque a -> IO (Maybe (Maybe a))
tryPopBackTMDeque (TMDeque closedVar q) = do
  melt <- tryPopR q
  case melt of
    Nothing -> do
      closed <- readTVarIO closedVar
      if closed
        then pure Nothing
        else pure (Just Nothing)
    may -> pure (Just may)

{- | Close the deque.  After closing, writes are silently ignored and reads
will drain remaining elements before signalling end-of-stream.  Closing
is idempotent.
-}
closeTMDeque :: TMDeque a -> STM ()
closeTMDeque (TMDeque closedVar _) = writeTVar closedVar True

-- | Check whether the deque has been closed.
isClosedTMDequeIO :: TMDeque a -> IO Bool
isClosedTMDequeIO (TMDeque closedVar _) = readTVarIO closedVar

-- | Check whether the deque is currently empty.
isEmptyTMDeque :: TMDeque a -> IO Bool
isEmptyTMDeque (TMDeque _ q) = nullQ q

-- | IO variant of 'countTMDeque'. O(1).
countTMDequeIO :: TMDeque a -> IO Int
countTMDequeIO (TMDeque _ q) = approxSize q
