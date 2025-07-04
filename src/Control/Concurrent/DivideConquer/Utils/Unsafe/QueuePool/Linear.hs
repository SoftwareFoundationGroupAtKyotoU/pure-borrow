{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Control.Concurrent.DivideConquer.Utils.Unsafe.QueuePool.Linear (
  QueuePool,
  newQueuePool,
  popFront,
  pushFront,
  prependFront,
  pushBack,
  appendBack,
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (TMQueue, closeTMQueue, isClosedTMQueue, newTMQueueIO, readTMQueue, tryPeekTMQueue, unGetTMQueue, writeTMQueue)
import Control.Functor.Linear qualified as Control
import Control.Functor.Linear.Extra (Reverse (..))
import Control.Monad qualified as NL
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Affine
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.STM (STM)
import Control.Monad.STM qualified as STM
import Data.Coerce qualified as NL
import Data.Functor.Linear qualified as Data
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as NL
import Data.Monoid qualified as NL
import Data.Proxy (Proxy (..))
import Data.V.Linear (V)
import Data.V.Linear.Internal qualified as V
import Data.Vector qualified
import GHC.Base qualified as GHC
import GHC.Conc qualified as GHC
import GHC.TypeNats
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NL

data QueuePool a = MkQueuePool
  { mine :: TMQueue a
  , others :: [TMQueue a]
  }

instance Consumable (QueuePool a) where
  {-# NOINLINE consume #-}
  consume = GHC.noinline $ Unsafe.toLinear \qs -> do
    case GHC.runRW# (GHC.unIO (atomically $ closeTMQueue qs.mine)) of
      (# _, () #) -> ()

newQueuePool ::
  forall n α a.
  (KnownNat n) =>
  a %1 ->
  Linearly %1 ->
  BO α (V n (Mut α (QueuePool a)), Lend α (V n (QueuePool a)))
newQueuePool = Unsafe.toLinear \a lin ->
  lin `lseq` unsafeSystemIOToBO do
    let n = fromIntegral @_ @Int $ natVal @n Proxy
    qs <- NL.replicateM n newTMQueueIO
    atomically NL.$ writeTMQueue (NE.head $ NE.fromList qs) a
    let pools = NL.map (UnsafeMut NL.. (`MkQueuePool` qs)) qs
    let vns = V.V NL.$ Data.Vector.fromList pools
    NL.pure (vns, NL.coerce vns)

tryWrite :: (Affable a) => (TMQueue a -> a -> STM ()) -> Mut α (QueuePool a) %1 -> a %1 -> BO α (Mut α (QueuePool a))
tryWrite put = Unsafe.toLinear2 \(UnsafeMut pool) a -> unsafeSystemIOToBO do
  atomically do
    closed <- isClosedTMQueue pool.mine
    () <-
      if closed
        then NL.pure $ pop $ aff a
        else put pool.mine a
    NL.pure $ UnsafeMut pool

pushFront :: (Affable a) => Mut α (QueuePool a) %1 -> a %1 -> BO α (Mut α (QueuePool a))
pushFront = tryWrite unGetTMQueue

{- | Prepends elements of @t a@ to the front of queue,
so that the first element comes the head of the queue after execution.
-}
prependFront :: (Data.Traversable t, Consumable (t ())) => Mut α (QueuePool a) %1 -> t a %1 -> BO α (Mut α (QueuePool a))
prependFront = Unsafe.toLinear2 \(UnsafeMut pool) as -> Control.do
  unsafeAtomically Control.do
    closed <- unsafeSTMToBO $ isClosedTMQueue pool.mine
    if closed
      then Control.pure $ UnsafeMut pool
      else Control.do
        Control.void
          $ Data.traverse
            (unsafeSTMToBO . Unsafe.toLinear (unGetTMQueue pool.mine))
          $ Reverse as
        Control.pure $ UnsafeMut pool

{- | Appends elements of @t a@ to the back of queue,
so that the last elements comes the last of the queue after execution.
-}
appendBack :: (Data.Traversable t, Consumable (t ())) => Mut α (QueuePool a) %1 -> t a %1 -> BO α (Maybe (QueuePool a))
appendBack = Unsafe.toLinear2 \(UnsafeMut pool) as -> Control.do
  unsafeAtomically Control.do
    closed <- unsafeSTMToBO $ isClosedTMQueue pool.mine
    if closed
      then Control.pure Nothing
      else Control.do
        Control.void
          $ Data.traverse
            (unsafeSTMToBO . Unsafe.toLinear (writeTMQueue pool.mine))
            as
        Control.pure $ Just pool

unsafeSTMToBO :: STM a %1 -> BO α a
unsafeSTMToBO (GHC.STM f) = BO (Unsafe.coerce f)

unsafeAtomically :: BO α a %1 -> BO α a
unsafeAtomically = Unsafe.toLinear \(BO f) -> unsafeSystemIOToBO (atomically (GHC.STM (Unsafe.coerce f)))

pushBack :: (Affable a) => Mut α (QueuePool a) %1 -> a %1 -> BO α (Mut α (QueuePool a))
pushBack = tryWrite writeTMQueue

popFront :: Mut α (QueuePool a) %1 -> BO α (Maybe (a, Mut α (QueuePool a)))
popFront = Unsafe.toLinear \(UnsafeMut pool) -> unsafeSystemIOToBO do
  atomically do
    m <- tryPeekTMQueue pool.mine
    let closed = NL.isNothing m
    case NL.join m of
      Just a -> do
        NL.void $ readTMQueue pool.mine
        NL.pure $ Just (a, UnsafeMut pool)
      Nothing -> do
        qs <- NL.catMaybes NL.<$> NL.mapM (\q -> NL.fmap (q,) NL.<$> tryPeekTMQueue q) pool.others
        if null qs && closed
          then NL.pure Nothing
          else case NL.getFirst $ NL.foldMap (\(a, b) -> NL.First NL.$ (a,) NL.<$> b) qs of
            Nothing -> STM.retry
            Just (q, a) -> do
              NL.void $ readTMQueue q
              NL.pure $ Just (a, UnsafeMut pool {others = NL.map NL.fst qs})
