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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{- | NOTE: This is only for internal use, and is not meant to be used by end users.
This is because multiple existence of @'Mut' α 'BMQueue'@ breaks purity!
-}
module Control.Concurrent.DivideConquer.Utils.BMQueue.Linear (
  BMQueue,
  newBMQueue,
  unsafeClone,
  unsafeCloneN,
  writeBMQueue,
  writeBMQueueMany,
  readBMQueue,
  closeBMQueue,
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Concurrent.STM.TBMQueue qualified as TBMQ
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.STM (STM)
import Data.Coerce qualified as NonLinear
import Data.Functor.Linear qualified as Data
import Data.Unrestricted.Linear (UrT (..), runUrT)
import Data.V.Linear (V)
import Data.V.Linear qualified as V
import GHC.Conc qualified as GHC
import GHC.Exts qualified as GHC
import GHC.IO qualified as GHC
import GHC.TypeLits (ErrorMessage (..), KnownNat)
import Prelude.Linear
import Prelude.Linear.Unsatisfiable
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

-- | A bounded closable queue
newtype BMQueue a = MkBMQ (TBMQueue a)

newBMQueue ::
  forall α a.
  Int ->
  Linearly %1 ->
  BO α (Mut α (BMQueue a), Lend α (BMQueue a))
newBMQueue n lin = Control.do
  q <- unsafeSystemIOToBO $ MkBMQ NonLinear.<$> TBMQ.newTBMQueueIO n
  Control.pure $ borrow q lin

instance
  (Unsatisfiable (ShowType (BMQueue a) :<>: Text " cannot be dereferenced!")) =>
  Derefable (BMQueue a)
  where
  unsafeDeref = unsatisfiable

-- | NOTE: unconditional use of this function MAY BREAK PURITY!
unsafeClone :: Mut α (BMQueue a) %1 -> (Mut α (BMQueue a), Mut α (BMQueue a))
unsafeClone = Unsafe.toLinear \mut -> (mut, mut)

-- | NOTE: unconditional use of this function MAY BREAK PURITY!
unsafeCloneN :: forall n α a. (KnownNat n) => Mut α (BMQueue a) %1 -> V n (Mut α (BMQueue a))
unsafeCloneN = Unsafe.toLinear \q ->
  V.fromReplicator (Data.pure q)

writeBMQueue ::
  Mut α (BMQueue a) %1 ->
  a %1 ->
  BO α (Mut α (BMQueue a))
writeBMQueue = Unsafe.toLinear2 \q a -> Control.do
  unsafeSystemIOToBO $ q NonLinear.<$ atomically (TBMQ.writeTBMQueue (NonLinear.coerce q) a)

writeBMQueueMany ::
  (Data.Traversable t, Consumable (t ())) =>
  Mut α (BMQueue a) %1 ->
  t a %1 ->
  BO α (Mut α (BMQueue a))
writeBMQueueMany = Unsafe.toLinear2 \q as ->
  Control.fmap (`lseq` q)
    $ unsafeAtomically
    $ Data.traverse (unsafeSTMToBO . Unsafe.toLinear (TBMQ.writeTBMQueue (NonLinear.coerce q))) as

readBMQueue :: Mut α (BMQueue a) %1 -> BO α (Maybe (a, Mut α (BMQueue a)))
readBMQueue = Unsafe.toLinear \mutq@(UnsafeMut (MkBMQ q)) ->
  unsafeSystemIOToBO (atomically $ TBMQ.readTBMQueue q) Control.<&> \case
    Nothing -> mutq `lseq` Nothing
    Just a -> Just (a, mutq)

unsafeSTMToBO :: STM a %1 -> BO α a
unsafeSTMToBO (GHC.STM f) = BO (Unsafe.coerce f)

unsafeAtomically :: BO α a %1 -> BO α a
unsafeAtomically = Unsafe.toLinear \(BO f) -> unsafeSystemIOToBO (atomically (GHC.STM (Unsafe.coerce f)))

closeBMQueue :: Mut α (BMQueue a) %1 -> BO α ()
closeBMQueue = Unsafe.toLinear \(UnsafeMut (MkBMQ q)) ->
  unsafeSystemIOToBO $ atomically $ TBMQ.closeTBMQueue q

instance Consumable (BMQueue a) where
  {-# NOINLINE consume #-}
  consume = GHC.noinline $ Unsafe.toLinear \(MkBMQ q) -> do
    case GHC.runRW# (GHC.unIO (atomically $ TBMQ.closeTBMQueue q)) of
      (# _, () #) -> ()
