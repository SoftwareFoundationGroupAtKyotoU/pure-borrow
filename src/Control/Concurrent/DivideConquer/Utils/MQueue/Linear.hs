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
This is because multiple existence of @'Mut' α 'MQueue'@ breaks purity!
-}
module Control.Concurrent.DivideConquer.Utils.MQueue.Linear (
  MQueue,
  newMQueue,
  unsafeClone,
  unsafeCloneN,
  writeMQueue,
  writeMQueueMany,
  readMQueue,
  closeMQueue,
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (TMQueue)
import Control.Concurrent.STM.TMQueue qualified as TMQ
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.STM (STM)
import Data.Coerce qualified as NonLinear
import Data.Functor.Linear qualified as Data
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

-- | A closable queue
newtype MQueue a = MkMQ (TMQueue a)

newMQueue ::
  forall α a.
  Linearly %1 ->
  BO α (Mut α (MQueue a), Lend α (MQueue a))
newMQueue lin = Control.do
  q <- unsafeSystemIOToBO $ MkMQ NonLinear.<$> TMQ.newTMQueueIO
  Control.pure $ borrow q lin

instance
  (Unsatisfiable (ShowType (MQueue a) :<>: Text " cannot be copyed!")) =>
  Copyable (MQueue a)
  where
  unsafeCopy = unsatisfiable

-- | NOTE: unconditional use of this function MAY BREAK PURITY!
unsafeClone :: Mut α (MQueue a) %1 -> (Mut α (MQueue a), Mut α (MQueue a))
unsafeClone = Unsafe.toLinear \mut -> (mut, mut)

-- | NOTE: unconditional use of this function MAY BREAK PURITY!
unsafeCloneN :: forall n α a. (KnownNat n) => Mut α (MQueue a) %1 -> V n (Mut α (MQueue a))
unsafeCloneN = Unsafe.toLinear \q ->
  V.fromReplicator (Data.pure q)

writeMQueue ::
  Mut α (MQueue a) %1 ->
  a %1 ->
  BO α (Mut α (MQueue a))
writeMQueue = Unsafe.toLinear2 \q a -> Control.do
  unsafeSystemIOToBO $ q NonLinear.<$ atomically (TMQ.writeTMQueue (NonLinear.coerce q) a)

writeMQueueMany ::
  (Data.Traversable t, Consumable (t ())) =>
  Mut α (MQueue a) %1 ->
  t a %1 ->
  BO α (Mut α (MQueue a))
writeMQueueMany = Unsafe.toLinear2 \q as ->
  Control.fmap (`lseq` q) $
    unsafeAtomically $
      Data.traverse (unsafeSTMToBO . Unsafe.toLinear (TMQ.writeTMQueue (NonLinear.coerce q))) as

readMQueue :: Mut α (MQueue a) %1 -> BO α (Maybe (a, Mut α (MQueue a)))
readMQueue = Unsafe.toLinear \mutq@(UnsafeMut (MkMQ q)) ->
  unsafeSystemIOToBO (atomically $ TMQ.readTMQueue q) Control.<&> \case
    Nothing -> mutq `lseq` Nothing
    Just a -> Just (a, mutq)

unsafeSTMToBO :: STM a %1 -> BO α a
unsafeSTMToBO (GHC.STM f) = BO (Unsafe.coerce f)

unsafeAtomically :: BO α a %1 -> BO α a
unsafeAtomically = Unsafe.toLinear \(BO f) -> unsafeSystemIOToBO (atomically (GHC.STM (Unsafe.coerce f)))

closeMQueue :: Mut α (MQueue a) %1 -> BO α ()
closeMQueue = Unsafe.toLinear \(UnsafeMut (MkMQ q)) ->
  unsafeSystemIOToBO $ atomically $ TMQ.closeTMQueue q

instance Consumable (MQueue a) where
  {-# NOINLINE consume #-}
  consume = GHC.noinline $ Unsafe.toLinear \(MkMQ q) -> do
    case GHC.runRW# (GHC.unIO (atomically $ TMQ.closeTMQueue q)) of
      (# _, () #) -> ()
