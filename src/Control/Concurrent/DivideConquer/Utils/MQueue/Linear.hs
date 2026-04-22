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
  newRawMQueue,
  unsafeClone,
  unsafeCloneN,
  pushFrontMQueue,
  popBackMQueue,
  closeMQueue,
  popFrontMQueue,
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMDeque (TMDeque)
import Control.Concurrent.STM.TMDeque qualified as TMQ
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal (LinearOnly (..), LinearOnlyWitness (..))
import Data.Coerce qualified as NonLinear
import Data.Functor.Linear qualified as Data
import Data.V.Linear (V)
import Data.V.Linear qualified as V
import GHC.Exts qualified as GHC
import GHC.IO qualified as GHC
import GHC.TypeLits (ErrorMessage (..), KnownNat)
import Prelude.Linear
import Prelude.Linear.Unsatisfiable
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

-- | A closable queue.
newtype MQueue a = MkMQ (TMDeque a)

instance LinearOnly (MQueue a) where
  linearOnly = UnsafeLinearOnly

newMQueue ::
  forall α a.
  BO α (Mut α (MQueue a), Lend α (MQueue a))
newMQueue = Control.do
  q <- unsafeSystemIOToBO $ MkMQ NonLinear.<$> TMQ.newTMDequeIO
  asksLinearly $ borrow q

newRawMQueue ::
  forall α a.
  BO α (MQueue a)
newRawMQueue = asksLinearlyM \lin ->
  lin `lseq` unsafeSystemIOToBO (MkMQ NonLinear.<$> TMQ.newTMDequeIO)

instance
  (Unsatisfiable (ShowType (MQueue a) :<>: Text " cannot be copied!")) =>
  Copyable (MQueue a)
  where
  copy = unsatisfiable

-- | NOTE: unconditional use of this function MAY BREAK PURITY!
unsafeClone :: Mut α (MQueue a) %1 -> (Mut α (MQueue a), Mut α (MQueue a))
unsafeClone = Unsafe.toLinear \mut -> (mut, mut)

-- | NOTE: unconditional use of this function MAY BREAK PURITY!
unsafeCloneN :: forall n α a. (KnownNat n) => Mut α (MQueue a) %1 -> V n (Mut α (MQueue a))
unsafeCloneN = Unsafe.toLinear \q ->
  V.fromReplicator (Data.pure q)

pushFrontMQueue ::
  Mut α (MQueue a) %1 ->
  a %1 ->
  BO α (Mut α (MQueue a))
pushFrontMQueue = Unsafe.toLinear2 \q a -> Control.do
  unsafeSystemIOToBO $ q NonLinear.<$ atomically (TMQ.pushFrontTMDeque (NonLinear.coerce q) a)

popBackMQueue :: Mut α (MQueue a) %1 -> BO α (Maybe (a, Mut α (MQueue a)))
popBackMQueue = Unsafe.toLinear \mutq@(UnsafeAlias (MkMQ q)) ->
  unsafeSystemIOToBO (atomically $ TMQ.popBackTMDeque q) Control.<&> \case
    Nothing -> mutq `lseq` Nothing
    Just a -> Just (a, mutq)

popFrontMQueue :: Mut α (MQueue a) %1 -> BO α (Maybe (a, Mut α (MQueue a)))
popFrontMQueue = Unsafe.toLinear \mutq@(UnsafeAlias (MkMQ q)) ->
  unsafeSystemIOToBO (atomically $ TMQ.popFrontTMDeque q) Control.<&> \case
    Nothing -> mutq `lseq` Nothing
    Just a -> Just (a, mutq)

closeMQueue :: Mut α (MQueue a) %1 -> BO α ()
closeMQueue = Unsafe.toLinear \(UnsafeAlias (MkMQ q)) ->
  unsafeSystemIOToBO $ atomically $ TMQ.closeTMDeque q

instance Consumable (MQueue a) where
  {-# NOINLINE consume #-}
  consume = GHC.noinline $ Unsafe.toLinear \(MkMQ q) -> do
    case GHC.runRW# (GHC.unIO (atomically $ TMQ.closeTMDeque q)) of
      (# _, () #) -> ()
