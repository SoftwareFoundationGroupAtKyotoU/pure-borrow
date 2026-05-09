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
{-# LANGUAGE OverloadedLabels #-}
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

module Control.Concurrent.DivideConquer.Utils.QueuePool (
  QueuePool,
  newQueuePool,
  pushWork,
  pushWorks,
  popWork,
  pushWorkMaster,
) where

import Control.Applicative qualified as P
import Control.Concurrent (yield)
import Control.Concurrent.Queue.ChaseLev (ChaseLevDeq, close, estimateSize, newDeq, pushFront, tryPopBack, tryPopFront)
import Control.Monad qualified as NonLinear
import Control.Monad qualified as P
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..), unsafeSystemIOToBO)
import Data.Coerce (coerce)
import Data.Foldable qualified as P
import Data.Function (fix)
import Data.List qualified as L
import Data.Ord (Down (..))
import Data.Ord qualified as P
import Data.V.Linear (V, theLength)
import Data.V.Linear.Internal (V (..))
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as AI
import Data.Vector.Hybrid.Mutable qualified as HMV
import Data.Vector.Mutable (RealWorld)
import GHC.Exts qualified as GHC
import GHC.IO qualified as GHC
import GHC.TypeLits (KnownNat)
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as P

data QueuePool a = QueuePool
  { mine :: !(ChaseLevDeq a)
  , others :: !(V.MVector RealWorld (ChaseLevDeq a))
  , num :: !Int
  }

newtype MasterQueuePool a = MasterQueuePool [ChaseLevDeq a]

instance Consumable (MasterQueuePool a) where
  consume = consume . map consumeTMDQ . Unsafe.coerce @_ @[ChaseLevDeq a]

consumeTMDQ :: ChaseLevDeq a %1 -> ()
{-# NOINLINE consumeTMDQ #-}
consumeTMDQ = GHC.noinline $ Unsafe.toLinear \q -> GHC.unsafePerformIO do
  !() <- close q
  P.pure ()

newQueuePool ::
  forall n a α.
  (KnownNat n) =>
  BO α (V n (Mut α (QueuePool a)), MasterQueuePool a)
newQueuePool = unsafeSystemIOToBO do
  let n = theLength @n

  qs <- NonLinear.replicateM n newDeq
  pools <-
    P.mapM
      ( \(num, ini, mine, tl) -> do
          others <- V.unsafeThaw $ V.fromList $ tl <> ini
          P.pure P.$ QueuePool {others, ..}
      )
      P.$ L.zip4
        [0 ..]
        (L.inits qs)
        qs
        (P.drop 1 $ L.tails qs)
  let master = MasterQueuePool $ P.map (mine P.. coerce) pools
  P.pure (V $ V.fromList $ map UnsafeAlias pools, master)

pushWorkMaster :: Mut α (MasterQueuePool a) %1 -> a %1 -> BO α (Mut α (MasterQueuePool a))
pushWorkMaster = Unsafe.toLinear2 \(UnsafeAlias (MasterQueuePool pools)) work ->
  case pools of
    (q : qs) -> unsafeSystemIOToBO do
      pushFront q work
      P.pure $ UnsafeAlias $ MasterQueuePool (q : qs)
    [] -> error "impossible: the length of pools is determined by the type-level nat n and cannot be zero"

pushWork :: Mut α (QueuePool a) %1 -> a %1 -> BO α (Mut α (QueuePool a))
pushWork = Unsafe.toLinear2 \(UnsafeAlias QueuePool {..}) work ->
  unsafeSystemIOToBO do
    pushFront mine work
    P.pure $ UnsafeAlias QueuePool {..}

newtype Backwards f a = Backwards {runBackwards :: f a}
  deriving newtype (P.Functor)

instance (P.Applicative f) => P.Applicative (Backwards f) where
  pure = Backwards P.. P.pure
  Backwards f <*> Backwards x = Backwards (x P.<**> f)

-- | Pushes works, the first element is on top.
pushWorks :: Mut α (QueuePool a) %1 -> [a] %1 -> BO α (Mut α (QueuePool a))
pushWorks = Unsafe.toLinear2 \(UnsafeAlias QueuePool {..}) work ->
  unsafeSystemIOToBO do
    runBackwards P.$ P.traverse_ (Backwards P.. pushFront mine) work
    P.pure $ UnsafeAlias QueuePool {..}

popWork :: Mut α (QueuePool a) %1 -> BO α (Maybe (a, Mut α (QueuePool a)))
popWork = Unsafe.toLinear \qs@(UnsafeAlias QueuePool {..}) ->
  unsafeSystemIOToBO do
    tryPopFront mine P.>>= \case
      Nothing -> P.pure Nothing
      Just (Just x) -> P.pure $ Just (x, qs)
      Just Nothing -> fix \self -> do
        !ranks <-
          V.unsafeThaw
            P.=<< V.mapM estimateSize
            P.=<< V.unsafeFreeze others
        let ranked = HMV.unsafeZip ranks others
        !() <- AI.sortBy (P.comparing P.$ Down P.. P.fst) ranked
        others' <- V.unsafeFreeze others

        progress <-
          V.foldr
            ( \q rest ->
                tryPopBack q P.>>= \case
                  Just (Just x) -> P.pure $ Just (Just x)
                  Just Nothing -> rest
                  Nothing -> P.pure Nothing
            )
            (P.pure $ Just Nothing)
            others'
        case progress of
          Nothing -> P.pure Nothing
          Just Nothing -> yield P.*> self
          Just (Just x) -> P.pure $ Just (x, qs)
