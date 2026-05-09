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
import Control.Concurrent.Queue.ChaseLev (ChaseLevDeq, StealResult (..), close, estimateSize, isClosed, newDeq, pushFront, pushFronts, tryPopBack, tryPopFront)
import Control.Monad qualified as NonLinear
import Control.Monad qualified as P
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..), unsafeSystemIOToBO)
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.List qualified as L
import Data.V.Linear (V, theLength)
import Data.V.Linear.Internal (V (..))
import Data.Vector qualified as V
import Data.Vector.Generic qualified as GV
import Data.Vector.Hybrid qualified as HV
import Data.Vector.Mutable (RealWorld)
import Debug.Trace (traceEventIO)
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
    traceEventIO $ "Pushing work..."
    pushFront mine work
    P.pure $ UnsafeAlias QueuePool {..}

-- | Pushes works, the first element is on top.
pushWorks :: Mut α (QueuePool a) %1 -> [a] %1 -> BO α (Mut α (QueuePool a))
pushWorks = Unsafe.toLinear2 \(UnsafeAlias QueuePool {..}) works ->
  unsafeSystemIOToBO do
    traceEventIO ("Pushing " P.<> P.show (P.length works) P.<> " works...")
    pushFronts mine works
    P.pure $ UnsafeAlias QueuePool {..}

popWork :: Mut α (QueuePool a) %1 -> BO α (Maybe (a, Mut α (QueuePool a)))
popWork = Unsafe.toLinear \qs@(UnsafeAlias QueuePool {..}) ->
  unsafeSystemIOToBO do
    tryPopFront mine P.>>= \case
      Nothing -> do
        traceEventIO "WORK[P]: Queue closed!"
        P.pure Nothing
      Just (Just x) -> do
        traceEventIO "WORK[P]: hit!"
        P.pure $ Just (x, qs)
      Just Nothing -> fix \self -> do
        traceEventIO "WORK[P]: no hit. stealing from others..."
        cls <- isClosed mine
        if cls
          then do
            traceEventIO "WORK[P]: Seems we are done"
            P.pure Nothing
          else do
            !others0 <- V.unsafeFreeze others
            !ranks <- V.mapM estimateSize others0
            traceEventIO $ "WORK[P]: ranks = " <> show ranks
            let (rank, targ) = GV.maximumOn fst $ HV.unsafeZip ranks others0
            -- ranks <- V.unsafeFreeze ranks
            if rank <= 0
              then do
                traceEventIO "WORK[P]: non avail. retry..."
                yield P.*> self
              else do
                progress <- tryPopBack targ
                case progress of
                  Nothing -> do
                    traceEventIO "WORK[P]: Seems we are done"
                    P.pure Nothing
                  Just Race -> do
                    traceEventIO "WORK[P]: failed to steal (race). retrying..."
                    yield P.*> self
                  Just Empty -> do
                    traceEventIO "WORK[P]: failed to steal (empty). retrying..."
                    yield P.*> self
                  Just (Found x) -> do
                    traceEventIO "WORK[P]: steal success!"
                    P.pure $ Just (x, qs)
