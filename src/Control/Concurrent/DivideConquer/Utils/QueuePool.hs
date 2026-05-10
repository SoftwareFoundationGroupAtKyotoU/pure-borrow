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
import Control.Concurrent.Queue.ChaseLev (ChaseLevDeq, StealResult (..), close, estimateSize, isClosed, newDeq, pushFront, pushFronts, stealHalf, tryPopFront)
import Control.Monad qualified as NonLinear
import Control.Monad qualified as P
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..), unsafeSystemIOToBO)
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Data.V.Linear (V, theLength)
import Data.V.Linear.Internal (V (..))
import Data.Vector qualified as V
import GHC.Exts qualified as GHC
import GHC.IO qualified as GHC
import GHC.TypeLits (KnownNat)
import Prelude.Linear
import System.Random.Stateful (Random (randoms), RandomGen, StdGen, mkStdGen, randomR)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as P

data QueuePool a = QueuePool
  { mine :: !(ChaseLevDeq a)
  , others :: !(V.Vector (ChaseLevDeq a))
  , num :: {-# UNPACK #-} !Int
  , gen :: {-# UNPACK #-} !(IORef StdGen)
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
  forall n a α g.
  (KnownNat n, RandomGen g) =>
  g ->
  BO α (V n (Mut α (QueuePool a)), MasterQueuePool a)
newQueuePool g = unsafeSystemIOToBO do
  let n = theLength @n

  qs <- NonLinear.replicateM n newDeq
  pools <-
    P.mapM
      ( \(num, ini, mine, tl, seed) -> do
          let others = V.fromList $ tl <> ini
          gen <- newIORef $ mkStdGen seed
          P.pure P.$ QueuePool {others, ..}
      )
      P.$ L.zip5
        [0 ..]
        (L.inits qs)
        qs
        (P.drop 1 $ L.tails qs)
        (randoms g)
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

-- | Pushes works, the last element is on the front.
pushWorks :: Mut α (QueuePool a) %1 -> [a] %1 -> BO α (Mut α (QueuePool a))
pushWorks = Unsafe.toLinear2 \(UnsafeAlias QueuePool {..}) works ->
  unsafeSystemIOToBO do
    pushFronts mine works
    P.pure $ UnsafeAlias QueuePool {..}

weighted :: V.Vector (Int, a) -> StdGen -> (StdGen, a)
weighted xs gen =
  let !accs = V.scanl1' (\(!i, _) (!j, !x) -> (i + j, x)) xs
      !ub = fst $ V.last accs
      !(!r, !gen') = randomR (0, ub - 1) gen
   in (gen', snd $ fromJust $ V.find ((P.> r) P.. fst) accs)

popWork :: Mut α (QueuePool a) %1 -> BO α (Maybe (a, Mut α (QueuePool a)))
popWork = Unsafe.toLinear \qs@(UnsafeAlias QueuePool {..}) ->
  unsafeSystemIOToBO do
    tryPopFront mine P.>>= \case
      Nothing -> do
        P.pure Nothing
      Just (Just x) -> do
        P.pure $ Just (x, qs)
      Just Nothing -> fix \self -> do
        cls <- isClosed mine
        if cls
          then do
            P.pure Nothing
          else do
            !candidates <- V.filter ((P.> 0) P.. fst) P.<$> V.mapM (\a -> (,a) P.<$> estimateSize a) others
            if V.null candidates
              then do
                yield P.*> self
              else do
                targ <- atomicModifyIORef' gen $ weighted candidates
                progress <- stealHalf targ
                case progress of
                  Nothing -> do
                    P.pure Nothing
                  Just (Found (x :| xs)) -> do
                    pushFronts mine xs
                    P.pure $ Just (x, qs)
                  Just {} -> do
                    yield P.*> self
