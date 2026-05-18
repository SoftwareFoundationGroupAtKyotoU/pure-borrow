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
import Control.Concurrent (threadDelay)
import Control.Concurrent.Queue.ChaseLev (ChaseLevDeq, StealResult (..), close, estimateSize, isClosed, newDeq, pushFront, pushFronts, stealHalf, tryPopFront)
import Control.Monad qualified as NonLinear
import Control.Monad qualified as P
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..), unsafeSystemIOToBO)
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)))
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

popWork :: Mut α (QueuePool a) %1 -> BO α (Maybe (a, Mut α (QueuePool a)))
popWork = Unsafe.toLinear \qs@(UnsafeAlias QueuePool {..}) ->
  unsafeSystemIOToBO do
    tryPopFront mine P.>>= \case
      Nothing -> do
        P.pure Nothing
      Just (Just x) -> do
        P.pure $ Just (x, qs)
      Just Nothing ->
        (1 :: Double) & fix \self !wait -> do
          let waitRandom = do
                g <- readIORef gen
                let (q, g') = randomR (0, floor wait) g
                writeIORef gen g'
                threadDelay q
              !wait' = min 500 $ wait * 1.5
          cls <- isClosed mine
          if cls
            then do
              P.pure Nothing
            else do
              let !nOthers = V.length others
              if
                | V.null others -> P.pure Nothing
                | V.length others == 1 -> do
                    let !q = V.unsafeHead others
                    progress <- stealHalf q
                    case progress of
                      Nothing -> do
                        P.pure Nothing
                      Just (Found (x :| xs)) -> do
                        pushFronts mine xs
                        P.pure $ Just (x, qs)
                      Just {} -> do
                        waitRandom P.*> self wait'
                | otherwise -> do
                    g0 <- readIORef gen
                    let (!i, !g1) = randomR (0, nOthers - 1) g0
                        (!j0, !g2) = randomR (0, nOthers - 2) g1
                        !j = if j0 P.== i then nOthers - 1 else j0
                    writeIORef gen g2
                    let !q1 = V.unsafeIndex others i
                        !q2 = V.unsafeIndex others j
                    !s1 <- estimateSize q1
                    !s2 <- estimateSize q2
                    let !targ = if s1 P.>= s2 then q1 else q2
                    progress <- stealHalf targ
                    case progress of
                      Nothing -> P.pure Nothing
                      Just (Found (x :| xs)) -> do
                        pushFronts mine xs
                        P.pure $ Just (x, qs)
                      Just {} -> waitRandom P.*> self wait'
