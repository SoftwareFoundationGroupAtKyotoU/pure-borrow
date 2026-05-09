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

import Control.Applicative (Alternative (..))
import Control.Applicative qualified as P
import Control.Concurrent (yield)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TMDeque (TMDeque, closeTMDeque, countTMDequeIO, isClosedTMDeque, isClosedTMDequeIO, newTMDequeIO, pushFrontTMDeque, sizeTMDeque, tryPopBackTMDeque, tryPopFrontTMDeque)
import Control.Monad qualified as NonLinear
import Control.Monad qualified as P
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..), unsafeSystemIOToBO)
import Data.Coerce (coerce)
import Data.Foldable qualified as P
import Data.Function (fix)
import Data.List qualified as L
import Data.Monoid (Alt (..))
import Data.Ord (Down (..))
import Data.Ord qualified as P
import Data.V.Linear (V, theLength)
import Data.V.Linear.Internal (V (..))
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as AI
import Data.Vector.Generic qualified as GV
import Data.Vector.Hybrid qualified as HV
import Data.Vector.Hybrid.Mutable qualified as HMV
import Data.Vector.Mutable (RealWorld)
import GHC.Exts qualified as GHC
import GHC.IO qualified as GHC
import GHC.TypeLits (KnownNat)
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as P

data QueuePool a = QueuePool
  { mine :: !(TMDeque a)
  , others :: !(V.Vector (TMDeque a))
  , num :: !Int
  }

newtype MasterQueuePool a = MasterQueuePool [TMDeque a]

instance Consumable (MasterQueuePool a) where
  consume = consume . map consumeTMDQ . Unsafe.coerce @_ @[TMDeque a]

consumeTMDQ :: TMDeque a %1 -> ()
{-# NOINLINE consumeTMDQ #-}
consumeTMDQ = GHC.noinline $ Unsafe.toLinear \q -> GHC.unsafePerformIO do
  !() <- atomically $ closeTMDeque q
  P.pure ()

newQueuePool ::
  forall n a α.
  (KnownNat n) =>
  BO α (V n (Mut α (QueuePool a)), MasterQueuePool a)
newQueuePool = unsafeSystemIOToBO do
  let n = theLength @n

  qs <- NonLinear.replicateM n newTMDequeIO
  pools <-
    P.mapM
      ( \(num, ini, mine, tl) -> do
          let others = V.fromList $ tl <> ini
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
      atomically $ pushFrontTMDeque q work
      P.pure $ UnsafeAlias $ MasterQueuePool (q : qs)
    [] -> error "impossible: the length of pools is determined by the type-level nat n and cannot be zero"

pushWork :: Mut α (QueuePool a) %1 -> a %1 -> BO α (Mut α (QueuePool a))
pushWork = Unsafe.toLinear2 \(UnsafeAlias QueuePool {..}) work ->
  unsafeSystemIOToBO do
    atomically $ pushFrontTMDeque mine work
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
    atomically $ runBackwards P.$ P.traverse_ (Backwards P.. pushFrontTMDeque mine) work
    P.pure $ UnsafeAlias QueuePool {..}

popWork :: Mut α (QueuePool a) %1 -> BO α (Maybe (a, Mut α (QueuePool a)))
popWork = Unsafe.toLinear \qs@(UnsafeAlias QueuePool {..}) ->
  unsafeSystemIOToBO do
    atomically (tryPopFrontTMDeque mine) P.>>= \case
      Nothing -> P.pure Nothing
      Just (Just x) -> P.pure $ Just (x, qs)
      Just Nothing -> fix \self -> do
        isCls <- isClosedTMDequeIO mine
        if isCls
          then P.pure Nothing
          else do
            !ranks <- V.mapM countTMDequeIO others
            -- traceEventIO $ "WORK[P]: ranks = " <> show ranks
            let (rank, targ) = GV.maximumOn fst $ HV.unsafeZip ranks others

            if rank <= 0
              then do
                -- traceEventIO "WORK[P]: non avail. retry..."
                yield P.*> self
              else do
                progress <- atomically $ tryPopBackTMDeque targ
                case progress of
                  Nothing -> do
                    -- traceEventIO "WORK[P]: Seems we are done"
                    P.pure Nothing
                  Just (Just (x)) -> do
                    -- traceEventIO $ "WORK[P]: steal success (" <> P.show (P.length xs + 1) <> " items)!"
                    -- pushFronts mine xs
                    P.pure $ Just (x, qs)
                  Just {} -> do
                    -- traceEventIO $ "WORK[P]: failed to steal (" <> kind p <> "). retrying..."
                    yield P.*> self

fromJustSTM :: Maybe (Maybe a) -> STM (Maybe a)
fromJustSTM = P.maybe (P.pure Nothing) $ P.maybe retry (P.pure P.. Just)
