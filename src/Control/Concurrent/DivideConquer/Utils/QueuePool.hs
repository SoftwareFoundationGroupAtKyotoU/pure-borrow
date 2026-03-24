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
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TMDeque (TMDeque, closeTMDeque, isClosedTMDeque, isClosedTMDequeIO, newTMDequeIO, pushFrontTMDeque, tryPopBackTMDeque, tryPopFrontTMDeque)
import Control.Monad qualified as NonLinear
import Control.Monad qualified as P
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Internal
import Data.Coerce (coerce)
import Data.Foldable qualified as P
import Data.Function (fix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as L
import Data.Monoid (Alt (..))
import Data.V.Linear (V, theLength)
import Data.V.Linear.Internal (V (..))
import Data.Vector (MVector)
import Data.Vector qualified as V
import Data.Vector.Mutable (RealWorld)
import Data.Vector.Mutable qualified as MV
import GHC.Exts qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.IO qualified as GHC
import GHC.TypeLits (KnownNat)
import Prelude.Linear
import System.Random (RandomGen)
import System.Random qualified as R
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as P

data QueuePool a = forall g. (RandomGen g) => QueuePool
  { mine :: !(TMDeque a)
  , others :: !(V.MVector RealWorld (TMDeque a))
  , gen :: !(IORef g)
  }

instance Consumable (QueuePool a) where
  {-# NOINLINE consume #-}
  consume = Unsafe.toLinear \(QueuePool {..}) -> unsafePerformIO do
    P.mapM_ (atomically P.. closeTMDeque) P.. V.toList P.=<< V.unsafeFreeze others
    atomically $ closeTMDeque mine

newtype MasterQueuePool a = MasterQueuePool [TMDeque a]

instance Consumable (MasterQueuePool a) where
  consume = consume . map consumeTMDQ . Unsafe.coerce @_ @[TMDeque a]

consumeTMDQ :: TMDeque a %1 -> ()
{-# NOINLINE consumeTMDQ #-}
consumeTMDQ = GHC.noinline $ Unsafe.toLinear \q -> GHC.unsafePerformIO do
  GHC.noDuplicate
  !() <- atomically $ closeTMDeque q
  P.pure ()

newQueuePool ::
  forall n a α g.
  (KnownNat n, RandomGen g) =>
  g ->
  BO α (V n (Mut α (QueuePool a)), MasterQueuePool a)
newQueuePool g = unsafeSystemIOToBO do
  let n = theLength @n

  qs <- NonLinear.replicateM n newTMDequeIO
  pools <-
    P.mapM
      ( \(ini, mine, tl, gen) -> do
          gen <- newIORef gen
          others <- V.unsafeThaw $ V.fromList $ ini <> tl
          P.pure P.$ QueuePool {others, ..}
      )
      P.$ L.zip4
        (L.inits qs)
        qs
        (P.drop 1 $ L.tails qs)
        (P.take n $ L.unfoldr (Just P.. R.split) g)
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
      Just Nothing -> do
        shuffle gen others
        !others' <- V.unsafeFreeze others
        P.fmap (,qs) P.<$!> atomically do
          ( isClosedTMDeque mine P.>>= \closed ->
              if closed then P.pure Nothing else retry
            )
            <|> getAlt (P.foldMap' (Alt P.. (fromJustSTM P.<=< tryPopBackTMDeque)) P.$ others')

fromJustSTM :: Maybe (Maybe a) -> STM (Maybe a)
fromJustSTM = P.maybe (P.pure Nothing) $ P.maybe retry (P.pure P.. Just)

shuffle :: (RandomGen g) => IORef g -> MVector RealWorld a -> IO ()
shuffle g mv =
  MV.length mv - 1 & fix \self !n ->
    if n == 0
      then P.pure ()
      else do
        gen <- readIORef g
        let (!k, !g') = R.randomR (0, n) gen
        writeIORef g g'
        MV.swap mv n k
        self $! n P.- 1
