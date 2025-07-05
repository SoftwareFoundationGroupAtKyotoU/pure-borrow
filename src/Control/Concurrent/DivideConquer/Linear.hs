{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Control.Concurrent.DivideConquer.Linear (
  divideAndConquer,
  divideAndConquerLocalQueues,
  DivideConquer (..),

  -- *  Examples
  qsortDC,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent qualified as Conc
import Control.Concurrent.DivideConquer.Linear.Types
import Control.Concurrent.DivideConquer.Utils.MQueue.Linear (newMQueue)
import Control.Concurrent.DivideConquer.Utils.MQueue.Linear qualified as MQ
import Control.Concurrent.DivideConquer.Utils.Unsafe.QueuePool.Linear (QueuePool, newQueuePool)
import Control.Concurrent.DivideConquer.Utils.Unsafe.QueuePool.Linear qualified as Pool
import Control.Functor.Linear (runStateT)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Internal
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce qualified as NonLinear
import Data.Coerce.Directed
import Data.Functor.Linear qualified as Data
import Data.Functor.Linear.Extra qualified as Data
import Data.Functor.Product qualified as F
import Data.Kind (Type)
import Data.OnceChan.Linear (Sink, Source)
import Data.OnceChan.Linear qualified as Once
import Data.Proxy (Proxy (..))
import Data.Tuple (Solo (..))
import Data.Unrestricted.Linear (dup4)
import Data.Vector.Mutable.Linear.Borrow qualified as LV
import GHC.TypeNats (SomeNat (..), someNatVal)
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

data DivideConquer α t a r = DivideConquer
  { divide :: forall β. (β <= α) => Mut β a %1 -> BO β (Result β t a r)
  , conquer :: Conquer α t r
  }

data Result β t a r = Done r | Continue (t (Mut β a))

data Conquer α t r where
  NoOp :: Conquer α t ()

{-
  -- NOTE: To handle conquer correctly, we need to be more careful
  -- with the dependency between tasks and easily deadlocks / starvation.
  --
  -- Meanwhile, qsort requires no postprocesses so we just handle No-Op.
  Conquer :: (forall β. (β <= α) => t r %1 -> BO β r) -> Conquer α t r
-}

data Work α a (t :: Type -> Type) (r :: Type) where
  Divide :: Mut α a %1 -> Sink r %1 -> Work α a t r
  Unite :: t (Source r) %1 -> Sink r %1 -> Work α a t r

divideAndConquer ::
  forall α t a r.
  (Data.Traversable t) =>
  -- | The # of workers
  Int ->
  DivideConquer α t a r ->
  Mut α a %1 ->
  BO α (r, Mut α a)
divideAndConquer n DivideConquer {..} ini = DataFlow.do
  (lin, ini) <- withLinearly ini
  (lin, lin', lin'') <- dup3 lin
  reborrowing ini \(ini :: Mut γ a) ->
    someNatVal (fromIntegral n) & \(SomeNat (_ :: Proxy n)) -> Control.do
      (q, lend) <- newMQueue lin
      DataFlow.do
        (q, q') <- MQ.unsafeClone q
        (rootSink, rootSource) <- Once.new lin'
        qs <- MQ.unsafeCloneN @n q'
        Control.do
          q <- MQ.writeMQueue q $ Divide ini rootSink
          chs <- concurrentMap lin'' worker qs
          r <- Once.take rootSource <* (chs `lseq` MQ.closeMQueue q)
          Control.pure (\end -> reclaim (upcast end) lend `lseq` r)
  where
    worker :: (β <= α) => Mut β (MQ.MQueue (Work β a t r)) %1 -> BO β ()
    worker q =
      whileJust_ q MQ.readMQueue \q -> \case
        Divide !inp !sink -> Control.do
          resl <- divide inp
          case resl of
            Done !r -> Control.do
              Once.put sink r
              Control.pure q
            Continue ts -> Control.do
              (sources, q) <- flip runStateT q Control.do
                Data.for ts \work -> Control.do
                  lin <- Control.state withLinearly
                  Once.new lin & \(sink, source) -> Control.do
                    Control.StateT \q ->
                      ((),) Control.<$> MQ.writeMQueue q (Divide work sink)
                    Control.pure source
              MQ.writeMQueue q (Unite sources sink)
        Unite sources sink -> Control.do
          case conquer of
            NoOp -> Control.do
              Once.put sink ()
              Control.pure $ Unsafe.toLinear (\_ -> ()) sources `lseq` q

divideAndConquerLocalQueues ::
  forall α t a r.
  (Data.Traversable t, Data.Unzip t, Consumable (t ())) =>
  -- | The # of workers
  Int ->
  DivideConquer α t a r ->
  Mut α a %1 ->
  BO α (r, Mut α a)
divideAndConquerLocalQueues n DivideConquer {..} ini
  | n <= 0 = error ("divideAndConquerLocalQueus: # of workers must be positive, but got" <> show n) ini
  | otherwise = case someNatVal (fromIntegral n) of
      SomeNat (Proxy :: Proxy n) -> DataFlow.do
        (lin, ini) <- withLinearly ini
        (lin, lin', lin'') <- dup3 lin
        reborrowing ini \(ini :: Mut γ a) -> DataFlow.do
          (rootSink, rootSource) <- Once.new lin'
          Control.do
            (pools, releaser, lend) <- newQueuePool @n (Divide ini rootSink) lin
            chs <- concurrentMap lin'' worker pools
            r <- Once.take rootSource <* (chs `lseq` Pool.release releaser)
            Control.pure (\end -> reclaim (upcast end) lend `lseq` r)
  where
    worker :: (β <= α) => Mut β (QueuePool (Work β a t r)) %1 -> BO β ()
    worker q = whileJust_ q Pool.popFront \q -> \case
      Divide !inp !sink -> Control.do
        resl <- divide inp
        case resl of
          Done !r -> Control.do
            Once.put sink r
            Control.pure q
          Continue ts ->
            withLinearly q & \(lin, q) -> Control.do
              (works, sources) <-
                Data.unzip Control.<$> flip Control.runReaderT lin Control.do
                  Data.for ts \work -> Control.do
                    lin <- Control.ask
                    (sink, source) <- Control.pure $ Once.new lin
                    Control.pure (Divide work sink, source)
              Pool.prependFront q $ F.Pair works (MkSolo (Unite sources sink))
      Unite sources sink -> Control.do
        -- pieces <- Data.mapM Once.take sources
        case conquer of
          NoOp -> Control.do
            Once.put sink ()
            Control.pure $ Unsafe.toLinear (\_ -> ()) sources `lseq` q

killThreadBO :: Thread %1 -> BO α ()
killThreadBO = Unsafe.toLinear \(Thread tid _) ->
  unsafeSystemIOToBO (Conc.killThread $ NonLinear.coerce tid)

concurrentMap ::
  (Data.Traversable t) =>
  Linearly %1 ->
  (a %1 -> BO α ()) ->
  t a %1 ->
  BO α (t Thread)
concurrentMap lin k ts = flip Control.runReaderT lin do
  Data.traverse
    ( \a -> Control.do
        lin <- Control.ask
        Control.lift DataFlow.do
          (sink, source) <- Once.new lin
          Control.do
            tid <- forkBO (k a Control.>> Once.put sink ())
            Control.pure (Thread tid source)
    )
    ts

forkBO :: BO α () %1 -> BO α ThreadId_
forkBO = Unsafe.toLinear \bo ->
  unsafeSystemIOToBO (ThreadId_ NonLinear.<$> forkIO (unsafeBOToSystemIO bo))

whileJust_ ::
  (Control.Monad m) =>
  r %1 ->
  (r %1 -> m (Maybe (a, r))) ->
  (r %1 -> a %1 -> m r) ->
  m ()
whileJust_ ini next action = loop ini
  where
    loop cur = Control.do
      m <- next cur
      case m of
        Nothing -> Control.pure ()
        Just (!x, !cur) -> Control.do
          cur <- action cur x
          loop cur

qsortDC ::
  (Ord a, Movable a, Deborrowable a) =>
  -- | Threshold for the length of vector to switch to sequential sort
  Int ->
  DivideConquer α Pair (LV.Vector a) ()
qsortDC thresh =
  DivideConquer
    { divide = \vs ->
        case LV.size vs of
          (Ur n, v)
            | n <= 1 ->
                v `lseq` Control.pure (Done ())
            | n <= thresh ->
                Done Control.<$> LV.qsort 0 v
            | otherwise -> Control.do
                let i = n `quot` 2
                (pivot, v) <- sharing_ v \v ->
                  move . deborrow Control.<$> LV.unsafeGet i v
                pivot & \(Ur pivot) -> Control.do
                  (lo, hi) <- LV.divide pivot v 0 n
                  Control.pure $ Continue $ Pair lo hi
    , conquer = NoOp
    }
