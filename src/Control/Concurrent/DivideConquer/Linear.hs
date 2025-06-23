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
  DivideConquer (..),

  -- *  Examples
  qsortDC,
) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent qualified as Conc
import Control.Concurrent.DivideConquer.Utils.MQueue.Linear (newMQueue)
import Control.Concurrent.DivideConquer.Utils.MQueue.Linear qualified as MQ
import Control.Functor.Linear (runStateT)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Affine (Affable, GenericallyAffable (..))
import Control.Monad.Borrow.Pure.Internal
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce qualified as NonLinear
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.OnceChan.Linear (Sink, Source)
import Data.OnceChan.Linear qualified as Once
import Data.Proxy (Proxy (..))
import Data.Unrestricted.Linear (AsMovable (..))
import Data.Vector.Mutable.Linear.Borrow qualified as LV
import GHC.Generics qualified as GHC
import GHC.TypeNats (SomeNat (..), someNatVal)
import Generics.Linear.TH (deriveGeneric, deriveGenericAnd1)
import Prelude.Linear
import Prelude.Linear.Generically (Generically, Generically1)
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
          q <- MQ.writeMQueueMany q [Divide ini rootSink]
          chs <- concurrentMap lin'' worker qs
          r <- case conquer of
            NoOp -> Control.do
              Once.take rootSource
              MQ.closeMQueue q
              Control.void $ Data.traverse wait chs
              Control.pure ()
          {-
            -- NOTE: To handle conquer correctly, we need to be more careful
            -- with the dependency between tasks and easily deadlocks / starvation.
            --
            -- Meanwhile, qsort requires no postprocesses so we just handle No-Op.
            Conquer -> Control.do
              !r <- Once.take rootSource
              MQ.closeMQueue q
              Control.void $ forkBO $ Control.void $ Data.traverse killThreadBO chs
          -}
          Control.pure (\end -> reclaim end lend `lseq` r)
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
              q <- MQ.writeMQueue q (Unite sources sink)
              Control.pure q
        Unite sources sink -> Control.do
          case conquer of
            NoOp -> Control.do
              Once.put sink ()
              Control.pure $ Unsafe.toLinear (\_ -> ()) sources `lseq` q

{-
            -- NOTE: To handle conquer correctly, we need to be more careful
            -- with the dependency between tasks and easily deadlocks / starvation.
            --
            -- Meanwhile, qsort requires no postprocesses so we just handle No-Op.
            Conquer k ->
              pieces <- Data.traverse Once.take sources
              !res <- k pieces
              Once.put sink res
              Control.pure q
  -}

newtype ThreadId_ = ThreadId_ ThreadId
  deriving stock (GHC.Generic)
  deriving (Consumable, Dupable) via AsMovable ThreadId_

instance Movable ThreadId_ where
  move = Unsafe.toLinear Ur

wait :: Thread %1 -> BO α ()
wait (Thread tid source) = tid `lseq` Once.take source

data Thread = Thread !ThreadId_ !(Source ())
  deriving stock (GHC.Generic)

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

deriveGeneric ''Thread

deriving via Generically Thread instance Consumable Thread

data Pair a where
  Pair :: !a %1 -> !a %1 -> Pair a
  deriving (GHC.Generic, GHC.Generic1)

deriveGenericAnd1 ''Pair

deriving via Generically1 Pair instance Data.Functor Pair

deriving via
  Generically (Pair a)
  instance
    (Consumable a) => Consumable (Pair a)

deriving via
  Generically (Pair a)
  instance
    (Dupable a) => Dupable (Pair a)

deriving via
  GenericallyAffable (Pair a)
  instance
    (Affable a) => Affable (Pair a)

deriving via
  Generically (Pair a)
  instance
    (Movable a) => Movable (Pair a)

instance Data.Traversable Pair where
  traverse = Data.genericTraverse
  {-# INLINE traverse #-}

qsortDC ::
  (Ord a, Movable a, Derefable a) =>
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
                  move . derefShare Control.<$> LV.unsafeGet i v
                pivot & \(Ur pivot) -> Control.do
                  (lo, hi) <- LV.divide pivot v 0 n
                  Control.pure $ Continue $ Pair lo hi
    , conquer = NoOp
    }
