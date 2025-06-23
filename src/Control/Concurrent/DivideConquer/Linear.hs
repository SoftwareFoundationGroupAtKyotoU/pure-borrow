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
import Control.Concurrent.DivideConquer.Utils.BMQueue.Linear (newBMQueue)
import Control.Concurrent.DivideConquer.Utils.BMQueue.Linear qualified as BMQ
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
import Generics.Linear.TH (deriveGenericAnd1)
import Prelude.Linear
import Prelude.Linear.Generically (Generically, Generically1)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

data DivideConquer α t a r = DivideConquer
  { divide :: forall β. (β <= α) => Mut β a %1 -> BO β (Result β t a r)
  , conquer :: Conquer α t r
  }

divideAndConquer ::
  forall α t a r.
  (Data.Traversable t, Consumable (t ())) =>
  -- | The # of workers
  Int ->
  DivideConquer α t a r ->
  Linearly %1 ->
  Mut α a %1 ->
  BO α (r, Mut α a)
divideAndConquer n DivideConquer {..} lin ini =
  dup2 lin & \(lin, lin') ->
    reborrowing ini \(ini :: Mut γ a) ->
      someNatVal (fromIntegral n) & \(SomeNat (_ :: Proxy n)) -> Control.do
        (q, lend) <- newBMQueue 128 lin
        DataFlow.do
          (q, q') <- BMQ.unsafeClone q
          (rootSink, rootSource) <- Once.new lin'
          qs <- BMQ.unsafeCloneN @n q'
          Control.do
            q <- BMQ.writeBMQueueMany q [Divide ini rootSink]
            chs <- concurrentMap worker qs
            !r <- Once.take rootSource
            BMQ.closeBMQueue q
            Control.void $ forkBO $ Control.void $ Data.traverse killThreadBO chs
            Control.pure (\end -> reclaim end lend `lseq` r)
  where
    worker :: (β <= α) => Mut β (BMQ.BMQueue (Work β a t r)) %1 -> BO β ()
    worker q =
      whileJust_ q BMQ.readBMQueue \q -> \case
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
                      ((),) Control.<$> BMQ.writeBMQueue q (Divide work sink)
                    Control.pure source
              q <- BMQ.writeBMQueue q (Unite sources sink)
              Control.pure q
        Unite sources sink -> Control.do
          pieces <- Data.traverse Once.take sources
          case conquer of
            NoOp ->
              pieces `lseq` Control.do
                Once.put sink ()
                Control.pure q
            Conquer k -> Control.do
              !res <- k pieces
              Once.put sink res
              Control.pure q

killThreadBO :: ThreadId_ %1 -> BO α ()
killThreadBO = Unsafe.toLinear \tid ->
  unsafeSystemIOToBO (Conc.killThread $ NonLinear.coerce tid)

newtype ThreadId_ = ThreadId_ ThreadId
  deriving stock (GHC.Generic)
  deriving (Consumable, Dupable) via AsMovable ThreadId_

instance Movable ThreadId_ where
  move = Unsafe.toLinear Ur

concurrentMap ::
  (Data.Traversable t) =>
  (a %1 -> BO α ()) -> t a %1 -> BO α (t ThreadId_)
concurrentMap k = Data.traverse (forkBO . k)

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

data Result β t a r = Done r | Continue (t (Mut β a))

data Conquer α t r where
  NoOp :: Conquer α t ()
  Conquer :: (forall β. (β <= α) => t r %1 -> BO β r) -> Conquer α t r

data Work α a (t :: Type -> Type) (r :: Type) where
  Divide :: Mut α a %1 -> Sink r %1 -> Work α a t r
  Unite :: t (Source r) %1 -> Sink r %1 -> Work α a t r

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
