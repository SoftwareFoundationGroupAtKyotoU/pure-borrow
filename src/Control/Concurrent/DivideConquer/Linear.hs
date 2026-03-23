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
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear (Sink, Source)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear qualified as Once
import Control.Concurrent.DivideConquer.Utils.QueuePool (QueuePool, newQueuePool, popWork, pushWork, pushWorkMaster)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Affine (Affine, GenericallyAffine (..))
import Control.Monad.Borrow.Pure.Internal
import Data.Coerce qualified as NonLinear
import Data.Coerce.Directed
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Unrestricted.Linear (AsMovable (..))
import Data.V.Linear (V)
import Data.V.Linear.Internal (V (..))
import Data.Vector.Mutable.Linear.Borrow qualified as LV
import GHC.Generics qualified as GHC
import GHC.TypeNats (SomeNat (..), someNatVal)
import Generics.Linear.TH (deriveGeneric, deriveGenericAnd1)
import Prelude.Linear
import Prelude.Linear.Generically (Generically, Generically1)
import System.Random (RandomGen)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

data DivideConquer α t a = DivideConquer
  { divide :: forall β. (β <= α) => Mut β a %1 -> BO β (Result β t a)
  }

data Result β t a = Done | Continue (t (Mut β a))

data Work α a (t :: Type -> Type) where
  Process :: Mut α a %1 -> Sink () %1 -> Work α a t %1 -> Work α a t
  Unite :: t (Source ()) %1 -> Sink () %1 -> Work α a t
  Final :: Work α a t

-- TODO: perhaps we can use atomic counter here again?

divideAndConquer ::
  forall α t a g.
  (RandomGen g, Data.Traversable t, Consumable (t ())) =>
  -- | The # of workers
  Int ->
  DivideConquer α t a ->
  g ->
  Mut α a %1 ->
  BO α (Mut α a)
divideAndConquer n DivideConquer {..} g ini
  | n == 0 = error ("divideAndConquer: # of workers must be positive, but got: " <> show n) ini
  | otherwise =
      uncurry (lseq @()) Control.<$> reborrowing' ini \(ini :: Mut γ a) ->
        someNatVal (fromIntegral n) & \(SomeNat (_ :: Proxy n)) -> Control.do
          (workers, master) <- newQueuePool @n g
          (masterQ, masterLend) <- asksLinearly $ borrow master
          (rootSink, rootSource) <- asksLinearly Once.new

          Control.void $ pushWorkMaster masterQ $ Process ini rootSink Final

          concurrentMap_ worker workers
          Once.take rootSource
          Control.pure (upcast $ consume Control.<$> reclaim' masterLend)
  where
    worker :: (β <= α) => Mut β (QueuePool (Work β a t)) %1 -> BO β ()
    worker q =
      whileJust_ q popWork \q -> \case
        Final -> Control.pure q
        Process ini sink next -> Control.do
          q <- pushWork q next
          resl <- divide ini
          case resl of
            Done -> Control.do
              Once.put sink ()
              Control.pure q
            Continue ts -> Control.do
              (sources, k) <-
                flip Control.runStateT mempty $ Data.for ts \work -> Control.do
                  (sink, source) <- Control.lift $ asksLinearly Once.new
                  Control.modify (<> Endo (Process work sink))
                  Control.pure source
              pushWork q $ appEndo k $ Unite sources sink
        Unite children sink -> Control.do
          Control.void $ Data.traverse Once.take children
          Once.put sink ()
          Control.pure q

newtype ThreadId_ = ThreadId_ ThreadId
  deriving stock (GHC.Generic)
  deriving (Consumable, Dupable) via AsMovable ThreadId_

instance Movable ThreadId_ where
  move = Unsafe.toLinear Ur

data Thread = Thread !ThreadId_ !(Source ())
  deriving stock (GHC.Generic)

-- Named with underscore, as it is not used for now
_killThreadBO :: Thread %1 -> BO α ()
_killThreadBO = Unsafe.toLinear \(Thread tid _) ->
  unsafeSystemIOToBO (Conc.killThread $ NonLinear.coerce tid)

concurrentMap_ ::
  forall n a α.
  (a %1 -> BO α ()) ->
  V n a %1 ->
  BO α ()
concurrentMap_ k = Unsafe.toLinear \(V ts) -> unsafeSystemIOToBO do
  NonLinear.mapM_
    ( \a -> unsafeBOToSystemIO Control.do
        (sink, source) <- asksLinearly Once.new
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
  GenericallyAffine (Pair a)
  instance
    (Affine a) => Affine (Pair a)

deriving via
  Generically (Pair a)
  instance
    (Movable a) => Movable (Pair a)

instance Data.Traversable Pair where
  traverse = Data.genericTraverse
  {-# INLINE traverse #-}

qsortDC ::
  (RandomGen g, Ord a, Copyable a) =>
  -- | The # of workers
  Int ->
  -- | Threshold for the length of vector to switch to sequential sort
  Int ->
  g ->
  Mut α (LV.Vector a) %1 ->
  BO α (Mut α (LV.Vector a))
qsortDC nwork thresh = divideAndConquer nwork (qsortDC' thresh)

qsortDC' ::
  (Ord a, Copyable a) =>
  -- | Threshold for the length of vector to switch to sequential sort
  Int ->
  DivideConquer α Pair (LV.Vector a)
qsortDC' thresh =
  DivideConquer
    { divide = \vs ->
        case LV.size vs of
          (Ur n, v)
            | n <= 1 ->
                v `lseq` Control.pure Done
            | n <= thresh ->
                Done Control.<$ LV.qsort 0 v
            | otherwise -> Control.do
                let i = n `quot` 2
                (Ur pivot, v) <- LV.copyAtMut i v
                (lo, hi) <- LV.divide pivot v 0 n
                Control.pure $ Continue $ Pair lo hi
    }
