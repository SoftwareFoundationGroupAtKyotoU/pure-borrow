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

  -- * Examples
  qsortDC,
) where

import Control.Applicative qualified as NonLinear
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.DivideConquer.Utils.AtomicCounter (Counter)
import Control.Concurrent.DivideConquer.Utils.AtomicCounter qualified as Counter
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear (Sink, Source)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear qualified as Once
import Control.Concurrent.DivideConquer.Utils.QueuePool (QueuePool, newQueuePool, popWork, pushWorkMaster, pushWorks)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine (Affine, GenericallyAffine (..))
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Copyable
import Data.Bifunctor.Linear qualified as BiL
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.V.Linear (V)
import Data.V.Linear.Internal (V (..))
import Data.Vector qualified as V
import Data.Vector.Mutable.Linear.Borrow qualified as LV
-- import Debug.Trace (traceEventIO)
import GHC.Generics qualified as GHC
import GHC.TypeNats (SomeNat (..), someNatVal)
import Generics.Linear.TH (deriveGenericAnd1)
import Prelude.Linear hiding (foldMap)
import Prelude.Linear.Generically (Generically, Generically1)
import System.Random (RandomGen)
import Unsafe.Linear qualified as Unsafe

data DivideConquer α t a = DivideConquer
  { divide :: forall β. (α >= β) => Mut β a %1 -> BO β (Result β t a)
  }

data Result β t a = Done | Continue (t (Mut β a))

data Switch
  = Switch
      {-# UNPACK #-} !Counter
      {-# UNPACK #-} !(Maybe (Sink ()))
      {-# UNPACK #-} !(Maybe Switch)

release :: Switch %1 -> BO α ()
release (Switch counter dest parent) = Control.do
  -- unsafeSystemIOToBO $ traceEventIO "WORK[SR]: Releasing switch..."
  isLast <- Counter.release counter
  if isLast
    then Control.do
      -- unsafeSystemIOToBO $ traceEventIO "WORK[SR]: Last!"
      maybe (Control.pure ()) (`Once.put` ()) dest
      case parent of
        Nothing -> Control.pure ()
        Just p -> release p
    else Unsafe.toLinear2 (\_ _ -> Control.pure ()) dest parent

newSwitch :: Int -> Switch %1 -> BO α Switch
newSwitch n parent = Control.do
  counter <- Counter.newCounter n
  Control.pure (Switch counter Nothing (Just parent))

newRootSwitch :: Int -> BO α (Switch, Source ())
newRootSwitch n = Control.do
  counter <- Counter.newCounter n
  (sink, source) <- asksLinearly Once.new
  Control.pure (Switch counter (Just sink) Nothing, source)

data Work α a (t :: Type -> Type) where
  Process ::
    Mut α a %1 ->
    {-# UNPACK #-} !Switch %1 ->
    Work α a t

newtype Thread = Thread ThreadId

newtype DList a = DList ([a] %1 -> [a])

instance Semigroup (DList a) where
  DList l <> DList r = DList (l . r)
  {-# INLINE (<>) #-}

instance Monoid (DList a) where
  mempty = DList id
  {-# INLINE mempty #-}

singletonD :: a %1 -> DList a
singletonD = DList . (:)
{-# INLINE singletonD #-}

toListD :: DList a %1 -> [a]
toListD (DList f) = f []
{-# INLINE toListD #-}

newtype QState α a t = Idle (Mut α (QueuePool (Work α a t)))

popQState ::
  QState α a t %1 ->
  BO α (Maybe (Work α a t, QState α a t))
popQState = \case
  Idle q -> Control.do
    -- unsafeSystemIOToBO $ traceEventIO "WORK[S]: popping work from queue..."
    Data.fmap (BiL.second Idle) Control.<$> popWork q

enqueues :: QState α a t %1 -> [Work α a t] %1 -> BO α (QState α a t)
enqueues q work = case q of
  Idle q -> Idle Control.<$> pushWorks q work

data P a where
  P :: {-# UNPACK #-} !Int -> !a %1 -> P a

divideAndConquer ::
  forall α β t a g.
  (Data.Traversable t, Consumable (t ()), α >= β, RandomGen g) =>
  g ->
  -- | The # of workers.
  Int ->
  DivideConquer α t a ->
  Mut α a %1 ->
  BO β (Mut α a)
divideAndConquer g n DivideConquer {..} ini
  | n == 0 = error ("divideAndConquer: # of workers must be positive, but got: " <> show n) ini
  | otherwise =
      upcast $
        uncurry (lseq @()) Control.<$> reborrowing' ini \(ini :: Mut γ a) ->
          someNatVal (fromIntegral n) & \(SomeNat (_ :: Proxy n)) -> Control.do
            (workers, master) <- newQueuePool @n g
            (masterQ, masterLend) <- asksLinearly $ borrow master
            (switch, rootSource) <- newRootSwitch 1

            Control.void $ pushWorkMaster masterQ $ Process ini switch

            concurrentMap_ worker workers
            Once.take rootSource
            -- unsafeSystemIOToBO $ traceEventIO "WORK[DC]: All work done."

            Control.pure (upcast $ consume Control.<$> reclaim' masterLend)
  where
    worker :: (α >= α') => Mut α' (QueuePool (Work α' a t)) %1 -> BO α' ()
    worker q = Control.do
      whileJust_ (Idle q) popQState \q -> \case
        Process ini switch -> Control.do
          resl <- divide ini
          case resl of
            Done -> Control.do
              -- unsafeSystemIOToBO $ traceEventIO "WORK[WP]: No more division needed. Return"
              release switch
              Control.pure q
            Continue ts -> Control.do
              -- unsafeSystemIOToBO $ traceEventIO "WORK[WP]: Division occurred."
              P num ks <- Control.do
                flip Control.execStateT (P 0 mempty) $
                  consume Control.<$> Data.for ts \work ->
                    Control.modify \(P num ks) -> P (num + 1) $ ks <> singletonD work
              if num == 0
                then Control.do
                  release switch
                  toListD ks `lseq` Control.pure q
                else Control.do
                  Ur switch' <- Unsafe.toLinear Ur Control.<$> newSwitch num switch
                  enqueues q $ map (`Process` switch') $ toListD ks

{- Unite children sink -> Control.do
  unsafeSystemIOToBO $ traceEventIO "WORK[WU]: Unite work reached."
  Control.void $ Data.traverse Once.take children
  unsafeSystemIOToBO $ traceEventIO "WORK[WU]: All children waited."
  Once.put sink ()
  unsafeSystemIOToBO $ traceEventIO "WORK[WU]: United."
  Control.pure q -}

-- unsafeSystemIOToBO $ traceEventIO "WORK[W-]: All job done!"

concurrentMap_ ::
  forall n a α.
  (a %1 -> BO α ()) ->
  V n a %1 ->
  BO α ()
concurrentMap_ k = Unsafe.toLinear \(V ts) -> unsafeSystemIOToBO do
  V.mapM_
    (\a -> unsafeBOToSystemIO $ forkBO (k a))
    ts

forkBO :: BO α () %1 -> BO α Thread
forkBO = Unsafe.toLinear \bo ->
  unsafeSystemIOToBO (Thread NonLinear.<$> forkIO (unsafeBOToSystemIO bo))

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
  (Ord a, Copyable a, α >= β, RandomGen g) =>
  g ->
  -- | The # of workers.
  Int ->
  -- | Threshold for the length of vector to switch to sequential sort.
  Int ->
  Mut α (LV.Vector a) %1 ->
  BO β (Mut α (LV.Vector a))
qsortDC g nwork thresh = divideAndConquer g nwork (qsortDC' thresh)

qsortDC' ::
  (Ord a, Copyable a) =>
  -- | Threshold for the length of vector to switch to sequential sort.
  Int ->
  DivideConquer α Pair (LV.Vector a)
qsortDC' thresh =
  DivideConquer
    { divide = \vs ->
        case LV.size vs of
          (Ur n, v)
            | n <= 1 ->
                v `lseq` Control.pure Done
            | n <= thresh -> Control.do
                -- unsafeSystemIOToBO $ traceEventIO "WORK[QS]: Sequential sorting..."
                !() <- LV.qsort 0 v
                -- unsafeSystemIOToBO (traceEventIO "WORK[QS]: Sequential sorting done.")
                Control.pure Done
            | otherwise -> Control.do
                let i = n `quot` 2
                (Ur pivot, v) <- LV.copyAtMut i v
                (lo, hi) <- LV.divide pivot v 0 n
                Control.pure $ Continue $ Pair lo hi
    }
