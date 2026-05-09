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
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear (Sink, Source)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear qualified as Once
import Control.Concurrent.DivideConquer.Utils.QueuePool (QueuePool, newQueuePool, popWork, pushWork, pushWorkMaster, pushWorks)
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
import Debug.Trace (traceEventIO)
import GHC.Exts qualified as GHC
import GHC.Generics qualified as GHC
import GHC.TypeNats (SomeNat (..), someNatVal)
import Generics.Linear.TH (deriveGenericAnd1)
import Prelude.Linear hiding (foldMap)
import Prelude.Linear.Generically (Generically, Generically1)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Linear qualified as Unsafe

data DivideConquer α t a = DivideConquer
  { divide :: forall β. (α >= β) => Mut β a %1 -> BO β (Result β t a)
  }

data Result β t a = Done | Continue (t (Mut β a))

data Work α a (t :: Type -> Type) where
  Process :: Mut α a %1 -> Sink () %1 -> Work α a t %1 -> Work α a t
  Unite :: t (Source ()) %1 -> Sink () %1 -> Work α a t
  Final :: Work α a t

newtype Thread = Thread ThreadId

instance Consumable Thread where
  {-# NOINLINE consume #-}
  consume = GHC.noinline $ Unsafe.toLinear \(Thread tid) -> unsafePerformIO $ do
    killThread tid

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

data QState α a t
  = Next !(Work α a t) !(Mut α (QueuePool (Work α a t)))
  | Idle !(Mut α (QueuePool (Work α a t)))

popQState ::
  QState α a t %1 ->
  BO α (Maybe (Work α a t, QState α a t))
popQState = \case
  Idle q -> Control.do
    unsafeSystemIOToBO $ traceEventIO "WORK[S]: popping work from queue..."
    Data.fmap (BiL.second Idle) Control.<$> popWork q
  Next work q -> Control.do
    unsafeSystemIOToBO $ traceEventIO "WORK[S]: using the next direct work."
    Control.pure $ Just (work, Idle q)

enqueue :: QState α a t %1 -> Work α a t %1 -> BO α (QState α a t)
enqueue q work = case q of
  Idle q -> Control.pure $ Next work q
  Next next0 q -> Next work Control.<$> pushWork q next0

doAndEnqueue ::
  QState α a t %1 ->
  Work α a t %1 ->
  Work α a t %1 ->
  BO α (QState α a t)
doAndEnqueue q next cont = case q of
  Idle q -> Control.do
    unsafeSystemIOToBO $ traceEventIO "WORK[D]: Idle. Adding direct next task."
    Next next Control.<$> pushWork q cont
  Next next0 q -> Control.do
    unsafeSystemIOToBO $ traceEventIO "WORK[D]: Already full. pushing forward"
    Next next Control.<$> pushWorks q [cont, next0]

data P a where
  P :: {-# UNPACK #-} !Int -> !a %1 -> P a

divideAndConquer ::
  forall α β t a.
  (Data.Traversable t, Consumable (t ()), α >= β) =>
  -- | The # of workers.
  Int ->
  DivideConquer α t a ->
  Mut α a %1 ->
  BO β (Mut α a)
divideAndConquer n DivideConquer {..} ini
  | n == 0 = error ("divideAndConquer: # of workers must be positive, but got: " <> show n) ini
  | otherwise =
      upcast $
        uncurry (lseq @()) Control.<$> reborrowing' ini \(ini :: Mut γ a) ->
          someNatVal (fromIntegral n) & \(SomeNat (_ :: Proxy n)) -> Control.do
            (workers, master) <- newQueuePool @n
            (masterQ, masterLend) <- asksLinearly $ borrow master
            (rootSink, rootSource) <- asksLinearly Once.new

            Control.void $ pushWorkMaster masterQ $ Process ini rootSink Final

            concurrentMap_ worker workers
            Once.take rootSource

            Control.pure (upcast $ consume Control.<$> reclaim' masterLend)
  where
    worker :: (α >= α') => Mut α' (QueuePool (Work α' a t)) %1 -> BO α' ()
    worker q =
      whileJust_ (Idle q) popQState \q -> \case
        Final -> Control.do
          unsafeSystemIOToBO $ traceEventIO "WORK[WF]: Final work reached."
          Control.pure q
        Process ini sink next -> Control.do
          q <- enqueue q next
          resl <- divide ini
          case resl of
            Done -> Control.do
              unsafeSystemIOToBO $ traceEventIO "WORK[WP]: No more division needed. Return"
              Once.put sink ()
              Control.pure q
            Continue ts -> Control.do
              unsafeSystemIOToBO $ traceEventIO "WORK[WP]: Division occurred."
              (sources, P num ks) <-
                flip Control.runStateT (P 0 mempty) $ Data.for ts \work -> Control.StateT \(P num ks) -> Control.do
                  (sink, source) <- asksLinearly Once.new
                  let %1 !ks' = ks <> singletonD (work, sink)
                  Control.pure (source, P (num + 1) ks')
              let %1 !(ls, rs) = splitAt (num `quot` 2) $ toListD ks
              doAndEnqueue
                q
                (foldr (uncurry Process) Final ls)
                (foldr (uncurry Process) (Unite sources sink) rs)
        Unite children sink -> Control.do
          unsafeSystemIOToBO $ traceEventIO "WORK[WU]: Unite work reached."
          Control.void $ Data.traverse Once.take children
          Once.put sink ()
          Control.pure q

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
  (Ord a, Copyable a, α >= β) =>
  -- | The # of workers.
  Int ->
  -- | Threshold for the length of vector to switch to sequential sort.
  Int ->
  Mut α (LV.Vector a) %1 ->
  BO β (Mut α (LV.Vector a))
qsortDC nwork thresh = divideAndConquer nwork (qsortDC' thresh)

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
            | n <= thresh ->
                Done Control.<$ LV.qsort 0 v
            | otherwise -> Control.do
                let i = n `quot` 2
                (Ur pivot, v) <- LV.copyAtMut i v
                (lo, hi) <- LV.divide pivot v 0 n
                Control.pure $ Continue $ Pair lo hi
    }
