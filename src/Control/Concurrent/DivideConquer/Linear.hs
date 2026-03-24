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

import Control.Applicative qualified as NonLinear
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear (Sink, Source)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear qualified as Once
import Control.Concurrent.DivideConquer.Utils.QueuePool (QueuePool, newQueuePool, popWork, pushWork, pushWorkMaster)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Affine (Affine, GenericallyAffine (..))
import Control.Monad.Borrow.Pure.Internal
import Data.Coerce.Directed
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.List.Linear qualified as LL
import Data.List.NonEmpty.Linear (NonEmpty (..))
import Data.List.NonEmpty.Linear qualified as NEL
import Data.Proxy (Proxy (..))
import Data.V.Linear (V)
import Data.V.Linear.Internal (V (..))
import Data.Vector qualified as V
import Data.Vector.Mutable.Linear.Borrow qualified as LV
import GHC.Exts qualified as GHC
import GHC.Generics qualified as GHC
import GHC.TypeNats (SomeNat (..), someNatVal)
import Generics.Linear.TH (deriveGenericAnd1)
import Prelude.Linear
import Prelude.Linear.Generically (Generically, Generically1)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (RandomGen)
import Unsafe.Linear qualified as Unsafe

data DivideConquer α t a = DivideConquer
  { divide :: forall β. (β <= α) => Mut β a %1 -> BO β (Result β t a)
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
  DList f <> DList g = DList (f . g)
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

-- TODO: perhaps we can use atomic counter here again?

data QState α a t
  = Idle !(Mut α (QueuePool (Work α a t)))
  | DoThen !(Work α a t) !(Mut α (QueuePool (Work α a t)))

popQState ::
  QState α a t %1 ->
  BO α (Maybe (Work α a t, QState α a t))
popQState = \case
  Idle q -> Control.do
    m <- popWork q
    case m of
      Nothing -> Control.pure Nothing
      Just (work, q) -> Control.pure (Just (work, Idle q))
  DoThen work q -> Control.pure $ Just (work, Idle q)

enqueue :: QState α a t %1 -> Work α a t %1 -> BO α (QState α a t)
enqueue q work = case q of
  Idle q -> Idle Control.<$> pushWork q work
  DoThen work' q -> error "Could not happen!" work q work'

doAndEnqueue :: QState α a t %1 -> Work α a t %1 -> Work α a t %1 -> BO α (QState α a t)
doAndEnqueue q work cont = case q of
  Idle q -> DoThen work Control.<$> pushWork q cont
  DoThen work' q -> error "Could not happen!" work cont work' q

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
      whileJust_ (Idle q) popQState \q -> \case
        Final -> Control.pure q
        Process ini sink next -> Control.do
          q <- enqueue q next
          resl <- divide ini
          case resl of
            Done -> Control.do
              Once.put sink ()
              Control.pure q
            Continue ts -> Control.do
              (sources, ks) <-
                flip Control.runStateT mempty $ Data.for ts \work -> Control.do
                  (sink, source) <- Control.lift $ asksLinearly Once.new
                  Control.modify (<> singletonD (work, sink))
                  Control.pure source
              let %1 !cont = Unite sources sink
              case NEL.nonEmpty $ toListD ks of
                Nothing -> enqueue q cont
                Just ((ini, sink) :| ks) ->
                  doAndEnqueue
                    q
                    (Process ini sink Final)
                    $ LL.foldr (uncurry Process) cont ks
        Unite children sink -> Control.do
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
