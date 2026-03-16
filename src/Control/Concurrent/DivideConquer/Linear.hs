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
import Control.Concurrent.DivideConquer.Utils.AtomicCounter.Linear (Counter)
import Control.Concurrent.DivideConquer.Utils.AtomicCounter.Linear qualified as Counter
import Control.Concurrent.DivideConquer.Utils.MQueue.Linear (newMQueue)
import Control.Concurrent.DivideConquer.Utils.MQueue.Linear qualified as MQ
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear (Sink, Source)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear qualified as Once
import Control.Functor.Linear (runState, runStateT)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Affine (Affine, GenericallyAffine (..))
import Control.Monad.Borrow.Pure.Internal
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce qualified as NonLinear
import Data.Coerce.Directed
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty.Linear qualified as NE
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
import Prelude qualified as P

data DivideConquer α t a r = DivideConquer
  { divide :: forall β. (β <= α) => Mut β a %1 -> BO β (Result β t a r)
  , conquer :: Conquer α t r
  }

data Result β t a r = Done r | Continue (t (Mut β a))

data Conquer α t r where
  NoOp :: Conquer α t ()

newtype ThreadId_ = ThreadId_ ThreadId
  deriving stock (GHC.Generic)
  deriving (Consumable, Dupable) via AsMovable ThreadId_

instance Movable ThreadId_ where
  move = Unsafe.toLinear Ur

wait :: Thread %1 -> BO α ()
wait (Thread tid source) = tid `lseq` Once.take source

data Thread = Thread !ThreadId_ !(Source ())
  deriving stock (GHC.Generic)

deriveGeneric ''Thread

deriving via Generically Thread instance Consumable Thread

{-
  -- NOTE: To handle conquer correctly, we need to be more careful
  -- with the dependency between tasks and easily deadlocks / starvation.
  --
  -- Meanwhile, qsort requires no postprocesses so we just handle No-Op.
  Conquer :: (forall β. (β <= α) => t r %1 -> BO β r) -> Conquer α t r
-}

data Switch = Switch {-# UNPACK #-} !Counter !(Maybe (Sink ()))

deriveGeneric ''Switch

deriving via Generically Switch instance Consumable Switch

unsafeDupSwitch :: Switch %1 -> (Switch, Switch)
unsafeDupSwitch = Unsafe.toLinear \(Switch ctr sink) ->
  dup ctr & \(ctr1, ctr2) -> (Switch ctr1 sink, Switch ctr2 sink)

unsafeDupSwitches :: NonEmpty Switch %1 -> (NonEmpty Switch, NonEmpty Switch)
unsafeDupSwitches = NE.unzip . NE.map unsafeDupSwitch

instance Dupable Switch where
  dup2 :: Switch %1 -> (Switch, Switch)
  dup2 = unsafeDupSwitch

-- | Returns 'True' if the counter reaches zero and the switch is released by this call.
release :: Switch %1 -> BO α Bool
release (Switch ctr sink) = Control.do
  (ctr, Ur i) <- Counter.decrement ctr
  if i P.<= 1
    then Control.do
      consume Control.<$> Data.mapM (`Once.put` ()) sink
      ctr `lseq` Control.pure True
    else ctr `lseq` sink `lseq` Control.pure False

releases :: NonEmpty Switch %1 -> BO α ()
releases = go . NE.toList
  where
    go :: [Switch] %1 -> BO α ()
    go [] = Control.pure ()
    go (s : ss) = Control.do
      released <- release s
      if released
        then go ss
        else ss `lseq` Control.pure ()

newChildSwitch :: Word -> Linearly %1 -> Switch
newChildSwitch n lin = Switch (Counter.withCapacity n lin) Nothing

data Work α a (t :: Type -> Type) (r :: Type) where
  Divide :: Mut α a %1 -> NonEmpty Switch %1 -> Work α a t ()

lengthT :: (Data.Traversable t) => t a -> (t a, Ur Word)
lengthT =
  flip runState (Ur 0)
    . Data.traverse (\x -> Control.state \s -> (x, Data.fmap (+ 1) s))

divideAndConquer ::
  forall α t a.
  (Data.Traversable t, Consumable (t ())) =>
  -- | The # of workers
  Int ->
  DivideConquer α t a () ->
  Mut α a %1 ->
  BO α (Mut α a)
divideAndConquer n DivideConquer {..} ini = DataFlow.do
  (lin, ini) <- withLinearly ini
  (lin, lin', lin'') <- dup3 lin
  uncurry lseq Control.<$> reborrowing' ini \(ini :: Mut γ a) ->
    someNatVal (fromIntegral n) & \(SomeNat (_ :: Proxy n)) -> Control.do
      (q, lend) <- newMQueue lin
      DataFlow.do
        (q, q') <- MQ.unsafeClone q
        (rootSink, rootSource) <- Once.new lin'
        qs <- MQ.unsafeCloneN @n q'
        Control.do
          rootCounter <- asksLinearly $ Counter.withCapacity 1
          q <- MQ.writeMQueue q $ Divide ini (Switch rootCounter (Just rootSink) :| [])
          chs <- concurrentMap lin'' worker qs
          case conquer of
            NoOp -> Control.do
              Once.take rootSource
              MQ.closeMQueue q
              -- Safety Note:
              -- 1. closing MQueue should make all workers eventually halt,
              -- 2. @chs@ are entirely allocated on GC heap
              -- Hence, leaking @chs@ here is not a problem.
              Control.pure $ Unsafe.toLinear (\_ -> ()) chs
          Control.pure (upcast @_ @(After _ ()) (consume Control.<$> reclaim' lend))
  where
    worker :: (β <= α) => Mut β (MQ.MQueue (Work β a t ())) %1 -> BO β ()
    worker q = Control.do
      whileJust_ q MQ.readMQueue \q -> \case
        Divide !inp !sinks -> Control.do
          resl <- divide inp
          case resl of
            Done () -> Control.do
              releases sinks
              Control.pure q
            Continue ts ->
              lengthT ts & \(ts, Ur n) -> Control.do
                sink <- asksLinearly $ newChildSwitch n
                (gomis, (q, ctr)) <- flip runStateT (q, sink NE.<| sinks) Control.do
                  Data.for ts \work -> Control.do
                    Control.StateT \(q, ctr) -> DataFlow.do
                      !(sws, sws') <- unsafeDupSwitches ctr
                      ((),) . (,sws) Control.<$> MQ.writeMQueue q (Divide work sws')
                ctr `lseq` gomis `lseq` Control.pure q
      Control.pure ()

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

-- Named with underscore, as it is not used for now
_killThreadBO :: Thread %1 -> BO α ()
_killThreadBO = Unsafe.toLinear \(Thread tid _) ->
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
  (Ord a, Copyable a) =>
  -- | The # of workers
  Int ->
  -- | Threshold for the length of vector to switch to sequential sort
  Int ->
  Mut α (LV.Vector a) %1 ->
  BO α (Mut α (LV.Vector a))
qsortDC nwork thresh = divideAndConquer nwork (qsortDC' thresh)

qsortDC' ::
  (Ord a, Copyable a) =>
  -- | Threshold for the length of vector to switch to sequential sort
  Int ->
  DivideConquer α Pair (LV.Vector a) ()
qsortDC' thresh =
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
                (Ur pivot, v) <- LV.copyAtMut i v
                (lo, hi) <- LV.divide pivot v 0 n
                Control.pure $ Continue $ Pair lo hi
    , conquer = NoOp
    }
