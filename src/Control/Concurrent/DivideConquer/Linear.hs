{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Control.Concurrent.DivideConquer.Linear (
  divideAndConquer,
  divideAndConquer',
  DivideConquer (..),
  Conquer (..),

  -- * Alternative naive and sequential implementations
  sequentialDivideAndConquer,
  sequentialDivideAndConquer',
  naiveDivideAndConquer,
  naiveDivideAndConquer',

  -- * Examples
  qsortDC,
  qsortDC',
  fftDC,
  fftDC',
) where

import Control.Applicative qualified as NonLinear
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear (Sink, Source)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear qualified as Once
import Control.Concurrent.DivideConquer.Utils.QueuePool (QueuePool, newQueuePool, popWork, pushWorkMaster, pushWorks)
import Control.Concurrent.DivideConquer.Utils.Semaphore (Semaphore)
import Control.Concurrent.DivideConquer.Utils.Semaphore qualified as Semaphore
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine (Affine, GenericallyAffine (..))
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Experimental.Borrows
import Control.Monad.Borrow.Pure.Experimental.Loop (iterReborrowing_)
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Data.Bifunctor.Linear qualified as BiL
import Data.Bits (bit, popCount, shiftR)
import Data.Complex (Complex (..))
import Data.Function (fix)
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.V.Linear (V)
import Data.V.Linear.Internal (V (..))
import Data.Vector qualified as V
import Data.Vector.Internal.Check (HasCallStack)
import Data.Vector.Mutable.Linear.Borrow qualified as LV
import GHC.Generics qualified as GHC
import GHC.TypeNats (SomeNat (..), someNatVal)
import Generics.Linear.TH (deriveGenericAnd1)
import Math.NumberTheory.Logarithms (intLog2)
import Prelude.Linear hiding (foldMap)
import Prelude.Linear.Generically (Generically, Generically1)
import System.Random (RandomGen)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear
import Prelude qualified as P

data DivideConquer c α t a r = DivideConquer
  { initialise :: forall β. (α >= β) => Mut β a %1 -> BO β (Ur c)
  , divide :: forall β. (α >= β) => c -> Mut β a %1 -> BO β (Result c β t a r)
  , conquer :: Conquer c α t a r
  }

data Conquer c α t a r where
  NoConquer :: Conquer c α t a ()
  Conquer :: (forall β. (α >= β) => c -> Mut β a %1 -> t r %1 -> BO β r) -> Conquer c α t a r

data Result c β t a r = Done !r | Continue !(t (Ur c, Mut β a))

data Switch r a
  = Switch
      {-# UNPACK #-} !(Semaphore a)
      !(Sink r)

release ::
  r %1 ->
  Switch r a %1 ->
  BO α (Maybe a)
release r (Switch sem dest) = Control.do
  Once.put dest r
  Semaphore.release sem

newRootSwitch :: BO α (Switch r (BO α ()), Source r)
newRootSwitch = Control.do
  (sink, source) <- asksLinearly Once.new
  sem <- Semaphore.newSemaphore $ Control.pure ()
  Control.pure (Switch sem sink, source)

data Work c α a (t :: Type -> Type) r where
  Process ::
    !c ->
    !(Mut α a) %1 ->
    !(Switch r (BO α ())) %1 ->
    Work c α a t r
  Resume ::
    !(BO α ()) %1 ->
    Work c α a t r

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

newtype QState c α a t r = Idle (Mut α (QueuePool (Work c α a t r)))

popQState ::
  QState c α a t r %1 ->
  BO α (Maybe (Work c α a t r, QState c α a t r))
popQState = \case
  Idle q -> Control.do
    Data.fmap (BiL.second Idle) Control.<$> popWork q

enqueues :: QState c α a t r %1 -> [Work c α a t r] %1 -> BO α (QState c α a t r)
enqueues q work = case q of
  Idle q -> Idle Control.<$> pushWorks q work

divideAndConquer ::
  forall c α β t a g.
  (Data.Traversable t, α >= β, RandomGen g) =>
  g ->
  -- | The # of workers.
  Int ->
  DivideConquer c α t a () ->
  Mut α a %1 ->
  BO β (Mut α a)
divideAndConquer g n dc = Control.fmap (uncurry lseq) . divideAndConquer' g n dc

divideAndConquer' ::
  forall c α β t a r g.
  (Data.Traversable t, α >= β, RandomGen g) =>
  g ->
  -- | The # of workers.
  Int ->
  DivideConquer c α t a r ->
  Mut α a %1 ->
  BO β (r, Mut α a)
divideAndConquer' g n DivideConquer {..} ini
  | n == 0 = error ("divideAndConquer: # of workers must be positive, but got: " <> show n) ini
  | otherwise =
      upcast @(BO _ (r, Mut _ a)) @(BO β (r, Mut α a)) $
        reborrowing' ini \(ini :: Mut γ a) ->
          someNatVal (fromIntegral n) & \(SomeNat (_ :: Proxy n)) -> Control.do
            (workers, master) <- newQueuePool @n g
            (masterQ, masterLend) <- asksLinearly $ borrow master
            (switch, rootSource) <- newRootSwitch
            (Ur c, ini) <- initialise <%~ ini

            Control.void $ pushWorkMaster masterQ $ Process c ini switch

            concurrentMap_ worker workers
            r <- Once.take rootSource

            Control.pure (upcast $ r Control.<$ reclaim' @γ masterLend)
  where
    worker :: (α >= α') => Mut α' (QueuePool (Work c α' a t r)) %1 -> BO α' ()
    worker q = Control.do
      whileJust_ (Idle q) popQState \q -> \case
        Resume k -> Control.do
          k
          Control.pure q
        -- NOTE: this leakage should be safe, because the finalization on ini'
        -- will only occur after all the subdivisions are processed.
        Process c ini switch ->
          Unsafe.toLinear (\a -> (a, a)) ini & \(ini, ini') -> Control.do
            resl <- divide c ini
            case resl of
              Done r -> Control.do
                cont <- release r switch
                ini' `lseq` case cont of
                  Nothing -> Control.pure q
                  Just k -> enqueues q [Resume k]
              Continue ts -> Control.do
                (sources, ks) <- Control.do
                  flip Control.runStateT mempty $
                    Data.for ts \work ->
                      Control.StateT \ks -> Control.do
                        (sink, source) <- asksLinearly Once.new
                        Control.pure (source, ks <> singletonD (work, sink))
                sem <- Semaphore.newSemaphore Control.do
                  case conquer of
                    NoConquer -> Control.do
                      cont <- release () switch
                      maybe (Control.pure ()) id cont
                      unsafeLeak sources `lseq` ini' `lseq` Control.pure ()
                    Conquer conq -> Control.do
                      rs <- Data.traverse Once.take sources
                      r <- conq c ini' rs
                      cont <- release r switch
                      maybe (Control.pure ()) id cont
                (tasks, sem) <- flip Control.runStateT sem $
                  Data.for (toListD ks) \((Ur c, work), sink) -> Control.StateT \sem -> Control.do
                    (sem, sem') <- Semaphore.retain sem
                    Control.pure (Process c work (Switch sem sink), sem')
                cont <- Semaphore.release sem
                case cont of
                  Nothing -> enqueues q tasks
                  Just k -> Control.do
                    unsafeLeak tasks `lseq` enqueues q [Resume k]

sequentialDivideAndConquer ::
  forall c α t a.
  (Data.Traversable t, Consumable (t ())) =>
  DivideConquer c α t a () ->
  Mut α a %1 ->
  BO α (Mut α a)
sequentialDivideAndConquer conq =
  Control.fmap (uncurry lseq) . sequentialDivideAndConquer' conq

sequentialDivideAndConquer' ::
  forall c α t a r.
  (Data.Traversable t, Consumable (t ())) =>
  DivideConquer c α t a r ->
  Mut α a %1 ->
  BO α (r, Mut α a)
sequentialDivideAndConquer' DivideConquer {..} ini = reborrowing ini \ini -> Control.do
  (Ur c, ini) <- initialise <%~ ini
  loop c ini
  where
    loop :: c -> Mut (γ /\ α) a %1 -> BO (γ /\ α) r
    loop c x = Control.do
      (resl, x) <- reborrowing x \x -> Control.do
        resl <- divide c (x)
        case resl of
          Done r -> Control.pure $ Left r
          Continue ts -> Control.do
            rs <- Data.traverse (\(Ur c, t) -> assocRBO $ loop c (assocBorrowL t)) ts
            Control.pure $ Right rs
      case resl of
        Left r -> x `lseq` Control.pure r
        Right rs -> case conquer of
          NoConquer -> Control.pure $ consume (x, rs)
          Conquer conq -> conq c x rs

newtype Par α a = Par (BO α a)
  deriving newtype (Data.Functor, Control.Functor)

runPar :: Par α a %1 -> BO α a
runPar = coerceLin
{-# INLINE runPar #-}

instance Data.Applicative (Par α) where
  pure = Par NonLinear.. Data.pure
  {-# INLINE pure #-}
  Par f <*> Par x = Par Control.do
    (f, x) <- parBO f x
    Control.pure $ f x

instance Control.Applicative (Par α) where
  pure = Par . Control.pure
  {-# INLINE pure #-}
  Par f <*> Par x = Par Control.do
    (f, x) <- parBO f x
    Control.pure $ f x

naiveDivideAndConquer ::
  forall c α t a.
  (Data.Traversable t, Consumable (t ())) =>
  DivideConquer c α t a () ->
  Mut α a %1 ->
  BO α (Mut α a)
naiveDivideAndConquer conq =
  Control.fmap (uncurry lseq) . naiveDivideAndConquer' conq

naiveDivideAndConquer' ::
  forall c α t a r.
  (Data.Traversable t, Consumable (t ())) =>
  DivideConquer c α t a r ->
  Mut α a %1 ->
  BO α (r, Mut α a)
naiveDivideAndConquer' DivideConquer {..} ini = reborrowing ini \ini -> Control.do
  (Ur c, ini) <- initialise <%~ ini
  loop c ini
  where
    loop :: c -> Mut (γ /\ α) a %1 -> BO (γ /\ α) r
    loop c x = Control.do
      (resl, x) <- reborrowing x \x -> Control.do
        resl <- divide c (x)
        case resl of
          Done r -> Control.pure $ Left r
          Continue ts -> Control.do
            rs <- runPar $ Data.traverse (\(Ur c, t) -> Par $ assocRBO $ loop c (assocBorrowL t)) ts
            Control.pure $ Right rs
      case resl of
        Left r -> x `lseq` Control.pure r
        Right rs -> case conquer of
          NoConquer -> Control.pure $ consume (x, rs)
          Conquer conq -> conq c x rs

unsafeLeak :: a %1 -> ()
{-# NOINLINE unsafeLeak #-}
unsafeLeak = Unsafe.toLinear \ !_ -> ()

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
  DivideConquer () α Pair (LV.Vector a) ()
qsortDC' thresh =
  DivideConquer
    { initialise = Control.pure . move . consume
    , divide = \_ vs ->
        case LV.size vs of
          (Ur n, v)
            | n <= 1 ->
                v `lseq` Control.pure $ Done ()
            | n <= thresh -> Control.do
                !() <- LV.qsort 0 v
                Control.pure $ Done ()
            | otherwise -> Control.do
                let i = n `quot` 2
                (Ur pivot, v) <- LV.copyAtMut i v
                (lo, hi) <- LV.divide pivot v 0 n
                Control.pure $ Continue $ Pair (Ur (), lo) (Ur (), hi)
    , conquer = NoConquer
    }

data FftCoe = FftCoe
  { cosθ :: {-# UNPACK #-} !Double
  , sinθ :: {-# UNPACK #-} !Double
  , size :: {-# UNPACK #-} !Int
  }
  deriving (Show)

fftDC ::
  (α >= β, RandomGen g, HasCallStack) =>
  g ->
  -- | The # of workers.
  Int ->
  -- | Threshold for the length of vector to switch to sequential sort.
  Int ->
  Mut α (LV.Vector (Complex Double)) %1 ->
  BO β (Mut α (LV.Vector (Complex Double)))
fftDC g nwork thresh vec =
  case LV.size vec of
    (Ur n, vec)
      | popCount n /= 1 -> vec `lseq` error ("fftDC: the length " <> show n <> " of vector must be a power of 2")
      | otherwise -> divideAndConquer g nwork (fftDC' thresh) vec

fftDC' ::
  -- | Threshold for the length of vector to switch to sequential FFT.
  Int ->
  DivideConquer FftCoe α Pair (LV.Vector (Complex Double)) ()
fftDC' thresh =
  DivideConquer
    { initialise = \array ->
        case LV.size array of
          (Ur n, array) -> Control.do
            Control.void $ reverseBit array
            Control.pure $
              Ur
                FftCoe
                  { cosθ = cos (2 * pi / fromIntegral n)
                  , sinθ = sin (2 * pi / fromIntegral n)
                  , size = n
                  }
    , divide = \coe@FftCoe {..} vs ->
        if
          | size <= 1 ->
              vs `lseq` Control.pure $ Done ()
          | size <= thresh -> Done () Control.<$ sequential coe vs
          | otherwise -> Control.do
              (Ur coe, lo, hi) <- step coe vs
              Control.pure $ Continue $ Pair (Ur coe, lo) (Ur coe, hi)
    , conquer = Conquer $ \coe vs l -> l `lseq` combine coe vs
    }
  where
    step ::
      FftCoe ->
      Mut α (LV.Vector (Complex Double)) %1 ->
      BO
        α
        ( Ur FftCoe
        , Mut α (LV.Vector (Complex Double))
        , Mut α (LV.Vector (Complex Double))
        )
    step FftCoe {..} vs = Control.do
      let !half = size `quot` 2
          !dblCs = 2 * cosθ * cosθ - 1
          !dblSn = 2 * sinθ * cosθ
          !coe' = FftCoe {cosθ = dblCs, sinθ = dblSn, size = half}
          %1 !(lo, hi) = LV.splitAt half vs
      Control.pure (Ur coe', lo, hi)

    sequential ::
      FftCoe ->
      Mut α (LV.Vector (Complex Double)) %1 ->
      BO α ()
    sequential coe vs = case LV.size vs of
      (Ur i, vs)
        | i <= 1 -> Control.pure $ consume vs
        | otherwise -> Control.do
            vs <- reborrowing_ vs \vs -> Control.do
              (Ur coe', lo, hi) <- step coe vs
              sequential coe' lo
              sequential coe' hi
            combine coe vs

    combine :: FftCoe -> Mut β (LV.Vector (Complex Double)) %1 -> BO β ()
    combine FftCoe {..} vs = Control.do
      let !half = size `quot` 2
          !kW = cosθ :+ sinθ
      go half kW 0 1 vs
      where
        go !half !kW !k !w vs
          | k >= half = Control.pure $ consume vs
          | otherwise = Control.do
              (Ur ek, vs) <- LV.copyAtMut k vs
              (Ur ok, vs) <- LV.copyAtMut (half + k) vs
              let !t = w P.* ok
              (lr :+ li, vs) <- LV.set k (ek P.+ t) vs
              (rr :+ ri, vs) <- LV.set (half + k) (ek P.- t) vs
              consume (lr, li, rr, ri) `lseq` go half kW (k + 1) (w P.* kW) vs

reverseBit ::
  forall α a.
  Mut α (LV.Vector a) %1 ->
  BO α ()
reverseBit v =
  LV.size v & \(Ur len, v) -> Control.do
    let !n = intLog2 len
        !m = bit $ n `shiftR` 1
    consume Control.<$> reborrowing' v \v -> Control.do
      (table, lend) <- borrowLinearlyM $ LV.constant m 0
      table <- buildTable n <%= table
      Control.void $
        iterReborrowing_ (m - 1) (table :- v :- BNil) $
          \((+ 1) -> !i) (table :- v :- BNil) -> Control.do
            (Ur iOff, table) <- LV.copyAtMut i table
            Control.void $
              iterReborrowing_ i (table :- v :- BNil) \j (table :- v :- BNil) -> Control.do
                (Ur jOff, table) <- LV.copyAtMut j table
                let !ji = j + iOff
                    !ij = i + jOff
                v <- LV.swap v ji ij
                if even n
                  then Control.pure $ v `lseq` consume table
                  else consume . (,table) Control.<$> LV.swap v (ji + m) (ij + m)

      Control.pure $ upcast @_ @(After _ ()) $ consume . LV.toList Control.<$> reclaim' lend
  where
    buildTable ::
      Int ->
      Mut β (LV.Vector Int) %1 ->
      BO β ()
    buildTable n table =
      fix
        ( \loop !pk !pl table ->
            if pl + 1 >= pk
              then Control.pure $ consume table
              else Control.do
                let !k = bit $ pk - 1
                    !l = bit pl
                table <- iterReborrowing_ l table \j table -> Control.do
                  (Ur t, table) <- LV.copyAtMut j table
                  consume Control.<$> LV.set (l + j) (t + k) table
                loop (pk - 1) (pl + 1) table
        )
        n
        0
        table
