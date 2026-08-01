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
  qsort,
  qsortDC,
  qsortDC',
  fftDC,
  fftDC',

  -- ** Example internals
  -- $example-internals
  Pair (..),
  FftCoe (..),
  combineLoop,
) where

import Control.Applicative qualified as NonLinear
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.DivideConquer.Linear.Internal (combineLoop)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear (Sink, Source)
import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear qualified as Once
import Control.Concurrent.DivideConquer.Utils.QueuePool (QueuePool, newQueuePool, popWork, pushWorkMaster, pushWorks)
import Control.Concurrent.DivideConquer.Utils.Semaphore (Semaphore)
import Control.Concurrent.DivideConquer.Utils.Semaphore qualified as Semaphore
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine (Affine, GenericallyAffine (..))
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Copyable ()
import Control.Monad.Borrow.Pure.Experimental.Borrows
import Control.Monad.Borrow.Pure.Experimental.Loop (iterReborrowing_)
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
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Vector
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
import Prelude qualified as P

{- $example-internals

The worked examples above expose these because they appear in their own
signatures: 'fftDC'' returns a @'DivideConquer' 'FftCoe' α 'Pair' …@, so a caller
cannot so much as write its type without them.
'combineLoop' is the FFT butterfly, exposed so that a benchmark or
inspection test can specialize it at a concrete backend.
-}

data Result c β t a r = Done !r | Continue !(t (Ur c, Mut β a))

data DivideConquer c α t a r = DivideConquer
  { initialise :: forall β. (α >= β) => Mut β a %1 -> BO β (Ur c)
  , divide :: forall β. (α >= β) => c -> Mut β a %1 -> BO β (Result c β t a r)
  , conquer :: Conquer c α t a r
  }

data Conquer c α t a r where
  NoConquer :: Conquer c α t a ()
  Conquer :: (forall β. (α >= β) => c -> Mut β a %1 -> t r %1 -> BO β r) -> Conquer c α t a r

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

data FftCoe = FftCoe
  { cosθ :: {-# UNPACK #-} !Double
  , sinθ :: {-# UNPACK #-} !Double
  , size :: {-# UNPACK #-} !Int
  }
  deriving (Show)

{- | Sort a vector, optionally using nested parallelism.

A zero budget is sequential. At every recursive split a positive budget is
halved, bounding the depth at which 'parBO' is used.
-}
qsort ::
  forall v a α β.
  (G.Vector v a, Ord a, α >= β) =>
  Word ->
  Mut α (Vector.Vector v a) %1 ->
  BO β ()
{-# INLINEABLE qsort #-}
qsort = go
  where
    go ::
      Word ->
      Mut α (Vector.Vector v a) %1 ->
      BO β ()
    go budget vector =
      case Vector.size vector of
        (Ur 0, vector) -> Control.pure (consume vector)
        (Ur 1, vector) -> Control.pure (consume vector)
        (Ur length_, vector) -> Control.do
          let pivotIndex = length_ `quot` 2
          (Ur pivot, vector) <-
            Vector.unsafeGet pivotIndex vector
          (lower, upper) <-
            partitionVector pivot vector 0 length_
          let nextBudget = budget `quot` 2
          Control.void $
            parIf
              (nextBudget P.> 0)
              (go nextBudget lower)
              (go nextBudget upper)

partitionVector ::
  (G.Vector v a, Ord a, α >= β) =>
  a ->
  Mut α (Vector.Vector v a) %1 ->
  Int ->
  Int ->
  BO
    β
    ( Mut α (Vector.Vector v a)
    , Mut α (Vector.Vector v a)
    )
{-# INLINEABLE partitionVector #-}
partitionVector pivot = partitionUp
  where
    partitionUp vector lower upper
      | lower < upper = Control.do
          (Ur element, vector) <-
            Vector.unsafeGet lower vector
          if element < pivot
            then partitionUp vector (lower + 1) upper
            else partitionDown vector lower (upper - 1)
      | otherwise =
          Control.pure (Vector.splitAt lower vector)

    partitionDown vector lower upper
      | lower < upper = Control.do
          (Ur element, vector) <-
            Vector.unsafeGet upper vector
          if pivot < element
            then partitionDown vector lower (upper - 1)
            else Control.do
              vector <-
                Vector.unsafeSwap vector lower upper
              partitionUp vector (lower + 1) upper
      | otherwise =
          Control.pure (Vector.splitAt lower vector)

parIf :: Bool %1 -> BO α a %1 -> BO α b %1 -> BO α (a, b)
{-# INLINE parIf #-}
parIf condition =
  if condition
    then parBO
    else Control.liftA2 (,)

{- | Sort a vector with the work-sharing scheduler.

The worker count must be positive. Subvectors no longer than the threshold are
sorted sequentially.
-}
qsortDC ::
  (G.Vector v a, Ord a, α >= β, RandomGen g) =>
  g ->
  Int ->
  Int ->
  Mut α (Vector.Vector v a) %1 ->
  BO β (Mut α (Vector.Vector v a))
{-# INLINE qsortDC #-}
qsortDC generator workers threshold =
  divideAndConquer
    generator
    workers
    (qsortDC' threshold)

-- | Construct a quicksort workload with the given sequential cutoff.
qsortDC' ::
  (G.Vector v a, Ord a) =>
  Int ->
  DivideConquer
    ()
    α
    Pair
    (Vector.Vector v a)
    ()
{-# INLINEABLE qsortDC' #-}
qsortDC' threshold =
  DivideConquer
    { initialise = Control.pure . move . consume
    , divide = \_ vector ->
        case Vector.size vector of
          (Ur length_, vector)
            | length_ <= 1 ->
                vector `lseq` Control.pure (Done ())
            | length_ <= threshold -> Control.do
                !() <- qsort 0 vector
                Control.pure (Done ())
            | otherwise -> Control.do
                let pivotIndex = length_ `quot` 2
                (Ur pivot, vector) <-
                  Vector.unsafeGet pivotIndex vector
                (lower, upper) <-
                  partitionVector pivot vector 0 length_
                Control.pure $
                  Continue $
                    Pair
                      (Ur (), lower)
                      (Ur (), upper)
    , conquer = NoConquer
    }

{- | Transform a power-of-two vector with the work-sharing scheduler.

The worker count must be positive. The vector length is checked here and must
be a power of two. Subvectors no longer than the threshold are transformed
sequentially.
-}
fftDC ::
  ( G.Vector v (Complex Double)
  , α >= β
  , RandomGen g
  , HasCallStack
  ) =>
  g ->
  Int ->
  Int ->
  Mut α (Vector.Vector v (Complex Double)) %1 ->
  BO β (Mut α (Vector.Vector v (Complex Double)))
{-# INLINE fftDC #-}
fftDC generator workers threshold vector =
  case Vector.size vector of
    (Ur length_, vector)
      | popCount length_ /= 1 ->
          vector `lseq`
            error
              ( "fftDC: the length "
                  <> show length_
                  <> " of vector must be a power of 2"
              )
      | otherwise ->
          divideAndConquer
            generator
            workers
            (fftDC' threshold)
            vector

{- | Construct an FFT workload with the given sequential cutoff.

This lower-level constructor does not validate the input length. Every vector
run with the returned workload must have power-of-two length; use 'fftDC' when
that check should be performed by the API.
-}
fftDC' ::
  forall v α.
  (G.Vector v (Complex Double)) =>
  Int ->
  DivideConquer
    FftCoe
    α
    Pair
    (Vector.Vector v (Complex Double))
    ()
{-# INLINEABLE fftDC' #-}
fftDC' threshold =
  DivideConquer
    { initialise = \array ->
        case Vector.size array of
          (Ur length_, array) -> Control.do
            Control.void (reverseBit array)
            Control.pure $
              Ur
                FftCoe
                  { cosθ =
                      cos
                        (2 * pi / fromIntegral length_)
                  , sinθ =
                      sin
                        (2 * pi / fromIntegral length_)
                  , size = length_
                  }
    , divide = \coefficient@FftCoe {..} vector ->
        if
          | size <= 1 ->
              vector `lseq` Control.pure (Done ())
          | size <= threshold ->
              Done ()
                Control.<$ sequential coefficient vector
          | otherwise -> Control.do
              (Ur nextCoefficient, lower, upper) <-
                step coefficient vector
              Control.pure $
                Continue $
                  Pair
                    (Ur nextCoefficient, lower)
                    (Ur nextCoefficient, upper)
    , conquer =
        Conquer \coefficient vector results ->
          results `lseq` combine coefficient vector
    }
  where
    step ::
      FftCoe ->
      Mut β (Vector.Vector v (Complex Double)) %1 ->
      BO
        β
        ( Ur FftCoe
        , Mut β (Vector.Vector v (Complex Double))
        , Mut β (Vector.Vector v (Complex Double))
        )
    step FftCoe {..} vector = Control.do
      let !half = size `quot` 2
          !doubleCosine =
            2 * cosθ * cosθ - 1
          !doubleSine =
            2 * sinθ * cosθ
          !nextCoefficient =
            FftCoe
              { cosθ = doubleCosine
              , sinθ = doubleSine
              , size = half
              }
          %1 !(lower, upper) =
            Vector.splitAt half vector
      Control.pure
        (Ur nextCoefficient, lower, upper)

    sequential ::
      FftCoe ->
      Mut β (Vector.Vector v (Complex Double)) %1 ->
      BO β ()
    sequential coefficient vector =
      case Vector.size vector of
        (Ur length_, vector)
          | length_ <= 1 ->
              Control.pure (consume vector)
          | otherwise -> Control.do
              vector <-
                reborrowing_ vector \shorter -> Control.do
                  (Ur nextCoefficient, lower, upper) <-
                    step coefficient shorter
                  sequential nextCoefficient lower
                  sequential nextCoefficient upper
              combine coefficient vector

    combine ::
      FftCoe ->
      Mut β (Vector.Vector v (Complex Double)) %1 ->
      BO β ()
    combine FftCoe {..} vector = Control.do
      let !half = size `quot` 2
          !root = cosθ :+ sinθ
      combineLoop half root 0 1 vector

reverseBit ::
  forall v a α.
  (G.Vector v a) =>
  Mut α (Vector.Vector v a) %1 ->
  BO α ()
{-# INLINEABLE reverseBit #-}
reverseBit vector =
  Vector.size vector
    & \(Ur length_, vector) -> Control.do
      let !bits = intLog2 length_
          !middle = bit (bits `shiftR` 1)
      consume
        Control.<$> reborrowing' vector \shorter -> Control.do
          (table, lend) <-
            borrowLinearlyM (LV.constant middle 0)
          table <- buildTable bits <%= table
          Control.void $
            iterReborrowing_
              (middle - 1)
              (table :- shorter :- BNil)
              \((+ 1) -> !first) (table :- current :- BNil) -> Control.do
                (Ur firstOffset, table) <-
                  LV.copyAtMut first table
                Control.void $
                  iterReborrowing_
                    first
                    (table :- current :- BNil)
                    \second (table :- current :- BNil) -> Control.do
                      (Ur secondOffset, table) <-
                        LV.copyAtMut second table
                      let !forward =
                            second + firstOffset
                          !backward =
                            first + secondOffset
                      current <-
                        Vector.unsafeSwap
                          current
                          forward
                          backward
                      if even bits
                        then
                          Control.pure $
                            current `lseq`
                              consume table
                        else
                          consume . (,table)
                            Control.<$> Vector.unsafeSwap
                              current
                              (forward + middle)
                              (backward + middle)

          Control.pure $
            upcast @_ @(After _ ()) $
              consume . LV.toList
                Control.<$> reclaim' lend
  where
    buildTable ::
      Int ->
      Mut β (LV.Vector Int) %1 ->
      BO β ()
    buildTable bits table =
      fix
        ( \loop !high !low table ->
            if low + 1 >= high
              then Control.pure (consume table)
              else Control.do
                let !highBit = bit (high - 1)
                    !lowBit = bit low
                table <-
                  iterReborrowing_ lowBit table \index table -> Control.do
                    (Ur value, table) <-
                      LV.copyAtMut index table
                    consume
                      Control.<$> LV.set
                        (lowBit + index)
                        (value + highBit)
                        table
                loop (high - 1) (low + 1) table
        )
        bits
        0
        table
