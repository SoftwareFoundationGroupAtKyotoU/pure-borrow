{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Divide-and-conquer algorithms for linearly owned vectors of unrestricted
elements.
-}
module Control.Concurrent.DivideConquer.Linear.Unrestricted (
  qsort,
  qsortDC,
  qsortDC',
  fftDC,
  fftDC',
) where

import Control.Concurrent.DivideConquer.Linear (
  Conquer (..),
  DivideConquer (..),
  divideAndConquer,
 )
import Control.Concurrent.DivideConquer.Linear.Types.Internal (Result (..))
import Control.Concurrent.DivideConquer.Linear.Unrestricted.Internal
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Experimental.Borrows
import Control.Monad.Borrow.Pure.Experimental.Loop (iterReborrowing_)
import Data.Bits (bit, popCount, shiftR)
import Data.Complex (Complex (..))
import Data.Function (fix)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Vector
import Data.Vector.Mutable.Linear.Borrow qualified as Owning
import GHC.Stack (HasCallStack)
import Math.NumberTheory.Logarithms (intLog2)
import Prelude.Linear
import System.Random (RandomGen)
import Prelude qualified as NonLinear

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
              (nextBudget NonLinear.> 0)
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
            borrowLinearlyM (Owning.constant middle 0)
          table <- buildTable bits <%= table
          Control.void $
            iterReborrowing_
              (middle - 1)
              (table :- shorter :- BNil)
              \((+ 1) -> !first) (table :- current :- BNil) -> Control.do
                (Ur firstOffset, table) <-
                  Owning.copyAtMut first table
                Control.void $
                  iterReborrowing_
                    first
                    (table :- current :- BNil)
                    \second (table :- current :- BNil) -> Control.do
                      (Ur secondOffset, table) <-
                        Owning.copyAtMut second table
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
              consume . Owning.toList
                Control.<$> reclaim' lend
  where
    buildTable ::
      Int ->
      Mut β (Owning.Vector Int) %1 ->
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
                      Owning.copyAtMut index table
                    consume
                      Control.<$> Owning.set
                        (lowBit + index)
                        (value + highBit)
                        table
                loop (high - 1) (low + 1) table
        )
        bits
        0
        table
