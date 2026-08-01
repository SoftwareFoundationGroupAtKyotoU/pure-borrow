{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PureBorrow.Internal.Bench.Unboxed (
  defaultMain,
  benches,
  directFixedUnboxedKernel,
  directFixedUnboxedMaterialization,
  directGrowableUnboxedGrowthKernel,
  directGrowableUnboxedMaterialization,
  directGrowableUnboxedNoGrowthKernel,
  owningBoxedQsortKernel,
  pureBorrowFixedUnboxedKernel,
  pureBorrowFixedUnboxedMaterialization,
  pureBorrowGrowableUnboxedGrowthKernel,
  pureBorrowGrowableUnboxedMaterialization,
  pureBorrowGrowableUnboxedNoGrowthKernel,
  unrestrictedBoxedFftKernel,
  unrestrictedBoxedQsortKernel,
  unrestrictedUnboxedFftKernel,
  unrestrictedUnboxedQsortKernel,
) where

import Control.Concurrent.DivideConquer.Linear qualified as DC
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.ST.Strict (ST, runST)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Complex (Complex (..))
import Data.Ref.Linear qualified as Ref
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector qualified as B
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as UnrestrictedVector
import Data.Vector.Mutable.Linear.Borrow qualified as OwningVector
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow qualified as Growable
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow.Internal qualified as GrowableInternal
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as Vector
import Data.Vector.Unboxed.Mutable.Linear.Borrow.Internal qualified as VectorInternal
import GHC.IO (unsafePerformIO)
import Prelude.Linear
import Test.Tasty.Bench hiding (defaultMain)
import Test.Tasty.Bench qualified as Bench
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

directFixedUnboxedKernel :: U.Vector Int -> U.Vector Int
{-# NOINLINE directFixedUnboxedKernel #-}
directFixedUnboxedKernel input =
  U.modify (\vector -> directWorker (UM.length vector) 0 vector) input

directWorker :: Int -> Int -> UM.MVector s Int -> ST s ()
{-# NOINLINE directWorker #-}
directWorker !length_ !index vector
  | index >= length_ = NonLinear.pure ()
  | otherwise = do
      value <- UM.unsafeRead vector index
      UM.unsafeWrite vector index (value + 1)
      directWorker length_ (index + 1) vector

{- | Benchmark-only monomorphic finalization for the fixed-vector kernels.

For 'Int', 'move' is observationally the identity. Bypassing the public
materialization loop isolates the update worker; this is not an API-performance
root.
-}
unsafeBenchmarkFreezeFixedInt :: Vector.Vector Int %1 -> Ur (U.Vector Int)
{-# NOINLINE unsafeBenchmarkFreezeFixedInt #-}
unsafeBenchmarkFreezeFixedInt =
  Unsafe.toLinear \(VectorInternal.Vector vector) ->
    Ur (unsafePerformIO (U.unsafeFreeze vector))

directFixedUnboxedMaterialization :: U.Vector Int -> U.Vector Int
{-# NOINLINE directFixedUnboxedMaterialization #-}
directFixedUnboxedMaterialization input =
  U.modify (\_ -> NonLinear.pure ()) input

pureBorrowFixedUnboxedMaterialization :: U.Vector Int -> U.Vector Int
{-# NOINLINE pureBorrowFixedUnboxedMaterialization #-}
pureBorrowFixedUnboxedMaterialization input =
  unur $
    linearly \linear ->
      Vector.toVector (Vector.fromVector input linear)

pureBorrowFixedUnboxedKernel :: U.Vector Int -> U.Vector Int
{-# NOINLINE pureBorrowFixedUnboxedKernel #-}
pureBorrowFixedUnboxedKernel input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Vector.fromVector input ownerLinear)
      pureBorrowWorker (U.length input) 0 vector
      pureAfter (unsafeBenchmarkFreezeFixedInt (reclaim lend))

directGrowableUnboxedNoGrowthKernel :: U.Vector Int -> U.Vector Int
{-# NOINLINE directGrowableUnboxedNoGrowthKernel #-}
directGrowableUnboxedNoGrowthKernel input =
  U.modify
    ( \vector -> do
        -- The header ref mirrors the growable owner's indirection, so the
        -- baseline pays for the same loads the measured variant does.
        header <- newSTRef (U.length input, vector)
        (_, content) <- readSTRef header
        directWorker (UM.length content) 0 content
        -- Match the growable owner's final header access.
        (_, _) <- readSTRef header
        NonLinear.pure ()
    )
    input

{- | Benchmark-only monomorphic finalization for the growable-vector kernels.

The reclaimed owner is consumed exactly once and only its initialized prefix is
frozen. As with 'unsafeBenchmarkFreezeFixedInt', this isolates the kernel and
must not be interpreted as public materialization cost.
-}
unsafeBenchmarkFreezeGrowableInt ::
  Growable.GrowableVector Int %1 ->
  Ur (U.Vector Int)
{-# NOINLINE unsafeBenchmarkFreezeGrowableInt #-}
unsafeBenchmarkFreezeGrowableInt =
  Unsafe.toLinear \(GrowableInternal.GrowableVector ref) ->
    case Ref.free ref of
      GrowableInternal.Header logicalSize vector ->
        Ur
          ( unsafePerformIO
              (U.unsafeFreeze (UM.unsafeTake logicalSize vector))
          )

directGrowableUnboxedMaterialization :: U.Vector Int -> U.Vector Int
{-# NOINLINE directGrowableUnboxedMaterialization #-}
directGrowableUnboxedMaterialization input =
  U.modify
    ( \vector -> do
        header <- newSTRef (U.length input, vector)
        (_, _) <- readSTRef header
        NonLinear.pure ()
    )
    input

pureBorrowGrowableUnboxedMaterialization :: U.Vector Int -> U.Vector Int
{-# NOINLINE pureBorrowGrowableUnboxedMaterialization #-}
pureBorrowGrowableUnboxedMaterialization input =
  unur $
    linearly \linear ->
      Growable.toVector (Growable.fromVector input linear)

pureBorrowGrowableUnboxedNoGrowthKernel :: U.Vector Int -> U.Vector Int
{-# NOINLINE pureBorrowGrowableUnboxedNoGrowthKernel #-}
pureBorrowGrowableUnboxedNoGrowthKernel input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.unsafeFromMutable
              (unsafePerformIO (U.thaw input))
              ownerLinear
          )
      vector <-
        Growable.withContent_ vector \content ->
          pureBorrowWorker (U.length input) 0 content
      let !() = consume vector
      pureAfter (unsafeBenchmarkFreezeGrowableInt (reclaim lend))

directGrowableUnboxedGrowthKernel :: U.Vector Int -> (U.Vector Int, Int)
{-# NOINLINE directGrowableUnboxedGrowthKernel #-}
-- The result length differs from the input's, so this one cannot go through
-- 'U.modify'; 'runST' keeps it equally safe, and the buffer never escapes.
directGrowableUnboxedGrowthKernel input = runST do
  initial <- UM.unsafeNew 0
  header <- newSTRef (0, initial)
  directGrowthWorker input 0 header
  (_, capacityContent) <- readSTRef header
  (logicalSize, content) <- readSTRef header
  frozen <- U.freeze (UM.unsafeTake logicalSize content)
  NonLinear.pure (frozen, UM.length capacityContent)

directGrowthWorker ::
  U.Vector Int ->
  Int ->
  STRef s (Int, UM.MVector s Int) ->
  ST s ()
{-# NOINLINE directGrowthWorker #-}
directGrowthWorker input !index header
  | index >= U.length input = NonLinear.pure ()
  | otherwise = do
      (logicalSize, content) <- readSTRef header
      grown <-
        if logicalSize < UM.length content
          then NonLinear.pure content
          else do
            let !capacity = growthTarget (UM.length content) (logicalSize + 1)
            target <- UM.unsafeNew capacity
            UM.unsafeCopy
              (UM.unsafeTake logicalSize target)
              (UM.unsafeTake logicalSize content)
            NonLinear.pure target
      UM.unsafeWrite grown logicalSize (U.unsafeIndex input index)
      writeSTRef header (logicalSize + 1, grown)
      directGrowthWorker input (index + 1) header

pureBorrowGrowableUnboxedGrowthKernel :: U.Vector Int -> (U.Vector Int, Int)
{-# NOINLINE pureBorrowGrowableUnboxedGrowthKernel #-}
pureBorrowGrowableUnboxedGrowthKernel input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.empty ownerLinear)
      vector <- pureBorrowGrowthWorker input 0 vector
      Growable.capacity vector & \(Ur finalCapacity, vector) -> DataFlow.do
        consume vector
        pureAfter
          (attachCapacity finalCapacity (unsafeBenchmarkFreezeGrowableInt (reclaim lend)))

attachCapacity :: Int -> Ur (U.Vector Int) %1 -> Ur (U.Vector Int, Int)
attachCapacity =
  Unsafe.toLinear2 \finalCapacity (Ur frozen) ->
    Ur (frozen, finalCapacity)

pureBorrowGrowthWorker ::
  forall α.
  U.Vector Int ->
  Int ->
  Mut α (Growable.GrowableVector Int) %1 ->
  BO α (Mut α (Growable.GrowableVector Int))
{-# NOINLINE pureBorrowGrowthWorker #-}
pureBorrowGrowthWorker input !index vector
  | index >= U.length input = Control.pure vector
  | otherwise = Control.do
      vector <- Growable.push (U.unsafeIndex input index) vector
      pureBorrowGrowthWorker input (index + 1) vector

growthTarget :: Int -> Int -> Int
{-# INLINE growthTarget #-}
growthTarget oldCapacity required
  | required <= oldCapacity = oldCapacity
  | oldCapacity <= 0 = required `max` 1
  | oldCapacity > maxBound `quot` 2 = required
  | otherwise = required `max` (oldCapacity * 2)

pureBorrowWorker ::
  forall α.
  Int ->
  Int ->
  Mut α (Vector.Vector Int) %1 ->
  BO α ()
{-# NOINLINE pureBorrowWorker #-}
pureBorrowWorker !length_ !index vector
  | index >= length_ = Control.pure (consume vector)
  | otherwise = Control.do
      ((), vector) <-
        Vector.unsafeUpdate
          index
          (\ !value -> Control.pure ((), value + 1))
          vector
      pureBorrowWorker length_ (index + 1) vector

owningBoxedQsortKernel :: B.Vector Int -> B.Vector Int
{-# NOINLINE owningBoxedQsortKernel #-}
owningBoxedQsortKernel input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (OwningVector.fromVector input ownerLinear)
      OwningVector.qsort 0 vector
      pureAfter (OwningVector.toVector (reclaim lend))

unrestrictedQsortKernel ::
  (G.Vector v Int) =>
  v Int ->
  v Int
{-# INLINEABLE unrestrictedQsortKernel #-}
unrestrictedQsortKernel input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (UnrestrictedVector.fromVector input ownerLinear)
      DC.qsort 0 vector
      pureAfter (UnrestrictedVector.toVector (reclaim lend))

unrestrictedBoxedQsortKernel :: B.Vector Int -> B.Vector Int
{-# NOINLINE unrestrictedBoxedQsortKernel #-}
unrestrictedBoxedQsortKernel = unrestrictedQsortKernel

unrestrictedUnboxedQsortKernel :: U.Vector Int -> U.Vector Int
{-# NOINLINE unrestrictedUnboxedQsortKernel #-}
unrestrictedUnboxedQsortKernel = unrestrictedQsortKernel

unrestrictedFftKernel ::
  (G.Vector v (Complex Double)) =>
  v (Complex Double) ->
  v (Complex Double)
{-# INLINEABLE unrestrictedFftKernel #-}
unrestrictedFftKernel input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (UnrestrictedVector.fromVector input ownerLinear)
      Control.void $
        DC.sequentialDivideAndConquer
          (DC.fftDC' 128)
          vector
      pureAfter (UnrestrictedVector.toVector (reclaim lend))

unrestrictedBoxedFftKernel ::
  B.Vector (Complex Double) ->
  B.Vector (Complex Double)
{-# NOINLINE unrestrictedBoxedFftKernel #-}
unrestrictedBoxedFftKernel = unrestrictedFftKernel

unrestrictedUnboxedFftKernel ::
  U.Vector (Complex Double) ->
  U.Vector (Complex Double)
{-# NOINLINE unrestrictedUnboxedFftKernel #-}
unrestrictedUnboxedFftKernel = unrestrictedFftKernel

defaultMain :: IO ()
defaultMain = Bench.defaultMain benches

benches :: [Benchmark]
benches =
  NonLinear.concat
    [ [ env
          (NonLinear.pure $ U.generate length_ (`NonLinear.rem` 1024))
          \input ->
            bgroup
              ("kernel/fixed-unboxed/" <> show length_)
              [ bench "direct" $ nf directFixedUnboxedKernel input
              , bench "pure-borrow" $ nf pureBorrowFixedUnboxedKernel input
              ]
      , env
          (NonLinear.pure $ U.generate length_ (`NonLinear.rem` 1024))
          \input ->
            bgroup
              ("kernel/growable-unboxed/no-growth/" <> show length_)
              [ bench "direct" $ nf directGrowableUnboxedNoGrowthKernel input
              , bench "pure-borrow" $ nf pureBorrowGrowableUnboxedNoGrowthKernel input
              ]
      , env
          (NonLinear.pure $ U.generate length_ (`NonLinear.rem` 1024))
          \input ->
            bgroup
              ("kernel/growable-unboxed/growth/" <> show length_)
              [ bench "direct" $ nf directGrowableUnboxedGrowthKernel input
              , bench "pure-borrow" $ nf pureBorrowGrowableUnboxedGrowthKernel input
              ]
      , env
          (NonLinear.pure $ U.generate length_ (`NonLinear.rem` 1024))
          \input ->
            bgroup
              ("public-materialization/fixed-unboxed/" <> show length_)
              [ bench "direct" $ nf directFixedUnboxedMaterialization input
              , bench "pure-borrow" $ nf pureBorrowFixedUnboxedMaterialization input
              ]
      , env
          (NonLinear.pure $ U.generate length_ (`NonLinear.rem` 1024))
          \input ->
            bgroup
              ("public-materialization/growable-unboxed/" <> show length_)
              [ bench "direct" $ nf directGrowableUnboxedMaterialization input
              , bench "pure-borrow" $ nf pureBorrowGrowableUnboxedMaterialization input
              ]
      ]
    | length_ <- [0, 1, 1024, 1024 * 1024]
    ]
    <> [ env
           ( NonLinear.pure
               ( B.generate length_ (comparisonQsortValue length_)
               , U.generate length_ (comparisonQsortValue length_)
               )
           )
           \ ~(boxedInput, unboxedInput) ->
             bgroup
               ( "algorithm/qsort/storage-and-element-ownership/"
                   <> show length_
               )
               [ bench "owning/boxed" $
                   nf owningBoxedQsortKernel boxedInput
               , bench "unrestricted/boxed" $
                   nf unrestrictedBoxedQsortKernel boxedInput
               , bench "unrestricted/unboxed" $
                   nf unrestrictedUnboxedQsortKernel unboxedInput
               ]
       | length_ <- [8 * 1024, 32 * 1024]
       ]
    <> [ env
           ( NonLinear.pure
               ( B.generate length_ comparisonFftValue
               , U.generate length_ comparisonFftValue
               )
           )
           \ ~(boxedInput, unboxedInput) ->
             bgroup
               ( "algorithm/fft/storage-and-element-ownership/"
                   <> show length_
               )
               [ bench "unrestricted/boxed" $
                   nf unrestrictedBoxedFftKernel boxedInput
               , bench "unrestricted/unboxed" $
                   nf unrestrictedUnboxedFftKernel unboxedInput
               ]
       | length_ <- [64 * 1024, 1024 * 1024]
       ]

comparisonQsortValue :: Int -> Int -> Int
comparisonQsortValue length_ index =
  (index * 1103515245 + 12345) `NonLinear.rem` (length_ + 1)

comparisonFftValue :: Int -> Complex Double
comparisonFftValue index =
  let position = fromIntegral index
   in (sin (position / 17) + cos (position / 31)) :+ 0
