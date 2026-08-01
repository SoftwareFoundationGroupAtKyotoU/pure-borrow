{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Concurrent.DivideConquer.LinearSpec (
  module Control.Concurrent.DivideConquer.LinearSpec,
) where

import Control.Concurrent.DivideConquer.Linear
import Control.Concurrent.DivideConquer.Linear qualified as DC
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Copyable
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Complex (Complex (..), magnitude)
import Data.List qualified as List
import Data.List qualified as NonLinear
import Data.Vector qualified as V
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as UnrestrictedVector
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Data.Vector.Unboxed qualified as U
import Prelude.Linear
import System.Random (StdGen, mkStdGen)
import Test.Falsify.Generator qualified as G
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as G
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Prelude qualified as NonLinear

test_qsort :: TestTree
test_qsort =
  testGroup
    "qsort"
    [ testProperty "empty" do
        seed <- F.gen $ G.int $ G.between (minBound, maxBound)
        F.assert $
          P.expect (V.empty) P..$ ("output", qsortUnrestrictedDCVec (mkStdGen seed) (V.empty @Int))
    , testProperty "coincides with Data.List.sort on Ints" do
        seed <- F.gen $ G.int $ G.between (minBound, maxBound)
        xs <- F.gen $ G.list (G.between (1, 100)) $ G.int $ G.between (-100, 100)
        let v = V.fromList xs
            unrestrictedBoxed =
              qsortUnrestrictedDCVec (mkStdGen seed) v
            unrestrictedUnboxed =
              qsortUnrestrictedDCVec
                (mkStdGen seed)
                (U.fromList xs)
        F.collect "length" [ceiling @_ @Int (fromIntegral @_ @Double (V.length v) / 10) * 10]
        F.collect "min" [NonLinear.minimum v `quot` 10 * 10]
        F.collect "max" [NonLinear.maximum v `quot` 10 * 10]
        F.collect "sorted" [V.and $ V.zipWith (NonLinear.<=) v (V.tail v)]
        F.info $ "input: " <> show xs
        F.assert $
          P.expect
            ( List.sort xs
            , List.sort xs
            )
            P..$ ( "output"
                 ,
                   ( V.toList unrestrictedBoxed
                   , U.toList unrestrictedUnboxed
                   )
                 )
    ]

test_qsort_exported_modes :: TestTree
test_qsort_exported_modes =
  testCase "direct, naive, and work-sharing modes support boxed and unboxed backends" do
    let input = [5, 1, 4, 1, 3, 2] :: [Int]
        expected = List.sort input
    V.toList (qsortUnrestrictedDirectVec (V.fromList input)) @?= expected
    U.toList (qsortUnrestrictedDirectVec (U.fromList input)) @?= expected
    V.toList (qsortUnrestrictedNaiveVec (V.fromList input)) @?= expected
    U.toList (qsortUnrestrictedNaiveVec (U.fromList input)) @?= expected
    V.toList
      (qsortUnrestrictedDCVec (mkStdGen 42) (V.fromList input))
      @?= expected
    U.toList
      (qsortUnrestrictedDCVec (mkStdGen 42) (U.fromList input))
      @?= expected

qsortUnrestrictedDCVec ::
  (G.Vector v a, Ord a) =>
  StdGen ->
  v a ->
  v a
qsortUnrestrictedDCVec generator source =
  unur $
    linearly \linear -> DataFlow.do
      (ownerLinear, runLinear) <- dup linear
      runBO runLinear Control.do
        (vector, lend) <-
          borrowM
            ( UnrestrictedVector.fromVector
                source
                ownerLinear
            )
        Control.void $
          DC.qsortDC
            generator
            10
            128
            vector
        pureAfter
          ( UnrestrictedVector.toVector
              (reclaim lend)
          )

qsortUnrestrictedDirectVec ::
  (G.Vector v a, Ord a) =>
  v a ->
  v a
qsortUnrestrictedDirectVec source =
  unur $
    linearly \linear -> DataFlow.do
      (ownerLinear, runLinear) <- dup linear
      runBO runLinear Control.do
        (vector, lend) <-
          borrowM (UnrestrictedVector.fromVector source ownerLinear)
        DC.qsort 0 vector
        pureAfter (UnrestrictedVector.toVector (reclaim lend))

qsortUnrestrictedNaiveVec ::
  (G.Vector v a, Ord a) =>
  v a ->
  v a
qsortUnrestrictedNaiveVec source =
  unur $
    linearly \linear -> DataFlow.do
      (ownerLinear, runLinear) <- dup linear
      runBO runLinear Control.do
        (vector, lend) <-
          borrowM (UnrestrictedVector.fromVector source ownerLinear)
        Control.void $
          naiveDivideAndConquer
            (DC.qsortDC' 4)
            vector
        pureAfter (UnrestrictedVector.toVector (reclaim lend))

test_fft :: TestTree
test_fft =
  testGroup
    "fft"
    [ testCase "agrees across schedulers and backends" do
        let input =
              V.generate 16 \index ->
                fromIntegral index :+ 0
            unboxedInput =
              U.generate 16 \index ->
                fromIntegral index :+ 0
            boxedSequential =
              fftUnrestrictedSequential input
            boxedNaive =
              fftUnrestrictedNaive input
            boxedWorkSharing =
              fftUnrestrictedWorkSharing input
            unboxedSequential =
              fftUnrestrictedSequential unboxedInput
            unboxedNaive =
              fftUnrestrictedNaive unboxedInput
            unboxedWorkSharing =
              fftUnrestrictedWorkSharing unboxedInput
        boxedNaive @?= boxedSequential
        boxedWorkSharing @?= boxedSequential
        unboxedNaive @?= unboxedSequential
        unboxedWorkSharing @?= unboxedSequential
        U.toList unboxedSequential @?= V.toList boxedSequential
    , testCase "matches a direct DFT for small power-of-two inputs" do
        NonLinear.mapM_ checkFftAgainstDft [1, 2, 4, 8, 16]
    ]

checkFftAgainstDft :: Int -> IO ()
checkFftAgainstDft length_ = do
  let values =
        [ fromIntegral ((index * 7 + 3) `NonLinear.mod` 11)
            :+ fromIntegral ((index * 5 + 1) `NonLinear.mod` 7)
        | index <- [0 .. length_ - 1]
        ]
      expected = directDft values
      boxed = V.toList (fftUnrestrictedSequential (V.fromList values))
      unboxed = U.toList (fftUnrestrictedSequential (U.fromList values))
      tolerance = 1e-9 * fromIntegral length_
  assertComplexListsClose tolerance expected boxed
  assertComplexListsClose tolerance expected unboxed

directDft :: [Complex Double] -> [Complex Double]
directDft values =
  [ NonLinear.sum
      [ value
          NonLinear.* (cos angle :+ sin angle)
      | (sampleIndex, value) <- NonLinear.zip [0 :: Int ..] values
      , let angle =
              2
                * pi
                * fromIntegral outputIndex
                * fromIntegral sampleIndex
                / fromIntegral (NonLinear.length values)
      ]
  | outputIndex <- [0 .. NonLinear.length values - 1]
  ]

assertComplexListsClose ::
  Double ->
  [Complex Double] ->
  [Complex Double] ->
  IO ()
assertComplexListsClose tolerance expected actual =
  assertBool
    ( "expected "
        <> show expected
        <> ", but got "
        <> show actual
    )
    ( NonLinear.length expected
        == NonLinear.length actual
        && NonLinear.and
          ( NonLinear.zipWith
              ( \expectedValue actualValue ->
                  magnitude
                    (expectedValue NonLinear.- actualValue)
                    <= tolerance
              )
              expected
              actual
          )
    )

fftUnrestrictedSequential ::
  (G.Vector v (Complex Double)) =>
  v (Complex Double) ->
  v (Complex Double)
fftUnrestrictedSequential source =
  unur $
    linearly \linear -> DataFlow.do
      (ownerLinear, runLinear) <- dup linear
      runBO runLinear Control.do
        (vector, lend) <-
          borrowM
            ( UnrestrictedVector.fromVector
                source
                ownerLinear
            )
        Control.void $
          sequentialDivideAndConquer
            (DC.fftDC' 4)
            vector
        pureAfter
          ( UnrestrictedVector.toVector
              (reclaim lend)
          )

fftUnrestrictedNaive ::
  (G.Vector v (Complex Double)) =>
  v (Complex Double) ->
  v (Complex Double)
fftUnrestrictedNaive source =
  unur $
    linearly \linear -> DataFlow.do
      (ownerLinear, runLinear) <- dup linear
      runBO runLinear Control.do
        (vector, lend) <-
          borrowM (UnrestrictedVector.fromVector source ownerLinear)
        Control.void $
          naiveDivideAndConquer
            (DC.fftDC' 4)
            vector
        pureAfter (UnrestrictedVector.toVector (reclaim lend))

fftUnrestrictedWorkSharing ::
  (G.Vector v (Complex Double)) =>
  v (Complex Double) ->
  v (Complex Double)
fftUnrestrictedWorkSharing source =
  unur $
    linearly \linear -> DataFlow.do
      (ownerLinear, runLinear) <- dup linear
      runBO runLinear Control.do
        (vector, lend) <-
          borrowM
            ( UnrestrictedVector.fromVector
                source
                ownerLinear
            )
        Control.void $
          DC.fftDC
            (mkStdGen 42)
            2
            4
            vector
        pureAfter
          ( UnrestrictedVector.toVector
              (reclaim lend)
          )
