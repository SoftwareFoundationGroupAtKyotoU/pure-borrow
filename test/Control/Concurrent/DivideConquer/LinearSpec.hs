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
import Control.Exception (evaluate)
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
import System.Random (StdGen, mkStdGen, randomR)
import System.Timeout (timeout)
import Test.Falsify.Generator qualified as G
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as G
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
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

-- | A divide-and-conquer that splits into @pieces@ pieces at once, down to leaves of at most @leafSize@ elements; with @skewed@, every piece but the last has one element.
data Fanout = Fanout {pieces :: Int, leafSize :: Int, skewed :: Bool}
  deriving (NonLinear.Show)

-- | Split a borrow of @n@ elements as the 'Fanout' says, the last piece taking the rest.
fanoutPieces :: (G.Vector v Int) => Fanout -> Int -> Mut β (UnrestrictedVector.Vector v Int) %1 -> [(Ur (), Mut β (UnrestrictedVector.Vector v Int))]
fanoutPieces shape@(Fanout k _ _) n v
  | k NonLinear.<= 1 NonLinear.|| n NonLinear.<= 1 = [(Ur (), v)]
  | otherwise =
      let c = fanoutFirst shape n
       in if n NonLinear.<= c
            then [(Ur (), v)]
            else case UnrestrictedVector.splitAt c v of
              (l, r) -> (Ur (), l) : fanoutPieces shape {pieces = k NonLinear.- 1} (n NonLinear.- c) r

-- | The size of the first piece.
fanoutFirst :: Fanout -> Int -> Int
fanoutFirst (Fanout k _ skew) n = if skew then 1 else (n NonLinear.+ k NonLinear.- 1) `NonLinear.div` k

-- | The sizes of the pieces of 'fanoutPieces'.
fanoutSizes :: Fanout -> Int -> [Int]
fanoutSizes shape@(Fanout k _ _) n
  | k NonLinear.<= 1 NonLinear.|| n NonLinear.<= 1 = [n]
  | otherwise =
      let c = fanoutFirst shape n
       in if n NonLinear.<= c then [n] else c : fanoutSizes shape {pieces = k NonLinear.- 1} (n NonLinear.- c)

-- | The number of nodes of the divide-and-conquer on @n@ elements.
fanoutNodes :: Fanout -> Int -> Int
fanoutNodes shape@(Fanout _ leaf _) n
  | n NonLinear.<= leaf = 1
  | otherwise = 1 NonLinear.+ NonLinear.sum (NonLinear.map (fanoutNodes shape) (fanoutSizes shape n))

-- | Increment the elements from @i@ below @n@ once each.
incrementFrom :: (G.Vector v Int) => Int -> Int -> Mut β (UnrestrictedVector.Vector v Int) %1 -> BO β (Mut β (UnrestrictedVector.Vector v Int))
incrementFrom i n v
  | i NonLinear.>= n = Control.pure v
  | otherwise = Control.do
      v <- UnrestrictedVector.modify i (NonLinear.+ 1) v
      incrementFrom (i NonLinear.+ 1) n v

-- | Every leaf increments each of its elements once, and every node counts itself.
fanoutDC :: (G.Vector v Int) => Fanout -> DivideConquer () α [] (UnrestrictedVector.Vector v Int) Int
fanoutDC shape@(Fanout _ leaf _) =
  DivideConquer
    { initialise = \v -> Control.pure (move (consume v))
    , divide = \() v -> case UnrestrictedVector.size v of
        (Ur n, v)
          | n NonLinear.<= leaf -> Control.do
              v <- incrementFrom 0 n v
              Control.pure (v `lseq` Done 1)
          | otherwise -> Control.pure (Continue (fanoutPieces shape n v))
    , conquer = Conquer \() v rs -> v `lseq` Control.pure (case move rs of Ur xs -> 1 NonLinear.+ NonLinear.sum xs)
    }

-- | Run 'fanoutDC' on @n@ zeros with the work-stealing scheduler; returns the node count and the elements.
runFanout :: Int -> Int -> Fanout -> Int -> (Int, U.Vector Int)
{-# NOINLINE runFanout #-}
runFanout seed workers shape n =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (UnrestrictedVector.constant n 0 ownerLinear)
      (count, vector) <- divideAndConquer' (mkStdGen seed) workers (fanoutDC shape) vector
      case move count of
        Ur count -> vector `lseq` pureAfter (case UnrestrictedVector.toVector (reclaim lend) of Ur elements -> Ur (count, elements))

{- | A divide-and-conquer that splits many ways at once pushes many tasks in one batch, so that its work-stealing deques grow while thieves steal.
A task that a thief took twice increments its elements twice, and a lost one leaves them at 0, or hangs the call.
Up to 0.1.0.0 the deque did both; with only its resize fixed, this test failed in 10 of 10 runs at @-N10@, each within its first 60 cases, three of them by the timeout.
-}
test_fanout :: TestTree
test_fanout =
  testCase "a divide-and-conquer that splits many ways runs every node exactly once" do
    NonLinear.mapM_ run [1 .. 200 :: Int]
  where
    run seed = do
      let gen0 = mkStdGen seed
          (k, gen1) = randomR (2, 300) gen0
          (leaf, gen2) = randomR (1, 64) gen1
          (skew, gen3) = randomR (False, True) gen2
          (n, gen4) = randomR (0, 20_000) gen3
          (workers, _) = randomR (2, 10) gen4
          shape = Fanout {pieces = k, leafSize = leaf, skewed = skew}
          what = NonLinear.show (seed, shape, n, workers)
      result <- timeout 10_000_000 do
        case runFanout seed workers shape n of
          (count, elements) -> evaluate (count `NonLinear.seq` elements `NonLinear.seq` (count, elements))
      case result of
        NonLinear.Nothing -> assertFailure ("did not finish within 10 seconds: " <> what)
        NonLinear.Just (count, elements) -> do
          assertBool ("an element was not incremented exactly once: " <> what) (U.all (NonLinear.== 1) elements)
          assertBool ("a node was not counted exactly once: " <> what) (count NonLinear.== fanoutNodes shape n)
