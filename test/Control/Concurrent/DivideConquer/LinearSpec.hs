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
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Copyable
import Control.Syntax.DataFlow qualified as DataFlow
import Data.List qualified as List
import Data.List qualified as NonLinear
import Data.Vector qualified as V
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Prelude.Linear
import System.Random (StdGen, mkStdGen)
import Test.Falsify.Generator qualified as G
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as G
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.Falsify qualified as F
import Prelude qualified as NonLinear

test_qsort :: TestTree
test_qsort =
  testGroup
    "qsort"
    [ testProperty "empty" do
        seed <- F.gen $ G.int $ G.between (minBound, maxBound)
        F.assert $
          P.expect (V.empty) P..$ ("output", qsortDCVec (mkStdGen seed) (V.empty @Int))
    , testProperty "coincides with Data.List.sort on Ints" do
        seed <- F.gen $ G.int $ G.between (minBound, maxBound)
        xs <- F.gen $ G.list (G.between (1, 100)) $ G.int $ G.between (-100, 100)
        let v = V.fromList xs
            sorted = qsortDCVec (mkStdGen seed) v
        F.collect "length" [ceiling @_ @Int (fromIntegral @_ @Double (V.length v) / 10) * 10]
        F.collect "min" [NonLinear.minimum v `quot` 10 * 10]
        F.collect "max" [NonLinear.maximum v `quot` 10 * 10]
        F.collect "sorted" [V.and $ V.zipWith (NonLinear.<=) v (V.tail v)]
        F.info $ "input: " <> show xs
        F.assert $
          P.expect (V.fromList $ List.sort xs)
            P..$ ("output", sorted)
    ]

qsortDCVec :: (Ord a, Copyable a) => StdGen -> V.Vector a -> V.Vector a
qsortDCVec g v = unur $ linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  runBO l1 Control.do
    (v, lend) <- borrowM (VL.fromVector v l2)
    Control.void $ qsortDC g 10 128 v
    Control.pure $ After (VL.toVector (reclaim lend))
