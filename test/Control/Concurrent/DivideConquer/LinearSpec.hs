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
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.List qualified as List
import Data.List qualified as NonLinear
import Data.Vector qualified as V
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Prelude.Linear
import Test.Falsify.Generator qualified as G
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as G
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude qualified as NonLinear

test_qsort :: TestTree
test_qsort =
  testGroup
    "qsort"
    [ testCase "empty" do
        qsortDCVec (V.empty @Int) @?= V.empty
    , testProperty "coincides with Data.List.sort on Ints" do
        xs <- F.gen $ G.list (G.between (1, 100)) $ G.int $ G.between (-100, 100)
        let v = V.fromList xs
            sorted = qsortDCVec v
        F.collect "length" [ceiling @_ @Int (fromIntegral @_ @Double (V.length v) / 10) * 10]
        F.collect "min" [NonLinear.minimum v `quot` 10 * 10]
        F.collect "max" [NonLinear.maximum v `quot` 10 * 10]
        F.collect "sorted" [V.and $ V.zipWith (NonLinear.<=) v (V.tail v)]
        F.info $ "input: " <> show xs
        F.assert
          $ P.expect (V.fromList $ List.sort xs)
          P..$ ("output", sorted)
    ]

qsortDCVec :: (Ord a, Copyable a) => V.Vector a -> V.Vector a
qsortDCVec v = unur $ linearly \lin -> DataFlow.do
  (l1, l2, l3) <- dup3 lin
  runBO l1
    $ borrow (VL.fromVector v l2) l3
    & \(v, lend) -> Control.do
      Control.void $ divideAndConquer 10 (qsortDC 16) v
      Control.pure $ \end -> VL.toVector (reclaim lend end)
