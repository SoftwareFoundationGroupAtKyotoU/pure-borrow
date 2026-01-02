{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Vector.Mutable.Linear.BorrowSpec (
  module Data.Vector.Mutable.Linear.BorrowSpec,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Bifunctor.Linear qualified as Bi
import Data.List qualified as List
import Data.Vector qualified as V
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Prelude.Linear
import Test.Falsify.Generator qualified as G
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Property qualified as F
import Test.Falsify.Range qualified as G
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit
import Prelude qualified as NonLinear

qsortVec :: (Ord a, Copyable a) => V.Vector a -> V.Vector a
qsortVec v = unur $ linearly \lin -> DataFlow.do
  (l1, l2, l3) <- dup3 lin
  runBO l1 $
    borrow (VL.fromVector v l2) l3
      & \(v, lend) -> Control.do
        VL.qsort 8 v
        Control.pure $ \end -> VL.toVector (reclaim lend end)

divideList :: [Int] -> (Int, [Int])
divideList [] = (0, [])
divideList xs =
  let v0 = (V.fromList xs)
      pivot = v0 V.! (V.length v0 `quot` 2)
   in Bi.second unur $ linearly \lin -> DataFlow.do
        (l1, l2, l3) <- dup3 lin
        runBO l1 $
          borrow (VL.fromList xs l2) l3
            & \(v, lend) ->
              VL.size v & \(Ur len, v) -> Control.do
                (lo, hi) <- VL.divide pivot v 0 len
                VL.size lo & \(Ur n, lo) -> DataFlow.do
                  consume lo
                  consume hi
                  Control.pure \end ->
                    (n, VL.toList $ reclaim lend end)

test_divideList :: TestTree
test_divideList =
  testGroup
    "divideList"
    [ testCase "empty" do
        divideList [] @?= (0, [])
    , testProperty "singleton" do
        x <- F.gen $ G.int $ G.between (-100, 100)
        F.assert $
          P.expect (0, [x])
            P..$ ("answer", divideList [x])
    , testProperty "non-empty" do
        xs <- F.gen $ G.list (G.between (1, 100)) $ G.int $ G.between (0, 100)
        let v = V.fromList xs
            pivot = v V.! (V.length v `quot` 2)
            (off, vs) = divideList xs
            (lo, hi) = V.splitAt off $ V.fromList vs

        F.collect "length" [ceiling @_ @Int (fromIntegral @_ @Double (V.length v) / 10) * 10]
        F.collect "min" [NonLinear.minimum v `quot` 10 * 10]
        F.collect "max" [NonLinear.maximum v `quot` 10 * 10]
        F.info $ "pivot: " <> show pivot
        F.assert $
          P.satisfies ("lo <= " <> show pivot, V.all (NonLinear.<= pivot))
            P..$ ("lo", lo)
        F.assert $
          P.satisfies ("hi >= " <> show pivot, V.all (NonLinear.>= pivot))
            P..$ ("hi", hi)
    ]

test_qsort :: TestTree
test_qsort =
  testGroup
    "qsort"
    [ testCase "empty" do
        qsortVec (V.empty @Int) @?= V.empty
    , testProperty "coincides with Data.List.sort on Ints" do
        xs <- F.gen $ G.list (G.between (1, 100)) $ G.int $ G.between (-100, 100)
        let v = V.fromList xs
            sorted = qsortVec v
        F.collect "length" [ceiling @_ @Int (fromIntegral @_ @Double (V.length v) / 10) * 10]
        F.collect "min" [NonLinear.minimum v `quot` 10 * 10]
        F.collect "max" [NonLinear.maximum v `quot` 10 * 10]
        F.collect "sorted" [V.and $ V.zipWith (NonLinear.<=) v (V.tail v)]
        F.info $ "input: " <> show xs
        F.assert $
          P.expect (V.fromList $ List.sort xs)
            P..$ ("output", sorted)
    ]
