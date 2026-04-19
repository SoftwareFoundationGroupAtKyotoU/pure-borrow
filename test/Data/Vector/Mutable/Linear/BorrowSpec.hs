{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
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
  runBO l1 \ @α ->
    borrow @α (VL.fromVector v l2) l3
      & \(v, lend) -> Control.do
        VL.qsort 8 v
        pureAfter $ VL.toVector (reclaim lend)

divideList :: [Int] -> (Int, [Int])
divideList [] = (0, [])
divideList xs =
  let v0 = (V.fromList xs)
      pivot = v0 V.! (V.length v0 `quot` 2)
   in Bi.second unur $ linearly \lin -> DataFlow.do
        (l1, l2, l3) <- dup3 lin
        runBO l1 \ @α ->
          borrow @α (VL.fromList xs l2) l3
            & \(v, lend) ->
              VL.size v & \(Ur len, v) -> Control.do
                (lo, hi) <- VL.divide pivot v 0 len
                VL.size lo & \(Ur n, lo) -> DataFlow.do
                  consume lo
                  consume hi
                  pureAfter (n, VL.toList $ reclaim lend)

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

example1 :: (Int, [Int])
example1 = linearly \lin -> DataFlow.do
  (lin, lin') <- dup lin
  vec <- VL.fromList [0, 1, 2] lin
  runBO lin' \ @α -> Control.do
    let !(mvec, lend) = borrowLinearOnly @α vec
    mvec <- VL.modify 0 (+ 3) mvec
    mvec <- VL.modify 2 (+ 5) mvec
    mvec <- VL.modify 0 (* 4) mvec
    let !(Ur svec) = share mvec
    Ur n <- VL.copyAt 0 svec
    pureAfter $ (n, unur $ VL.toList (reclaim lend))

test_example1 :: TestTree
test_example1 =
  testCase "example1" do
    example1 @?= (12, [12, 1, 7])

example2 :: (Int, [Int])
example2 = linearly \lin -> DataFlow.do
  (lin, lin') <- dup lin
  vec <- VL.fromList [0, 1, 2] lin
  runBO lin' \ @α -> Control.do
    let !(mvec, lend) = borrowLinearOnly @α vec
    let !(mvec1, mvec2) = VL.splitAt 1 mvec
    (mvec, ()) <-
      parBO
        ( Control.do
            mvec1 <- VL.modify 0 (+ 3) mvec1
            VL.modify 0 (* 4) mvec1
        )
        (consume Control.<$> VL.modify 1 (+ 5) mvec2)
    let !(Ur svec) = share mvec
    Ur n <- VL.copyAt 0 svec
    pureAfter $ (n, unur $ VL.toList (reclaim lend))

test_example2 :: TestTree
test_example2 =
  testCase "example2" do
    example2 @?= (12, [12, 1, 7])

example3 :: (Int, [Int])
example3 = linearly \lin -> DataFlow.do
  (lin, lin') <- dup lin
  vec <- VL.fromList [0, 1, 2] lin
  runBO lin' \ @α -> Control.do
    let !(mvec, lend) = borrowLinearOnly @α vec
    mvec <- reborrowing_ mvec \mvec -> Control.do
      let !(mvec1, mvec2) = VL.splitAt 1 mvec
      consume
        Control.<$> parBO
          ( Control.do
              mvec1 <- VL.modify 0 (+ 3) mvec1
              VL.modify 0 (* 4) mvec1
          )
          (VL.modify 1 (+ 5) mvec2)
    let !(Ur svec) = share mvec
    Ur n <- VL.copyAt 0 svec
    pureAfter $ (n, unur $ VL.toList (reclaim lend))

test_example3 :: TestTree
test_example3 =
  testCase "example3" do
    example3 @?= (12, [12, 1, 7])
