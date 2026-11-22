{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -O0 #-}

module PureBorrow.Unoptimised.OrderingSpec (
  module PureBorrow.Unoptimised.OrderingSpec,
) where

import PureBorrow.Unoptimised.Ordering qualified as Ordering
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

test_ordering :: TestTree
test_ordering =
  testGroup
    "reads and writes through borrows are ordered by the state token, at -O0"
    [ testCase "Ref.Borrow.update writes before the scope ends" do
        Ordering.refUpdateWrites @?= (0, 1)
    , testCase "HashMap.take_ leaves an empty table behind" do
        Ordering.hashMapTakeEmpties @?= ([(1, 10), (2, 20)], [(3, 30)])
    , testCase "a growable size read after reclaim sees the push" do
        Ordering.growableSizeAfterReclaim @?= (3, 4)
    , testCase "a hash map size read after reclaim sees the insert" do
        Ordering.hashMapSizeAfterReclaim @?= (2, 3)
    , testCase "an owner reclaimed after a scope is not served from an earlier dup2" do
        Ordering.refDupReclaimFresh @?= (1, 0)
    , testCase "an owner reclaimed after a scope does not hand back a reference it gave away" do
        Ordering.refDupReclaimOwners @?= (100, 1)
    ]
