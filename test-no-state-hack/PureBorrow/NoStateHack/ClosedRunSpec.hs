{-# LANGUAGE BlockArguments #-}

module PureBorrow.NoStateHack.ClosedRunSpec (
  module PureBorrow.NoStateHack.ClosedRunSpec,
) where

import PureBorrow.NoStateHack.ClosedRun qualified as ClosedRun
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

test_closedRun :: TestTree
test_closedRun =
  testGroup
    "a run of BO whose action has no free variables allocates afresh"
    [ testCase "two references from two runBO_ calls are distinct" do
        ClosedRun.twoMkRef 1 @?= (1, 0)
    , testCase "two references from two runBO calls are distinct" do
        ClosedRun.twoMkRefAfter 1 @?= (1, 0)
    , testCase "two references from two inlined runs are distinct" do
        ClosedRun.twoMkRefInline 1 @?= (1, 0)
    , testCase "two vectors from two runs are distinct" do
        ClosedRun.twoMkVector 5 @?= (5, 0)
    , testCase "a reference allocated by a run is fresh on every call" do
        ClosedRun.callTwiceMkRef @?= (0, 0)
    , testCase "a reference allocated between newLifetime and endLifetime is fresh on every call" do
        ClosedRun.callTwiceNewLifetime @?= (0, 0)
    ]
