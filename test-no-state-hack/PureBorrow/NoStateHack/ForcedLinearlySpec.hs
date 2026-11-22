{-# LANGUAGE BlockArguments #-}

module PureBorrow.NoStateHack.ForcedLinearlySpec (
  module PureBorrow.NoStateHack.ForcedLinearlySpec,
) where

import PureBorrow.NoStateHack.ForcedLinearly qualified as ForcedLinearly
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

test_forcedLinearly :: TestTree
test_forcedLinearly =
  testGroup
    "a forced Linearly still allocates a fresh resource"
    [ testCase "two references from two forced tokens are distinct" do
        ForcedLinearly.twoRefs 5 @?= (6, 5)
    , testCase "two references from two unforced tokens are distinct" do
        ForcedLinearly.twoRefsUnforced 5 @?= (6, 5)
    , testCase "a reference allocated from a forced token is fresh on every call" do
        ForcedLinearly.callTwiceRef @?= (0, 0)
    , testCase "a vector allocated from a forced token is fresh on every call" do
        ForcedLinearly.callTwiceVector @?= (0, 0)
    , testCase "two vectors from two forced tokens are distinct" do
        ForcedLinearly.twoVectors 5 @?= (6, 5)
    ]
