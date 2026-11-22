{-# LANGUAGE BlockArguments #-}

module PureBorrow.NoStateHack.ForcedTokenSpec (
  module PureBorrow.NoStateHack.ForcedTokenSpec,
) where

import PureBorrow.NoStateHack.ForcedToken qualified as ForcedToken
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

test_forcedToken :: TestTree
test_forcedToken =
  testGroup "a scope written by the user that forces the end token" $
    [ testCase path (result @?= (1, 0))
    | (path, result) <- ForcedToken.kernels
    ]
      <> [ testCase "an owner taken out and freed in the scope is not handed back" do
             ForcedToken.givenAway @?= (100, 0)
         ]
