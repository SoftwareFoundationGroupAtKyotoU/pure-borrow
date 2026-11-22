{-# LANGUAGE BlockArguments #-}

module PureBorrow.NoStateHack.ScopesSpec (
  module PureBorrow.NoStateHack.ScopesSpec,
) where

import PureBorrow.NoStateHack.Scopes qualified as Scopes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

test_scopes :: TestTree
test_scopes =
  testGroup "the scopes" $
    [ testCase path (kernel 0 @?= 1)
    | (path, kernel) <- Scopes.kernels
    ]
      <> [ testCase "an owner taken out in srunBO is handed back once" do
             Scopes.ownerTwice 0 @?= (100, 1)
         , testCase "an owner taken out and freed in srunBO is not handed back" do
             Scopes.ownerGivenAway 0 @?= 100
         ]
