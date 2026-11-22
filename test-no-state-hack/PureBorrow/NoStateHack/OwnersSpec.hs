module PureBorrow.NoStateHack.OwnersSpec (
  module PureBorrow.NoStateHack.OwnersSpec,
) where

import PureBorrow.NoStateHack.Owners qualified as Owners
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

test_ownerKernels :: TestTree
test_ownerKernels =
  testGroup
    "the owner kernels"
    [ testCase path (result @?= (1, 0))
    | (path, result) <- Owners.ownerKernels
    ]
