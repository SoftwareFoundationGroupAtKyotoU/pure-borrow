{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Ref.LinearSpec (
  module Data.Ref.LinearSpec,
) where

import Control.Monad.Borrow.Pure (linearly)
import Data.Ref.Linear qualified as Ref
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- 'GHC.Exts.atomicModifyMutVar2#' stores the first field of the function's
-- result; these cases would read back the wrong value, or crash, if the
-- wrappers handed it a function whose result is not a record with the new
-- contents first.

atomicModifyInt :: Int -> Int
{-# NOINLINE atomicModifyInt #-}
atomicModifyInt start =
  linearly \lin -> Ref.free (Ref.atomicModify_ (+ 1) (Ref.new start lin))

atomicModifyPair :: Int -> (Int, Int)
{-# NOINLINE atomicModifyPair #-}
atomicModifyPair start = linearly \lin ->
  case Ref.atomicModify (\x -> (x, 5)) (Ref.new start lin) of
    (old, ref) -> (old, Ref.free ref)

atomicModifyList :: [Int] -> [Int]
{-# NOINLINE atomicModifyList #-}
atomicModifyList start =
  linearly \lin -> Ref.free (Ref.atomicModify_ (7 :) (Ref.new start lin))

test_atomicModify :: TestTree
test_atomicModify =
  testGroup
    "atomicModify stores the function's new value"
    [ testCase "atomicModify_ on an Int" do
        atomicModifyInt 0 @?= 1
    , testCase "atomicModify returns the old value and stores the new one" do
        atomicModifyPair 0 @?= (0, 5)
    , testCase "atomicModify_ on a list" do
        atomicModifyList [8] @?= [7, 8]
    ]
