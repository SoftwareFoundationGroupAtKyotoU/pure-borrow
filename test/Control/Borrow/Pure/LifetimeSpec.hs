{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -O0 #-}

module Control.Borrow.Pure.LifetimeSpec (
  module Control.Borrow.Pure.LifetimeSpec,
) where

import Control.Borrow.Pure.Lifetime.Internal
import Control.Borrow.Pure.Lifetime.TypingCases
import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Data.Functor
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Unsafe.Coerce (unsafeCoerce)

l1LeqL2 :: Dict (L1 <= L2)
l1LeqL2 = unsafeCoerce $ MkDict @(Static <= Static)

l2LeqL3 :: Dict (L2 <= L3)
l2LeqL3 = unsafeCoerce $ MkDict @(Static <= Static)

l1LeqL3 :: Dict (L1 <= L3)
l1LeqL3 = unsafeCoerce $ MkDict @(Static <= Static)

test_should_pass :: TestTree
test_should_pass =
  testGroup
    "should typechecks"
    [ testCase "(a <= b, b <= c) => a <= c" do
        void $ evaluate $ force $ withDict l1LeqL2 $ withDict l2LeqL3 $ transitive @L1 @L2 @L3
    , testCase "a <= b => a /\\ c <= b" do
        void $ evaluate $ force $ withDict l1LeqL2 $ infElimL L1 L2 L3
    , testCase "a <= b => c /\\ a <= b" do
        void $ evaluate $ force $ withDict l1LeqL2 $ infElimR L1 L2 L3
    , testCase "(a <= b, a <= c) => a <= b /\\ c" do
        void $ evaluate $ force $ withDict l1LeqL2 $ withDict l1LeqL3 $ infIntro L1 L2 L3
    , testCase "a /\\ b <= b /\\ a" do
        void $ evaluate $ force $ infComm L1 L2
    , testCase "a /\\ b <= a" do
        void $ evaluate $ force $ infL L1 L2
    , testCase "a /\\ b <= b" do
        void $ evaluate $ force $ infR L1 L2
    ]