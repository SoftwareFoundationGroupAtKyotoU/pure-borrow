{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -O0 #-}

module Control.Monad.Borrow.Pure.LifetimeSpec (
  module Control.Monad.Borrow.Pure.LifetimeSpec,
) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.Borrow.Pure.Lifetime.Internal
import Control.Monad.Borrow.Pure.Lifetime.TypingCases
import Data.Functor
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
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
    [ expectFailBecause "We don't rely on transitivity" $
        testCase "(a <= b, b <= c) => a <= c" do
          void $ evaluate $ force $ withDict l1LeqL2 $ withDict l2LeqL3 $ transitive @L1 @L2 @L3
    , expectFailBecause "Monotonicity is not guaranteed and not currently used" $
        testCase "a <= b => a /\\ c <= b" do
          void $ evaluate $ force $ withDict l1LeqL2 $ infElimL L1 L2 L3
    , expectFailBecause "Monotonicity is not guaranteed and not currently used" $
        testCase "a <= b => c /\\ a <= b" do
          void $ evaluate $ force $ withDict l1LeqL2 $ infElimR L1 L2 L3
    , expectFailBecause "Monotonicity is not guaranteed and not currently used" $
        testCase "a <= b => a /\\ c <= b /\\ c" do
          void $ evaluate $ force $ withDict l1LeqL2 $ infMonotone L1 L2 L3
    , testCase "(a <= b, a <= c) => a <= b /\\ c" do
        void $ evaluate $ force $ withDict l1LeqL2 $ withDict l1LeqL3 $ infIntro L1 L2 L3
    , testCase "a /\\ b <= b /\\ a" do
        void $ evaluate $ force $ infComm L1 L2
    , testCase "a /\\ b <= a" do
        void $ evaluate $ force $ infL L1 L2
    , testCase "a /\\ b <= b" do
        void $ evaluate $ force $ infR L1 L2
    ]
