{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -O0 #-}

module Control.Borrow.Pure.LifetimeSpec (module Control.Borrow.Pure.LifetimeSpec) where

import Control.Borrow.Pure.Lifetime.Internal
import Control.Borrow.Pure.Lifetime.TypingCases
import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Data.Functor
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Unsafe.Coerce (unsafeCoerce)

l1LeqL2 :: Dict (L1 <= L2)
l1LeqL2 = unsafeCoerce $ MkDict @(Static <= Static)

l2LeqL3 :: Dict (L2 <= L3)
l2LeqL3 = unsafeCoerce $ MkDict @(Static <= Static)

test_should_pass :: TestTree
test_should_pass =
  testGroup
    "should pass"
    [ testCase "transitivity" do
        void $ evaluate $ rnf $ transitive l1LeqL2 l2LeqL3
    ]