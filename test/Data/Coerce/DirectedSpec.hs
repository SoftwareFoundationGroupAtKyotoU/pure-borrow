{-# LANGUAGE BlockArguments #-}

module Data.Coerce.DirectedSpec (
  module Data.Coerce.DirectedSpec,
) where

import Control.Exception qualified as Exception
import Data.Coerce.Directed (upcast)
import Data.Coerce.Directed.TypingCases
import Data.List qualified as List
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- These must keep compiling: this module is not built with deferred type errors.

-- A linear function is usable as an unrestricted one.
linearAsUnrestricted :: (Int %1 -> Int) -> (Int -> Int)
linearAsUnrestricted f = upcast f

-- Reflexive upcasts stay derivable at a multiplicity variable.
multiplicityPolymorphicRefl :: (Int %p -> Int) -> (Int %p -> Int)
multiplicityPolymorphicRefl f = upcast f

-- A function of any multiplicity is usable as an unrestricted one.
anyAsUnrestricted :: (Int %m -> Int) -> (Int -> Int)
anyAsUnrestricted f = upcast f

-- A linear function is usable at any multiplicity.
linearAsAny :: (Int %1 -> Int) -> (Int %m -> Int)
linearAsAny f = upcast f

test_multiplicityOrder :: TestTree
test_multiplicityOrder =
  testGroup
    "multiplicity order used by upcast"
    [ testCase "One is below Many" do
        _ <- Exception.evaluate oneBelowMany
        pure ()
    , testCase "Many is below Many" do
        _ <- Exception.evaluate manyBelowMany
        pure ()
    , expectDeferredTypeError
        "Many is not below One"
        "Couldn't match type"
        badManyBelowOne
    , testCase "a linear function upcasts to an unrestricted one" do
        linearAsUnrestricted (\x -> x) 42 @?= 42
    , testCase "a multiplicity-polymorphic reflexive upcast" do
        multiplicityPolymorphicRefl (\x -> x) 42 @?= 42
    , testCase "a function of any multiplicity upcasts to an unrestricted one" do
        anyAsUnrestricted (\x -> x) 42 @?= 42
    , testCase "a linear function upcasts to any multiplicity" do
        linearAsAny (\x -> x) 42 @?= 42
    ]
  where
    expectDeferredTypeError description expectedFragment value =
      testCase description do
        result <- Exception.try @Exception.SomeException (Exception.evaluate value)
        case result of
          Left exception ->
            assertBool
              ("unexpected deferred type error: " <> Exception.displayException exception)
              (expectedFragment `List.isInfixOf` Exception.displayException exception)
          Right _ ->
            assertFailure
              ("expected deferred type error containing " <> expectedFragment)
