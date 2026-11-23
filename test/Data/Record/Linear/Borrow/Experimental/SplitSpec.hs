{-# LANGUAGE BlockArguments #-}

module Data.Record.Linear.Borrow.Experimental.SplitSpec (
  module Data.Record.Linear.Borrow.Experimental.SplitSpec,
) where

import Control.Exception qualified as Exception
import Data.List qualified as List
import Data.Record.Linear.Borrow.Experimental.Split.TypingCases
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

test_splitRecordRoles :: TestTree
test_splitRecordRoles =
  testGroup
    "SplitRecord cannot be upcast"
    [ expectDeferredTypeError
        "to a field list that puts back a field already split out"
        ["Couldn't match type: '[]", "arising from a use of"]
        badResetFields
    , expectDeferredTypeError
        "to a record whose field holds the record"
        ["Couldn't match type", "arising from a use of"]
        badViewRecord
    ]
  where
    expectDeferredTypeError description expectedFragments value =
      testCase description do
        result <- Exception.try @Exception.SomeException (Exception.evaluate value)
        case result of
          Left exception ->
            assertBool
              ("unexpected deferred type error: " <> Exception.displayException exception)
              (all (`List.isInfixOf` Exception.displayException exception) expectedFragments)
          Right _ ->
            assertFailure
              ("expected deferred type error containing " <> List.intercalate " and " expectedFragments)
