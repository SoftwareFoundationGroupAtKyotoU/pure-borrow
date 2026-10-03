{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Borrow.Pure.Experimental.LoopSpec (
  module Control.Monad.Borrow.Pure.Experimental.LoopSpec,
) where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Experimental.Loop (foldBorrow)
import Control.Monad.Borrow.Pure.Experimental.Loop.TypingCases
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude qualified as NonLinear

sumBorrowedList :: Int
{-# NOINLINE sumBorrowedList #-}
sumBorrowedList = linearly \lin -> runBO lin Control.do
  (list, lend) <- borrowM [1, 2, 3 :: Int]
  let !(Ur shared) = share list
      !(Sum total) = foldBorrow (\element -> Sum (copy element)) shared
  pureAfter (consume (reclaim lend) `lseq` total)

sumBorrowedNonEmpty :: Int
{-# NOINLINE sumBorrowedNonEmpty #-}
sumBorrowedNonEmpty = linearly \lin -> runBO lin Control.do
  (list, lend) <- borrowM (1 :| [2, 3 :: Int])
  let !(Ur shared) = share list
      !(Sum total) = foldBorrow (\element -> Sum (copy element)) shared
  pureAfter (consume (reclaim lend) `lseq` total)

test_foldBorrow :: TestTree
test_foldBorrow =
  testGroup
    "foldBorrow"
    [ testCase "folds the element borrows of an immutable container" do
        sumBorrowedList @?= 6
    , testCase "folds the element borrows of a non-empty list" do
        sumBorrowedNonEmpty @?= 6
    , testCase "a sum type is split, not folded" do
        result <-
          Exception.try @Exception.SomeException
            (Exception.evaluate (badFoldEither (NonLinear.error "never forced")))
        case result of
          NonLinear.Left exception ->
            assertBool
              ("unexpected deferred type error: " <> Exception.displayException exception)
              ("Use splitEither directly!" `List.isInfixOf` Exception.displayException exception)
          NonLinear.Right _ ->
            assertFailure "expected a deferred type error"
    , testCase "a mutable container cannot be folded through a borrow" do
        result <-
          Exception.try @Exception.SomeException
            (Exception.evaluate (badFoldMutableVector (NonLinear.error "never forced")))
        case result of
          NonLinear.Left exception ->
            assertBool
              ("unexpected deferred type error: " <> Exception.displayException exception)
              ("DistributesAlias" `List.isInfixOf` Exception.displayException exception)
          NonLinear.Right _ ->
            assertFailure "expected a deferred type error"
    ]
