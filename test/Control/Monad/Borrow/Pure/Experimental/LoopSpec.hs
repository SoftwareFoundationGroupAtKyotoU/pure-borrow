{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Borrow.Pure.Experimental.LoopSpec (
  module Control.Monad.Borrow.Pure.Experimental.LoopSpec,
) where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Experimental.Loop (foldBorrow, foldBorrowVia, traverseBorrowOf_)
import Control.Monad.Borrow.Pure.Experimental.Loop qualified as Loop
import Control.Monad.Borrow.Pure.Experimental.Loop.TypingCases
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Generics.Linear (Generically1 (..))
import Generics.Linear.TH (deriveGeneric1)
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
    , testCase "foldBorrow needs a DistributesAlias instance" do
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

sumBorrowedPair :: Int
{-# NOINLINE sumBorrowedPair #-}
sumBorrowedPair = linearly \lin -> runBO lin Control.do
  (pair, lend) <- borrowM (1 :: Int, 2 :: Int)
  let !(Sum total) = foldBorrowVia (\b -> case splitPair b of (x, y) -> [x, y]) (\element -> Sum (copy element)) pair
  pureAfter (consume (reclaim lend) `lseq` total)

visitNested :: [Int]
{-# NOINLINE visitNested #-}
visitNested = linearly \lin -> runBO lin Control.do
  (nested, lend) <- borrowM [[1, 2], [3, 4 :: Int]]
  let !(Ur shared) = share nested
      !(Ur seen) =
        Control.execState
          ( traverseBorrowOf_
              (\k -> foldBorrow (foldBorrow k))
              (\element -> case move (copy element) of Ur x -> Control.modify \(Ur xs) -> Ur (x : xs))
              shared
          )
          (Ur [])
  pureAfter (consume (reclaim lend) `lseq` NonLinear.reverse seen)

visitInBO :: [Int]
{-# NOINLINE visitInBO #-}
visitInBO = linearly \lin -> runBO lin Control.do
  (list, lend) <- borrowM [1, 2, 3 :: Int]
  Ur seen <-
    Control.execStateT
      (traverseBorrowOf_ foldBorrow (\element -> case move (copy element) of Ur x -> Control.modify \(Ur xs) -> Ur (x : xs)) list)
      (Ur [])
  pureAfter (consume (reclaim lend) `lseq` NonLinear.reverse seen)

test_foldBorrowVia :: TestTree
test_foldBorrowVia =
  testGroup
    "foldBorrowVia and traverseBorrowOf_"
    [ testCase "folds the borrows a splitter makes" do
        sumBorrowedPair @?= 3
    , testCase "visits each element of a composed fold once, in order" do
        visitNested @?= [1, 2, 3, 4]
    , testCase "runs an action over BO on each Mut, once, in order" do
        visitInBO @?= [1, 2, 3]
    ]

-- A field of the parameter, of a functor of it, and of compositions of functors.
data Fields a = Fields a [a] (Maybe [a]) [[a]]

deriveGeneric1 ''Fields

deriving via Generically1 Fields instance Loop.Foldable Fields

test_genericFoldable :: TestTree
test_genericFoldable =
  testCase "Foldable derives via Generically1 for fields a, f a and f (g a)" do
    Loop.toList (Fields 1 [2, 3] (Just [4]) [[5, 6], [7 :: Int]]) @?= [1, 2, 3, 4, 5, 6, 7]
