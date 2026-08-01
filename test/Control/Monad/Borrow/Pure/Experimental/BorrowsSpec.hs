{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Borrow.Pure.Experimental.BorrowsSpec (
  module Control.Monad.Borrow.Pure.Experimental.BorrowsSpec,
) where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Experimental.Borrows
import Control.Monad.Borrow.Pure.Experimental.Borrows.TypingCases
import Data.List qualified as List
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | A bundle of mutable borrows stays discardable; the owners survive it.
discardMutsBundle :: (Int, Int)
discardMutsBundle =
  linearly \linear ->
    runBO linear Control.do
      (mutA, lendA) <- borrowM (1 :: Int)
      (mutB, lendB) <- borrowM (2 :: Int)
      let !() = consume (mutA :- mutB :- BNil)
      pureAfter (reclaim lendA, reclaim lendB)

-- | The same for a bundle of shared borrows.
discardSharesBundle :: Int
discardSharesBundle =
  linearly \linear ->
    runBO linear Control.do
      (mut, lend) <- borrowM (3 :: Int)
      share mut & \(Ur shared) -> Control.do
        let !() = consume (shared :- BNil)
        pureAfter (reclaim lend)

test_bundleDiscarding :: TestTree
test_bundleDiscarding =
  testGroup
    "alias bundle discarding"
    [ testCase "a Muts bundle can be discarded without losing its owners" do
        discardMutsBundle @?= (1, 2)
    , testCase "a Shares bundle can be discarded without losing its owner" do
        discardSharesBundle @?= 3
    ]

{- |
Discarding a lender is rejected for bundles exactly as it is for a scalar 'Lend'.

@Lends@ used to be unconditionally @Affine@, which let safe code abandon a whole bundle of lenders and strand the owners it held.
These cases freeze the plural and the scalar behaviours as identical.
-}
test_lenderDiscardingIsRejected :: TestTree
test_lenderDiscardingIsRejected =
  testGroup
    "typing boundaries"
    [ expectDeferredTypeError
        "a Lends bundle is not Affine"
        badLendsAffCase
    , expectDeferredTypeError
        "a Lends bundle is not Consumable"
        badLendsConsumeCase
    , expectDeferredTypeError
        "a scalar Lend is not Affine"
        badLendAffCase
    , expectDeferredTypeError
        "a scalar Lend is not Consumable"
        badLendConsumeCase
    ]
  where
    -- Every case fails the same way: the alias kind is 'Lend, and both the scalar and the plural instances accept only a 'Borrow kind.
    expectedFragments = ["Couldn't match type", "Lend", "Borrow"]
    describeFragments = List.intercalate ", " expectedFragments
    expectDeferredTypeError description value =
      testCase description do
        result <- Exception.try @Exception.SomeException (Exception.evaluate value)
        case result of
          Left exception ->
            let rendered = Exception.displayException exception
             in assertBool
                  ("unexpected deferred type error: " <> rendered)
                  (List.all (`List.isInfixOf` rendered) expectedFragments)
          Right _ ->
            assertFailure
              ("expected deferred type error containing " <> describeFragments)
