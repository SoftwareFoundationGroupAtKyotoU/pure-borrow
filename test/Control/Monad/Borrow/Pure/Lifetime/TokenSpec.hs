{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-missing-methods -Wno-orphans #-}

module Control.Monad.Borrow.Pure.Lifetime.TokenSpec (
  module Control.Monad.Borrow.Pure.Lifetime.TokenSpec,
) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.BO
import Control.Monad.Borrow.Lifetime.Internal (Al)
import Data.List (isInfixOf)
import Prelude.Linear qualified as Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

-- Only the lifetime's type-level tag is internal; constructing a forged End instance needs no token constructor.
instance End (Al 987654321)

forgedReclaim :: Linear.Ur Int
forgedReclaim = linearly \lin ->
  case borrow @(Al 987654321) (Linear.Ur 7) lin of
    (mut, lend) -> Linear.consume mut `Linear.lseq` reclaim lend

forgedUnAfter :: Linear.Ur Int
forgedUnAfter = unAfter @(Al 987654321) (After (Linear.Ur 7))

assertFailsWith :: String -> a -> Assertion
assertFailsWith expected value = do
  result <- try (evaluate value)
  case result of
    Left (exception :: SomeException) ->
      assertBool ("unexpected exception: " <> displayException exception) $
        expected `isInfixOf` displayException exception
    Right _ -> assertFailure "the protected value escaped without an end token"

test_endWitness :: TestTree
test_endWitness =
  testGroup
    "End witnesses are evaluated"
    [ testCase "reclaim rejects a bodiless End instance" $
        assertFailsWith "endToken" forgedReclaim
    , testCase "unAfter rejects a bodiless End instance" $
        assertFailsWith "endToken" forgedUnAfter
    , testCase "withEnd evaluates its token even for a constant payload" $
        assertFailsWith "missing end token" $
          withEnd @(Al 987654322) (error "missing end token") (After (Linear.Ur (7 :: Int)))
    , testCase "a genuine end token still releases a lender" $
        Linear.unur
          ( linearly
              ( \lin -> runBOLend lin Control.do
                  (mut, lend) <- borrowM (Linear.Ur (7 :: Int))
                  Linear.consume mut `Linear.lseq` Control.pure lend
              )
          )
          @?= 7
    ]
