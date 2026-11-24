{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StandaloneDeriving #-}

{- | 'withLinearly' forces the 'LinearOnly' witness before minting a 'Linearly'.

An instance whose method is missing, holds a deferred error, or loops because it was derived via its own type, must not yield a token.
-}
module Control.Monad.Borrow.Pure.Lifetime.TokenSpec (
  module Control.Monad.Borrow.Pure.Lifetime.TokenSpec,
) where

import Control.Exception (ErrorCall (..), SomeException, displayException, evaluate, try)
import Control.Monad.Borrow.Pure.Lifetime.Token (withLinearly)
import Control.Monad.Borrow.Pure.Lifetime.Token.TypingCases qualified as TypingCases
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (LinearOnly (..))
import Control.Monad.Borrow.Pure.Lifetime.TokenSpec.ClosedRun qualified as ClosedRun
import Control.Monad.Borrow.Pure.Lifetime.TokenSpec.Forced qualified as Forced
import Control.Monad.Borrow.Pure.Lifetime.TokenSpec.Strict qualified as Strict
import Data.List (intercalate, isInfixOf)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | An instance written through the @.Unsafe@ module whose witness is an error.
data Forced = Forced

instance LinearOnly Forced where
  linearOnly = errorWithoutStackTrace "forced"

-- | An instance derived via its own type, which passes every role check and has a self-referential witness.
data SelfDerived = SelfDerived

deriving via SelfDerived instance LinearOnly SelfDerived

test_withLinearly :: TestTree
test_withLinearly =
  testGroup
    "withLinearly forces the LinearOnly witness"
    [ testCase "an erroneous witness throws instead of minting a token" do
        result <- try (evaluate (withLinearly Forced))
        case result of
          Left (ErrorCall message) -> message @?= "forced"
          Right _ -> assertFailure "withLinearly minted a token from an erroneous witness"
    , testCase "a self-derived witness does not mint a token" do
        -- It either loops until the timeout or is detected as a loop; both are fine.
        result <- timeout 2_000_000 (try @SomeException (evaluate (withLinearly SelfDerived)))
        case result of
          Just (Right _) -> assertFailure "withLinearly minted a token from a self-derived witness"
          _ -> pure ()
    ]

test_forcedTokens :: TestTree
test_forcedTokens =
  testGroup
    "a forced Linearly still allocates a fresh resource"
    [ testCase "two references from two forced tokens are distinct" do
        Forced.twoRefs 5 @?= (6, 5)
    , testCase "two references from two unforced tokens are distinct" do
        Forced.twoRefsUnforced 5 @?= (6, 5)
    , testCase "a reference allocated from a forced token is fresh on every call" do
        Forced.callTwiceRef @?= (0, 0)
    , testCase "a vector allocated from a forced token is fresh on every call" do
        Forced.callTwiceVector @?= (0, 0)
    , testCase "two vectors from two forced tokens are distinct" do
        Forced.twoVectors 5 @?= (6, 5)
    , testCase "two references in a Strict module are distinct" do
        Strict.twoRefsStrict 5 @?= (6, 5)
    , testCase "two references from records with a strict token field are distinct" do
        Strict.twoRefsEnv 5 @?= (6, 5)
    ]

test_closedRuns :: TestTree
test_closedRuns =
  testGroup
    "a run of BO whose action has no free variables allocates afresh"
    [ testCase "two references from two runBO_ calls are distinct" do
        ClosedRun.twoMkRef 1 @?= (1, 0)
    , testCase "two references from two runBO calls are distinct" do
        ClosedRun.twoMkRefAfter 1 @?= (1, 0)
    , testCase "two references from two inlined runs are distinct" do
        ClosedRun.twoMkRefInline 1 @?= (1, 0)
    , testCase "two vectors from two runs are distinct" do
        ClosedRun.twoMkVector 5 @?= (5, 0)
    , testCase "a reference allocated by a run is fresh on every call" do
        ClosedRun.callTwiceMkRef @?= (0, 0)
    , testCase "a reference allocated between newLifetime and endLifetime is fresh on every call" do
        ClosedRun.callTwiceNewLifetime @?= (0, 0)
    ]

test_tokenConversions :: TestTree
test_tokenConversions =
  testGroup
    "no token converts into another or into another lifetime, with every token module imported"
    [ expectDeferredTypeError "coerce refuses Any to Linearly" hiddenConstructor TypingCases.badCoerceAny
    , expectDeferredTypeError "coerce refuses EndToken to Linearly" hiddenConstructor TypingCases.badCoerceEndToLinearly
    , expectDeferredTypeError "coerce refuses Now to EndToken" hiddenConstructor TypingCases.badCoerceNowToEnd
    , expectDeferredTypeError "upcast refuses Now to EndToken" hiddenConstructor TypingCases.badUpcastNowToEnd
    , expectDeferredTypeError "coerce refuses relabelling a Now" relabelled TypingCases.badCoerceNowRelabel
    , expectDeferredTypeError "upcast refuses relabelling a Now" relabelled TypingCases.badUpcastNowRelabel
    , expectDeferredTypeError "coerce refuses relabelling an EndToken" relabelled TypingCases.badCoerceEndRelabel
    ]
  where
    hiddenConstructor = ["Couldn't match representation of type", "is not in scope"]
    relabelled = ["Couldn't match type", "arising from a use of"]
    expectDeferredTypeError :: String -> [String] -> a -> TestTree
    expectDeferredTypeError description expectedFragments value =
      testCase description do
        result <- try @SomeException (evaluate value)
        case result of
          Left exception ->
            let rendered = displayException exception
             in assertBool
                  ("unexpected deferred type error: " <> rendered)
                  (all (`isInfixOf` rendered) expectedFragments)
          Right _ ->
            assertFailure ("expected deferred type error containing " <> intercalate ", " expectedFragments)
