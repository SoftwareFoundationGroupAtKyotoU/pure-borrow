{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-methods -Wno-orphans #-}

module Control.Monad.Borrow.IO.ForgedImpureSpec (
  module Control.Monad.Borrow.IO.ForgedImpureSpec,
) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad.Borrow.BO
import Control.Monad.IO.Class.Linear (liftSystemIOU)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- Keep this instance isolated from the missing-instance cases in IO.TypingCases.
instance Impure Pure

test_forgedImpure :: TestTree
test_forgedImpure = testCase "a bodiless Impure Pure instance cannot run effects" do
  happened <- newIORef False
  result <- try @SomeException $
    evaluate $
      linearly \lin -> runBO_ lin (liftSystemIOU (writeIORef happened True))
  case result of
    Left exception ->
      assertBool ("unexpected exception: " <> displayException exception) $
        "liftLinIO" `isInfixOf` displayException exception
    Right _ -> assertFailure "a forged Impure Pure instance ran successfully"
  readIORef happened >>= (@?= False)
