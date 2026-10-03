{-# LANGUAGE BlockArguments #-}

{- | Owners handed back after a scope, with GHC's state hack switched off.

With the state hack, GHC does not float code out of a state-threaded lambda, so an owner read in a scope's 'Control.Monad.Borrow.Pure.After' stays after the scope's writes by its position alone.
Without it, only a data dependency on the end of the scope keeps it there, which is what Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal" provides.
A user may compile with @-fno-state-hack@, and may force the token that 'Control.Monad.Borrow.Pure.Lifetime.Token.endLifetime' hands out unrestricted; the library must stay correct either way.
The last two groups force 'Control.Monad.Borrow.Pure.Linearly' tokens before allocating with them, which one of those kernels got wrong only under this flag, and run 'Control.Monad.Borrow.Pure.BO' over actions with no free variables (Note [Tokens carry a field] in "Control.Monad.Borrow.Pure.Lifetime.Token.Internal").

This is a component of its own, built with @-fno-state-hack@ for every module, because the flag did not take effect when set in one module's @OPTIONS_GHC@ of the main test suite: there the same kernels stayed green against a library that the kernels fail here.
GHC does not recompile a module when only this flag changes, so a build directory that predates the flag must be cleaned before these tests can be trusted.
-}
module Main (main) where

import PureBorrow.NoStateHack.ClosedRun qualified as ClosedRun
import PureBorrow.NoStateHack.ForcedLinearly qualified as ForcedLinearly
import PureBorrow.NoStateHack.ForcedToken qualified as ForcedToken
import PureBorrow.NoStateHack.Owners qualified as Owners
import PureBorrow.NoStateHack.Scopes qualified as Scopes
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "an owner reclaimed after a scope sees the scope's writes, under -fno-state-hack"
      [ testGroup
          "the owner kernels"
          [ testCase path (result @?= (1, 0))
          | (path, result) <- Owners.ownerKernels
          ]
      , testGroup "the scopes" $
          [ testCase path (kernel 0 @?= 1)
          | (path, kernel) <- Scopes.kernels
          ]
            <> [ testCase "an owner taken out in srunBO is handed back once" do
                   Scopes.ownerTwice 0 @?= (100, 1)
               , testCase "an owner taken out and freed in srunBO is not handed back" do
                   Scopes.ownerGivenAway 0 @?= 100
               ]
      , testGroup "a scope written by the user that forces the end token" $
          [ testCase path (result @?= (1, 0))
          | (path, result) <- ForcedToken.kernels
          ]
            <> [ testCase "an owner taken out and freed in the scope is not handed back" do
                   ForcedToken.givenAway @?= (100, 0)
               ]
      , testGroup
          "a forced Linearly still allocates a fresh resource"
          [ testCase "two references from two forced tokens are distinct" do
              ForcedLinearly.twoRefs 5 @?= (6, 5)
          , testCase "two references from two unforced tokens are distinct" do
              ForcedLinearly.twoRefsUnforced 5 @?= (6, 5)
          , testCase "a reference allocated from a forced token is fresh on every call" do
              ForcedLinearly.callTwiceRef @?= (0, 0)
          , testCase "a vector allocated from a forced token is fresh on every call" do
              ForcedLinearly.callTwiceVector @?= (0, 0)
          , testCase "two vectors from two forced tokens are distinct" do
              ForcedLinearly.twoVectors 5 @?= (6, 5)
          ]
      , testGroup
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
      ]
