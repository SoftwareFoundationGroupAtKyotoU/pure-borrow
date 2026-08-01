{-# LANGUAGE CPP #-}

{- |
The build configuration the inspection obligations are stated against.

This is the only module in the component that looks at @PURE_BORROW_SLOW_SCOPES@.
An obligation about optimized Core is a statement about one of the two implementations the @slow@ flag selects, so rather than compiling such an obligation out under the other one, state it once and invert it with 'expectFailIfBecause'.
A test that is expected to fail is still a test: the day @+slow@ starts producing the same Core, the suite says so instead of staying quietly green.
-}
module PureBorrow.Inspection.Flags (
  isSlowAPI,
  expectFailIfBecause,
) where

import Test.Tasty (TestTree)
import Test.Tasty.ExpectedFailure (expectFailBecause)

-- | Whether this component was built against the @+slow@ library, i.e. the one whose sublifetime delimiters allocate a real runtime lifetime token.
isSlowAPI :: Bool
#ifdef PURE_BORROW_SLOW_SCOPES
isSlowAPI = True
#else
isSlowAPI = False
#endif

-- | Invert a test tree when the condition holds, recording why.
expectFailIfBecause :: Bool -> String -> TestTree -> TestTree
expectFailIfBecause False _ = id
expectFailIfBecause True reason = expectFailBecause reason
