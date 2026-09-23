{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

{- |
This module provides __unsafe__ internals of "Control.Monad.Borrow.Pure.Lifetime.Token".
These are not meant to be used by end-users, so generally YOU SHOULD NOT import this module, and import "Control.Monad.Borrow.Pure.Lifetime.Token" instead.

This module is meant for library authors who want to build a new API on top of Pure Borrow.
This module provides internals of 'BO' and 'Alias', which can break the soundness guarded by the role system.
We __STRONGLY__ recommend to you to import only the needed parts of the definitions, and not to import everything or qualified.
-}
module Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (
  -- * Tokens

  {- | Each token is a constructor with a field that carries nothing, exported together with a pattern under the constructor's old name.
  The pattern builds a token and matches an unrestricted one; match a linearly bound token with the constructor, as in @\\('UnsafeLinearlyToken' _) -> ()@, since GHC does not let a pattern synonym match a linear value.

  A token built with the pattern or the constructor is a constant, and GHC merges two allocations that take equal arguments: two @Data.Ref.Linear.new seed 'UnsafeLinearly'@ in one function are one reference.
  Build a token only inside a function the optimizer cannot see into, one that is @NOINLINE@ and applied through 'GHC.Exts.noinline', as 'Control.Monad.Borrow.Pure.linearly' is.
  A function that takes a token apart and returns another must pass the field on, as in @\\('UnsafeLinearlyToken' field) -> 'UnsafeNowToken' field@.
  One that returns two tokens must itself be @NOINLINE@ and applied through 'GHC.Exts.noinline' instead, as 'Control.Monad.Borrow.Pure.dup' is: two tokens with the same field are one expression, and allocations made with them merge.
  -}
  Linearly (UnsafeLinearlyToken, UnsafeLinearly),
  Now (UnsafeNowToken, UnsafeNow),
  EndToken (UnsafeEndToken, UnsafeEnd),

  -- * Capabilities
  LinearOnly (..),
  LinearOnlyWitness (..),
  Ended (..),
  End,
) where

import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
