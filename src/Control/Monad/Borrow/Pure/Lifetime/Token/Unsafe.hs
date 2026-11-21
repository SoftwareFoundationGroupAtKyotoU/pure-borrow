{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

{- |
This module provides __unsafe__ internals of "Control.Monad.Borrow.Pure.Lifetime.Token".
These are not meant to be used by end-users, so generally YOU SHOULD NOT import this module, and import "Control.Monad.Borrow.Pure.Lifetime.Token" instead.

This module is meant for library authors who want to build a new API on top of Pure Borrow.
This module provides the patterns that build the tokens and the methods of 'LinearOnly' and 'End', which can break the soundness that the tokens guard.
We __STRONGLY__ recommend to you to import only the needed parts of the definitions, and not to import everything or qualified.
-}
module Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (
  -- * Tokens
  Linearly (UnsafeLinearly),
  Now (UnsafeNow),
  EndToken (UnsafeEnd),

  -- * Capabilities
  LinearOnly (..),
  LinearOnlyWitness (..),
  End (..),
) where

import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
