{- |
This module provides __unsafe__ internals of "Control.Monad.Borrow.Pure.Affine".
These are not meant to be used by end-users, so generally YOU SHOULD NOT import this module, and import "Control.Monad.Borrow.Pure.Affine" instead.

This module is meant for library authors who want to build a new API on top of Pure Borrow.
This module provides internals of 'BO' and 'Alias', which can break the soundness guarded by the role system.
We __STRONGLY__ recommend to you to import only the needed parts of the definitions, and not to import everything or qualified.
-}
module Control.Monad.Borrow.Pure.Affine.Unsafe (
  -- * Affine Modality
  Aff (..),
) where

import Control.Monad.Borrow.Pure.Affine.Internal
