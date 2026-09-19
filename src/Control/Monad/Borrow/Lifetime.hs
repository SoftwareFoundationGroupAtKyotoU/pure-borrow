{-# LANGUAGE ExplicitNamespaces #-}

module Control.Monad.Borrow.Lifetime (
  type (/\),
  type (<=),
  type (>=),
  type Static,
  Lifetime,
) where

import Control.Monad.Borrow.Lifetime.Internal
