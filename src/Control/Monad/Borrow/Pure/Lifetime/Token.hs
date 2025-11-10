{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Control.Monad.Borrow.Pure.Lifetime.Token (
  Linearly (),
  linearly,
  Now (),
  End (),
  LinearOnly,
  withLinearly,
  withLinearly#,
  endLifetime,
  SomeNow (..),
  newLifetime,
  newLifetime',
  nowStatic,
  neverEnds,
) where

import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
