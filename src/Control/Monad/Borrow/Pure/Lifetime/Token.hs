{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Control.Monad.Borrow.Pure.Lifetime.Token (
  Linearly (),
  Now (),
  End (),
  LinearOnly,
  withLinearly,
  withLinearly#,
  endLifetime,
  SomeNow (..),
  newLifetime,
  nowStatic,
  neverEnds,
  alreadyEnded,
) where

import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
