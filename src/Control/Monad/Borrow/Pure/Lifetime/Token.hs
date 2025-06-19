{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Control.Monad.Borrow.Pure.Lifetime.Token (
  Linearly (),
  Now (),
  End (),
  LinearOnly,
  withLinearly,
  endLifetime,
  SomeNow (..),
  newLifetime,
  nowStatic,
  absurdEndStatic,
) where

import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
