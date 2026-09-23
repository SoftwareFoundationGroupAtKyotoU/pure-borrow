{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module LinearOnlyAnyclass where

import Control.Monad.Borrow.Pure.Lifetime.Token (LinearOnly)

-- This fixture must not typecheck: @deriving anyclass@ uses the method's
-- default, which is an unsatisfiable constraint.
-- EXPECT: A LinearOnly instance must come from a type that already has one.
data Dummy = Dummy
  deriving anyclass (LinearOnly)
