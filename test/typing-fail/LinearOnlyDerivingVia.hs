{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module LinearOnlyDerivingVia where

import Control.Monad.Borrow.Pure.Lifetime.Token (LinearOnly, Linearly)

data Dummy = Dummy

-- This fixture must not typecheck: coercing 'Linearly''s instance would give
-- the freely constructible 'Dummy' a 'LinearOnly' instance without any
-- warning, and 'Control.Monad.Borrow.Pure.withLinearly' would then mint an
-- unrestricted 'Linearly' from it.
-- EXPECT: Couldn't match representation of type
-- EXPECT: Linearly
-- EXPECT: Dummy
deriving via Linearly instance LinearOnly Dummy
