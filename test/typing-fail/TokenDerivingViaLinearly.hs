{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module TokenDerivingViaLinearly where

import Control.Monad.Borrow.Pure.Lifetime.Token (LinearOnly, Linearly)
import GHC.Exts (Any)

newtype Fake = Fake Any

-- This fixture must not typecheck: a type that is only representationally a token must not become LinearOnly through one.
-- EXPECT: Couldn't match representation of type
-- EXPECT: is not in scope
deriving via Linearly instance LinearOnly Fake
