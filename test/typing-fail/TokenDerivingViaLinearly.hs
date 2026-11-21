{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module TokenDerivingViaLinearly where

-- The token modules are imported whole, so that a constructor any of them exported would be in scope.
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe
import GHC.Exts (Any)

newtype Fake = Fake Any

-- This fixture must not typecheck: a type that is only representationally a token must not become LinearOnly through one.
-- EXPECT: Couldn't match representation of type
-- EXPECT: is not in scope
deriving via Linearly instance LinearOnly Fake
