{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module LinearOnlyDerivingVia where

-- The token modules are imported whole, so that a constructor any of them exported would be in scope.
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe
import GHC.Exts (Any)

data Dummy = Dummy

newtype Fake = Fake Any

-- This fixture must not typecheck: coercing 'Linearly''s instance would give the freely constructible 'Dummy' a 'LinearOnly' instance without any warning, and 'Control.Monad.Borrow.Pure.withLinearly' would then mint an unrestricted 'Linearly' from it.
-- Nor may 'Fake', which is only representationally a token, become 'LinearOnly' through one.
-- EXPECT: Couldn't match representation of type
-- EXPECT: is not in scope
-- EXPECT: instance LinearOnly Dummy
-- EXPECT: instance LinearOnly Fake
deriving via Linearly instance LinearOnly Dummy

deriving via Linearly instance LinearOnly Fake
