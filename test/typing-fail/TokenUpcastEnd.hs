{-# LANGUAGE LinearTypes #-}

module TokenUpcastEnd where

-- The token modules are imported whole, so that a constructor any of them exported would be in scope.
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe
import Data.Coerce.Directed (upcast)

-- This fixture must not typecheck: a Now turned into the EndToken of its own lifetime would reclaim while borrows are live.
-- EXPECT: Couldn't match representation of type
-- EXPECT: is not in scope
endNow :: Now α %1 -> EndToken α
endNow = upcast
