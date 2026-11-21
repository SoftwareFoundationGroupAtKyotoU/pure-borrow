{-# LANGUAGE LinearTypes #-}

module TokenUpcastEnd where

import Control.Monad.Borrow.Pure.Lifetime.Token (EndToken, Now)
import Data.Coerce.Directed (upcast)

-- This fixture must not typecheck: a Now turned into the EndToken of its own lifetime would reclaim while borrows are live.
-- EXPECT: Couldn't match representation of type
-- EXPECT: is not in scope
endNow :: Now α %1 -> EndToken α
endNow = upcast
