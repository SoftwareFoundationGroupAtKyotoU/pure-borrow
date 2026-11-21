{-# LANGUAGE LinearTypes #-}

module TokenUpcastRelabel where

import Control.Monad.Borrow.Pure.Lifetime.Token (Now)
import Data.Coerce.Directed (upcast)

-- This fixture must not typecheck: relabelling a Now would move a computation to another lifetime.
-- EXPECT: Couldn't match type
-- EXPECT: arising from a use of
relabel :: Now α %1 -> Now β
relabel = upcast
