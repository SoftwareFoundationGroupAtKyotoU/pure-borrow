{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}

module Subtype1BOLifetime where

import Control.Monad.Borrow.Pure.BO (BO)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Data.Coerce.Directed (upcast1)

-- This fixture must not typecheck: a computation of a shorter lifetime would run as one of the static lifetime.
-- EXPECT: No instance for
-- EXPECT: <=!!
bad :: BO (Al 0) Int %1 -> BO Static Int
bad = upcast1
