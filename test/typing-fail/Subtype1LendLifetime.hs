{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}

module Subtype1LendLifetime where

import Control.Monad.Borrow.Pure.BO (Lend)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Data.Coerce.Directed (upcast1)

-- This fixture must not typecheck: a lender would be reclaimed before the static lifetime ends.
-- EXPECT: No instance for
-- EXPECT: <=!!
bad :: Lend Static Int %1 -> Lend (Al 0) Int
bad = upcast1
