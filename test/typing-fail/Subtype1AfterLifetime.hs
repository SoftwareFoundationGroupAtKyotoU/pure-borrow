{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}

module Subtype1AfterLifetime where

import Control.Monad.Borrow.Pure.BO (After)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Data.Coerce.Directed (upcast1)

-- This fixture must not typecheck: a finalizer would be discharged before the static lifetime ends.
-- EXPECT: No instance for
-- EXPECT: <=!!
bad :: After Static Int %1 -> After (Al 0) Int
bad = upcast1
