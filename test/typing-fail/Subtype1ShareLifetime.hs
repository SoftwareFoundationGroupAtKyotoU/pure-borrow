{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}

module Subtype1ShareLifetime where

import Control.Monad.Borrow.Pure.BO (Share)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Data.Coerce.Directed (upcast1)

-- This fixture must not typecheck: a shared borrow would outlive its lifetime.
-- EXPECT: No instance for
-- EXPECT: <=!!
bad :: Share (Al 0) Int %1 -> Share Static Int
bad = upcast1
