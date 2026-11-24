{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}

module Subtype1FunArgument where

import Control.Monad.Borrow.Pure.BO (Mut)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Data.Coerce.Directed (upcast1)

-- This fixture must not typecheck: a function that needs a static borrow would accept one of a shorter lifetime.
-- EXPECT: No instance for
-- EXPECT: <=!!
bad :: (Mut Static Int %1 -> Int) %1 -> (Mut (Al 0) Int %1 -> Int)
bad = upcast1
