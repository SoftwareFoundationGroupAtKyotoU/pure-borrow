{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}

module Subtype1FunResult where

import Control.Monad.Borrow.Pure.BO (Mut)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Data.Coerce.Directed (upcast1)

-- This fixture must not typecheck: a function returning a borrow of a shorter lifetime would claim to return a static one.
-- EXPECT: No instance for
-- EXPECT: <=!!
bad :: (Int %1 -> Mut (Al 0) Int) %1 -> (Int %1 -> Mut Static Int)
bad = upcast1
