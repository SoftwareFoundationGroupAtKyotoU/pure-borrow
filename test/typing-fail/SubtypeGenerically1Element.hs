{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}

module SubtypeGenerically1Element where

import Control.Monad.Borrow.Pure.BO (Lend, Share)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Data.Coerce.Directed (upcast)
import Generics.Linear (Generically1)

-- This fixture must not typecheck: a lender under a Share would be reclaimable before the static lifetime ends.
-- EXPECT: No instance for
-- EXPECT: <=!!
bad :: Share Static (Lend Static Int) %1 -> Generically1 (Share Static) (Lend (Al 0) Int)
bad = upcast
