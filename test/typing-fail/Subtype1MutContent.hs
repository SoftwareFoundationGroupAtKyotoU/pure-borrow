{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Subtype1MutContent where

import Control.Monad.Borrow.Pure.BO (Mut, Share)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Data.Coerce.Directed (upcast1)
import Generics.Linear (Generically1)
import Generics.Linear.TH (deriveGenericAnd1)

data MV α a = MV (Mut α a) Int

deriveGenericAnd1 ''MV

-- This fixture must not typecheck: a Mut of a static Share would let a Share of a shorter lifetime be written through it.
-- EXPECT: Mut is invariant in the type it borrows
bad :: MV α (Share Static Int) %1 -> Generically1 (MV α) (Share α Int)
bad = upcast1
