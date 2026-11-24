{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}

module Subtype1MultiplicityOneToMany where

import Data.Coerce.Directed (upcast1)
import GHC.Exts (Multiplicity (..))
import Generics.Linear (MP1, Par1)

-- This fixture must not typecheck: a field held linearly would be read back as unrestricted.
-- EXPECT: No instance for
-- EXPECT: Many Par1
bad :: MP1 'One Par1 Int %1 -> MP1 'Many Par1 Int
bad = upcast1
