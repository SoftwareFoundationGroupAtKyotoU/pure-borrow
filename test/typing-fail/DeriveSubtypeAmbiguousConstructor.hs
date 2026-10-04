{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeAmbiguousConstructor where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: Left names both this module's constructor and the Prelude's, and the message says to hide the Prelude's.
-- EXPECT: whose name is ambiguous where it is spliced: hide the imported Left
data Choice a = Left a | Right Int

deriveSubtype ''Choice
