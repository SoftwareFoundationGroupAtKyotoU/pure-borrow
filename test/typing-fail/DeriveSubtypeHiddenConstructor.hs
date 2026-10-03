{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeHiddenConstructor where

import Data.Coerce.Directed.Unsafe (deriveSubtype)
import Data.Map (Map)

-- This fixture must not typecheck: Data.Map does not export the constructors of Map, so its invariants are out of reach of deriveSubtype.
-- EXPECT: needs its constructor
deriveSubtype ''Map
