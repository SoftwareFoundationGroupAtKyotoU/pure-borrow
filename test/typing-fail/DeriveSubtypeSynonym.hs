{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeSynonym where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: deriveSubtype takes a data or newtype declaration, not a synonym.
-- EXPECT: is a type synonym
type Pairs a = [(a, a)]

deriveSubtype ''Pairs
