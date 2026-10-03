{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeMultiplicity where

import Data.Coerce.Directed (upcast)
import Data.Coerce.Directed.Unsafe (deriveSubtype)
import Data.Proxy (Proxy (..))
import GHC.Exts (Multiplicity (..))

-- This fixture must not typecheck: a field stored linearly must not be read unrestricted after an upcast, which would let any linear value be used twice.
-- Without the multiplicity constraint, Proxy 'One <: Proxy 'Many holds through 'Data.Coerce.Coercible', since the role of Proxy's parameter is phantom.
-- EXPECT: Cannot satisfy: multiplicity Many <= One
data T p a where
  T :: a %p -> Proxy p -> T p a

deriveSubtype ''T

unrestrict :: T 'One a %1 -> T 'Many a
unrestrict = upcast
