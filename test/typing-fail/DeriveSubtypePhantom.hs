{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypePhantom where

import Data.Coerce.Directed (upcast)
import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: deriveSubtype keeps fixed a parameter that no field mentions.
-- Its nominal role keeps 'Data.Coerce.Coercible' from relating two instantiations instead.
-- EXPECT: Couldn't match type
data Tagged t a = Tagged a

type role Tagged nominal representational

deriveSubtype ''Tagged

retag :: Tagged Int Bool %1 -> Tagged Char Bool
retag = upcast
