{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeNoParameters where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: a type without parameters has no two instantiations to relate.
-- EXPECT: has no parameters
data Point = Point Int Int

deriveSubtype ''Point
