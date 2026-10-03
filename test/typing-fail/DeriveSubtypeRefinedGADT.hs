{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeRefinedGADT where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: a GADT constructor whose result refines the parameter would be reinterpreted at another index.
-- EXPECT: whose result is not
data Code a where
  IntCode :: Int -> Code Int

deriveSubtype ''Code
