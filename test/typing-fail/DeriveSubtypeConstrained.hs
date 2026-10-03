{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeConstrained where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: a constructor's constraint stores a dictionary, which an upcast would reinterpret at the target type.
-- EXPECT: with a constraint context
data Shown a where
  Shown :: (Show a) => a -> Shown a

deriveSubtype ''Shown
