{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeKindVariable where

import Data.Coerce.Directed.Unsafe (deriveSubtype)
import Data.Kind (Type)
import GHC.Exts (Any)

type G :: k -> Type
type family G x where
  G (x :: Bool) = Int
  G (x :: Ordering) = Double

-- This fixture must not typecheck: the first field of R mentions k, the kind of its parameter, which an instance could let differ between R 'True and R 'LT.
-- EXPECT: whose fields mention k, which is not a parameter of R
data R (x :: k) where
  RNil :: R x
  R :: forall k (x :: k). G (Any :: k) -> R x -> R x

deriveSubtype ''R
