{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeKindOfParameter where

import Data.Coerce.Directed (upcast)
import Data.Coerce.Directed.Unsafe (deriveSubtype)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.Exts (Any)

type Pick :: Bool -> Type
type family Pick b where
  Pick 'True = Bool
  Pick 'False = Ordering

type G :: k -> Type
type family G x where
  G (x :: Bool) = Int
  G (x :: Ordering) = Double

-- This fixture must not typecheck: b occurs in the second field only inside a kind, through which that field is an Int in K 'True and a Double in K 'False.
-- EXPECT: Couldn't match
data K (b :: Bool) = K (Proxy b) (G (Any :: Pick b))

deriveSubtype ''K

change :: K 'True %1 -> K 'False
change = upcast
