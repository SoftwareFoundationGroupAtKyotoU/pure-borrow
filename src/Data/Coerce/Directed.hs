{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Coerce.Directed (
  SubtypeWitness (UnsafeSubtype),
  type (<:) (..),
  upcast,
  AsCoercible (..),
  GSubtype (..),
  GenericSubtype,
  genericUpcast,
) where

import Data.Coerce (Coercible)
import Data.Kind (Constraint, Type)
import Data.Type.Ord
import GHC.Base (Multiplicity (..))
import Generics.Linear
import Prelude.Linear
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Linear qualified as Unsafe

infix 4 <:

-- Orphan instance!
type instance Compare (a :: Multiplicity) (b :: Multiplicity) = CmpMult One Many

type CmpMult :: Multiplicity -> Multiplicity -> Ordering
type family CmpMult p q where
  CmpMult One One = EQ
  CmpMult One Many = LT
  CmpMult Many One = GT
  CmpMult Many Many = EQ

data SubtypeWitness a b = UnsafeSubtype

class a <: b where
  subtype :: SubtypeWitness a b

upcast :: (a <: b) => a %1 -> b
upcast = Unsafe.toLinear unsafeCoerce

instance {-# INCOHERENT #-} (Coercible a b) => a <: b where
  subtype = UnsafeSubtype

newtype AsCoercible a = AsCoercible {runAsCoercible :: a}

instance (Coercible a b) => a <: AsCoercible b where
  subtype = UnsafeSubtype

deriving via
  Generically [b]
  instance
    (a <: b) => [a] <: [b]

deriving via
  Generically (a', b')
  instance
    (a <: a', b <: b') => (a, b) <: (a', b')

deriving via
  Generically (Either a' b')
  instance
    (a <: a', b <: b') => Either a b <: Either a' b'

deriving via
  Generically (a', b', c')
  instance
    (a <: a', b <: b', c <: c') => (a, b, c) <: (a', b', c')

instance
  (a' <: a, b <: b', p Data.Type.Ord.<= q) =>
  (a %p -> b) <: (a' %q -> b')
  where
  subtype = UnsafeSubtype

type GSubtype :: (k -> Type) -> (k -> Type) -> Constraint
class GSubtype f g where
  gsubtype :: SubtypeWitness f g

gupcast :: (GSubtype f g) => f a %1 -> g a
gupcast = Unsafe.toLinear unsafeCoerce

instance (a <: b) => GSubtype (K1 i a) (K1 i b) where
  gsubtype = UnsafeSubtype

instance {-# INCOHERENT #-} GSubtype f f where
  gsubtype = UnsafeSubtype

instance (GSubtype f g) => GSubtype (MP1 p f) (MP1 p g) where
  gsubtype = UnsafeSubtype

instance (GSubtype f g) => GSubtype (M1 i c f) (M1 i c g) where
  gsubtype = UnsafeSubtype

instance (GSubtype f f', GSubtype g g') => GSubtype (f :*: g) (f' :*: g') where
  gsubtype = UnsafeSubtype

instance (GSubtype l l', GSubtype r r') => GSubtype (l :+: r) (l' :+: r') where
  gsubtype = UnsafeSubtype

type GenericSubtype a b = (Generic a, Generic b, GSubtype (Rep a) (Rep b))

instance (GenericSubtype a b) => a <: Generically b where
  subtype = UnsafeSubtype

genericUpcast :: (GenericSubtype a b) => a %1 -> b
genericUpcast = to . gupcast . from
