{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Coerce.Directed (
  type (<:),
  AsCoercible (..),
  GenericUpcast,
  genericUpcast,
) where

import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Data.Coerce (Coercible)
import Data.Kind (Constraint, Type)
import Data.Type.Ord
import GHC.Base (Multiplicity (..))
import Generics.Linear
import Prelude.Linear
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

class a <: b where
  upcast :: a %1 -> b

instance {-# INCOHERENT #-} (Coercible a b) => a <: b where
  upcast = coerceLin
  {-# INLINE upcast #-}

newtype AsCoercible a = AsCoercible {runAsCoercible :: a}

instance (Coercible a b) => a <: AsCoercible b where
  upcast = coerceLin
  {-# INLINE upcast #-}

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
  {-# INLINE upcast #-}
  upcast f = Unsafe.toLinear \a -> upcast (f (upcast a))

type GUpcast :: (k -> Type) -> (k -> Type) -> Constraint
class GUpcast f g where
  gupcast :: f a %1 -> g a

instance (a <: b) => GUpcast (K1 i a) (K1 i b) where
  gupcast (K1 a) = K1 (upcast a)
  {-# INLINE gupcast #-}

instance {-# INCOHERENT #-} GUpcast f f where
  gupcast = id
  {-# INLINE gupcast #-}

instance (GUpcast f g) => GUpcast (MP1 p f) (MP1 p g) where
  gupcast (MP1 f) = MP1 (gupcast f)
  {-# INLINE gupcast #-}

instance (GUpcast f g) => GUpcast (M1 i c f) (M1 i c g) where
  gupcast (M1 f) = M1 (gupcast f)
  {-# INLINE gupcast #-}

instance (GUpcast f f', GUpcast g g') => GUpcast (f :*: g) (f' :*: g') where
  gupcast (f :*: g) = gupcast f :*: gupcast g
  {-# INLINE gupcast #-}

instance (GUpcast l l', GUpcast r r') => GUpcast (l :+: r) (l' :+: r') where
  gupcast (L1 f) = L1 $ gupcast f
  gupcast (R1 g) = R1 $ gupcast g
  {-# INLINE gupcast #-}

type GenericUpcast a b = (Generic a, Generic b, GUpcast (Rep a) (Rep b))

instance (GenericUpcast a b) => a <: Generically b where
  upcast = Generically . genericUpcast
  {-# INLINE upcast #-}

genericUpcast :: (GenericUpcast a b) => a %1 -> b
genericUpcast = to . gupcast . from
