{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Coerce.Directed.Internal (module Data.Coerce.Directed.Internal) where

import Data.Coerce (Coercible)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import GHC.Base (Multiplicity (..))
import GHC.TypeError (Assert, ErrorMessage (..), TypeError, Unsatisfiable, unsatisfiable)
import Generics.Linear
import Prelude.Linear
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Linear qualified as Unsafe

infix 4 <:

{- | Whether a function of multiplicity @p@ can stand in for one of multiplicity @q@: a linear function can be used where an unrestricted one is expected, never the other way round.

The equations are pairwise compatible, so each legitimate shape reduces even when a multiplicity is a variable: @m <= Many@, @One <= m@ and @m <= m@.
-}
type MultLe :: Multiplicity -> Multiplicity -> Bool
type family MultLe p q where
  MultLe p 'Many = 'True
  MultLe 'One q = 'True
  MultLe p p = 'True
  MultLe 'Many 'One = 'False

-- | @p <= q@ on multiplicities, with an error that says which order is meant.
type MultiplicityLe :: Multiplicity -> Multiplicity -> Constraint
type MultiplicityLe p q =
  Assert
    (MultLe p q)
    ( TypeError
        ( 'Text "Cannot satisfy: multiplicity "
            ':<>: 'ShowType p
            ':<>: 'Text " <= "
            ':<>: 'ShowType q
            ':$$: 'Text "The upcast needs a function of multiplicity "
            ':<>: 'ShowType p
            ':<>: 'Text " to stand in for one of multiplicity "
            ':<>: 'ShowType q
            ':<>: 'Text ", which is not known to be possible:"
            ':$$: 'Text "a linear function can stand in for an unrestricted one, but not the other way round."
            ':$$: 'Text "That function may be a part of the value, such as an argument of a function, where the direction is reversed."
            ':$$: 'Text "Where the multiplicities are variables, require the upcast itself in the signature, as in ((a %p -> b) <: (a %q -> b))."
        )
    )

data SubtypeWitness a b = UnsafeSubtype

type role SubtypeWitness nominal representational

{- | @a <: b@: a value of @a@ can be used as a value of @b@, through zero-cost 'upcast'.
Mainly used to coerce types containing lifetimes, such as @t'Control.Monad.Borrow.Pure.BO' α@ or @t'Control.Monad.Borrow.Pure.Mut' α@, properly.
You can use 'AsCoercible' with the @DerivingVia@ extension to derive the upcast relation between coercible types.
For a data type of your own, 'Data.Coerce.Directed.Unsafe.deriveSubtype' derives it field by field; read its caveat first.
-}
class a <: b where
  subtype :: SubtypeWitness a b

upcast :: (a <: b) => a %1 -> b
upcast = Unsafe.toLinear unsafeCoerce

instance {-# INCOHERENT #-} (Coercible a b) => a <: b where
  subtype = UnsafeSubtype

{- | A target for deriving '(<:)' between representationally equal types, for use where their representation is hidden, such as outside the module that defines a newtype.

In that module, write @deriving via 'AsCoercible' T instance S '<:' T@.
-}
newtype AsCoercible a = AsCoercible {runAsCoercible :: a}

instance (Coercible a b) => a <: AsCoercible b where
  subtype = UnsafeSubtype

instance (a <: b) => [a] <: [b] where
  subtype = UnsafeSubtype

instance (a <: b) => Maybe a <: Maybe b where
  subtype = UnsafeSubtype

instance (a <: b) => NonEmpty a <: NonEmpty b where
  subtype = UnsafeSubtype

instance (a <: a', b <: b') => (a, b) <: (a', b') where
  subtype = UnsafeSubtype

instance (a <: a', b <: b') => Either a b <: Either a' b' where
  subtype = UnsafeSubtype

instance (a <: a', b <: b', c <: c') => (a, b, c) <: (a', b', c') where
  subtype = UnsafeSubtype

instance
  (a' <: a, b <: b', MultiplicityLe p q) =>
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

-- 'genericUpcast' stays: unlike 'upcast', it calls 'from' and 'to'.
instance
  ( Unsatisfiable
      ( 'Text "(<:) can no longer be derived via Generically: it trusted the Rep of a Generic instance, which anyone can write by hand."
          ':$$: 'Text "Derive the instance with deriveSubtype from Data.Coerce.Directed.Unsafe, which reads the declaration itself; read its caveat about mutable data structures first."
          ':$$: 'Text "genericUpcast converts a value without an instance."
      )
  ) =>
  a <: Generically b
  where
  subtype = unsatisfiable

genericUpcast :: (GenericSubtype a b) => a %1 -> b
genericUpcast = to . gupcast . from

instance '[] <: ('[] :: [k]) where
  subtype = UnsafeSubtype

instance (a <: b, as <: bs) => (a ': as) <: (b ': bs) where
  subtype = UnsafeSubtype
