{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Coerce.Directed.Internal (module Data.Coerce.Directed.Internal) where

import Data.Coerce (Coercible)
import Data.Functor.Identity (Identity)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import GHC.Exts (FUN, Multiplicity (..))
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
You can derive '(<:)' via t'AsCoercible' between representationally equal types, via t'Generics.Linear.Generically' between two applications of one type constructor, or via t'Generics.Linear.Generically1' together with 'Subtype1'.
Deriving via t'Generics.Linear.Generically' needs one instance of linear-generics' 'Generic' that serves every choice of the arguments from the first one that differs to the last, at most eight trailing arguments, and that only takes a value apart and puts it back, as the Template Haskell of @linear-generics@ generates.
-}
class a <: b where
  subtype :: SubtypeWitness a b

upcast :: (a <: b) => a %1 -> b
upcast = Unsafe.toLinear unsafeCoerce

{- | Lifting of '(<:)' to unary type constructors, analogous to @Eq1@: @'Subtype1' f g@ means that @f a@ is a subtype of @g b@ whenever @a@ is one of @b@.

Derive an instance via t'Generics.Linear.Generically1', which needs one instance of linear-generics' 'Generic1' that serves every choice of the arguments from the first one that differs to the last, at most eight trailing arguments, and that only takes a value apart and puts it back, as the Template Haskell of @linear-generics@ generates.
A 'Subtype1' instance does not give '(<:)', which is derived by a line of its own:

> deriveGenericAnd1 ''View -- data View α a = View (Share α a) Int
>
> deriving via Generically1 (View β) instance (α >= β) => Subtype1 (View α) (View β)
> deriving via Generically1 (View β) b instance (α >= β, a <: b) => View α a <: View β b

@t'Control.Monad.Borrow.Pure.Mut' α@ has no instance, since a 'Control.Monad.Borrow.Pure.Mut' is invariant in what it borrows.
-}
type Subtype1 :: forall {k}. (k -> Type) -> (k -> Type) -> Constraint
class Subtype1 f g where
  subtype1 :: SubtypeWitness a b -> SubtypeWitness (f a) (g b)

-- | Lift 'upcast' through a 'Subtype1' constructor.
upcast1 :: (Subtype1 f g, a <: b) => f a %1 -> g b
upcast1 = Unsafe.toLinear unsafeCoerce

instance {-# INCOHERENT #-} (Coercible a b) => a <: b where
  subtype = UnsafeSubtype

{- | A target for deriving '(<:)' between representationally equal types, for use where their representation is hidden, such as outside the module that defines a newtype.

In that module, write @deriving via 'AsCoercible' T instance S '<:' T@.
-}
newtype AsCoercible a = AsCoercible {runAsCoercible :: a}

instance (Coercible a b) => a <: AsCoercible b where
  subtype = UnsafeSubtype

deriving via
  Generically [b]
  instance
    (a <: b) => [a] <: [b]

deriving via
  Generically (Maybe b)
  instance
    (a <: b) => Maybe a <: Maybe b

deriving via
  Generically (NonEmpty b)
  instance
    (a <: b) => NonEmpty a <: NonEmpty b

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

deriving via
  Generically (Identity b)
  instance
    (a <: b) => Identity a <: Identity b

deriving via Generically1 [] instance Subtype1 [] []

deriving via Generically1 Maybe instance Subtype1 Maybe Maybe

deriving via Generically1 NonEmpty instance Subtype1 NonEmpty NonEmpty

deriving via Generically1 Identity instance Subtype1 Identity Identity

deriving via
  Generically1 ((,) c')
  instance
    (c <: c') => Subtype1 ((,) c) ((,) c')

deriving via
  Generically1 (Either c')
  instance
    (c <: c') => Subtype1 (Either c) (Either c')

instance
  (a' <: a, b <: b', MultiplicityLe p q) =>
  (a %p -> b) <: (a' %q -> b')
  where
  subtype = UnsafeSubtype

instance
  (c' <: c, MultiplicityLe p q) =>
  Subtype1 (FUN p c) (FUN q c')
  where
  subtype1 _ = UnsafeSubtype

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

{- | @a@ and @b@ have instances of linear-generics' 'Generic' whose representations have the same shape and the same names, with the types of the fields related by '(<:)'.

'genericUpcast' converts through 'from' and 'to', pairing the fields by position, so it accepts two separate instances, and two applications of a type whose first differing argument is ninth from the end or further, which deriving via t'Generics.Linear.Generically' refuses.
-}
type GenericSubtype a b = (Generic a, Generic b, GSubtype (Rep a) (Rep b))

{-
Note [Subtyping through one Generic instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Allowing an unconstrained @(Generic a, Generic b, GSubtype (Rep a) (Rep b)) => a <: Generically b@ lets two separate 'Generic' instances compare structurally, which breaks ownership when two distinct types share a metadata-free representation or when two instantiations of one type constructor swap fields in separate instances (e.g. @Generic (P Int α β)@ vs @Generic (P Bool α β)@).

Instead, each 'Generically' instance applies one type constructor @t@ to trailing arguments @a1 .. an@ and @b1 .. bn@ and requires @(forall x1 .. xn. Generic (t x1 .. xn))@.
Because an instance whose context constrains any @xi@ cannot discharge the quantified constraint, @from@ at @t a1 .. an@ and @to@ at @t b1 .. bn@ must come from one parametric 'Generic' instance, so @to . from@ across the two instantiations is the identity on values and 'GSubtype' checks each field.
No witness is ever forced: 'upcast' and 'upcast1' ignore their dictionaries, so every method other than a refusal returns 'UnsafeSubtype' without looking at its context.
A method that forced the witnesses of its components would loop on a recursive type such as a list, whose representation needs the list's own witness, and so on every type that holds one, such as 'NonEmpty'; it would also turn a dictionary stored in a constructor into a thunk.
The heads of arities 1 to 8 overlap without one being more specific than another, so they carry @INCOHERENT@; every matching choice is sound on its own, so any choice GHC makes is sound and only acceptance can depend on arity.
The trailing refusal instance on @(a :: Type) <: Generically b@ catches the rest, two distinct type constructors, a type with no arguments, or a first differing argument ninth from the end or further, with a message that points to 'genericUpcast' for the last.

The 'Generically1' route for 'Subtype1' applies the same design: one non-incoherent head for a single functor @f@ with @Generic1 f@, eight @INCOHERENT@ heads for arities 1 to 8 with @(forall x1 .. xn. Generic1 (t x1 .. xn))@, and a trailing @INCOHERENT@ refusal instance.
Eight covers tuples of up to eight components, and types with several lifetime and element parameters.

As a guard, hidden-constructor abstract and linear types — 'Linearly', 'Now', 'EndToken', 'Alias', 'BO', 'Aff', 'SplitRecord', 'RecordLabel', 'RecordLabel'', 'UniqueSource', 'ChaseLevDeq', the vector and growable vector types, and the hash map and 'InsertPlan' types — carry 'Unsatisfiable' instances of 'Generic' (and 'Generic1' where their kind allows) so Template Haskell cannot derive a representation that exposes their hidden constructors or omits their phantom parameters.
'Ref' needs no guard: it wraps an unlifted type, which no representation can hold.
Each guard defines 'Rep' or 'Rep1' as 'V1', so that a more specific instance written beside it conflicts with its family instance.
The family instances cost a type-family consistency check, some tens of milliseconds, in a module that imports two guarded modules of which neither imports the other.
-}

-- See Note [Subtyping through one Generic instance].

-- | Subtype @t a1@ against @t b1@ using one parametric 'Generic' instance covering the differing trailing argument.
instance
  {-# INCOHERENT #-}
  (forall x1. Generic (t x1), GSubtype (Rep (t a1)) (Rep (t b1))) =>
  t a1 <: Generically (t b1)
  where
  subtype = UnsafeSubtype

-- | Subtype with two trailing arguments covered by one parametric 'Generic' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2. Generic (t x1 x2), GSubtype (Rep (t a1 a2)) (Rep (t b1 b2))) =>
  t a1 a2 <: Generically (t b1 b2)
  where
  subtype = UnsafeSubtype

-- | Subtype with three trailing arguments covered by one parametric 'Generic' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3. Generic (t x1 x2 x3), GSubtype (Rep (t a1 a2 a3)) (Rep (t b1 b2 b3))) =>
  t a1 a2 a3 <: Generically (t b1 b2 b3)
  where
  subtype = UnsafeSubtype

-- | Subtype with four trailing arguments covered by one parametric 'Generic' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3 x4. Generic (t x1 x2 x3 x4), GSubtype (Rep (t a1 a2 a3 a4)) (Rep (t b1 b2 b3 b4))) =>
  t a1 a2 a3 a4 <: Generically (t b1 b2 b3 b4)
  where
  subtype = UnsafeSubtype

-- | Subtype with five trailing arguments covered by one parametric 'Generic' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3 x4 x5. Generic (t x1 x2 x3 x4 x5), GSubtype (Rep (t a1 a2 a3 a4 a5)) (Rep (t b1 b2 b3 b4 b5))) =>
  t a1 a2 a3 a4 a5 <: Generically (t b1 b2 b3 b4 b5)
  where
  subtype = UnsafeSubtype

-- | Subtype with six trailing arguments covered by one parametric 'Generic' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3 x4 x5 x6. Generic (t x1 x2 x3 x4 x5 x6), GSubtype (Rep (t a1 a2 a3 a4 a5 a6)) (Rep (t b1 b2 b3 b4 b5 b6))) =>
  t a1 a2 a3 a4 a5 a6 <: Generically (t b1 b2 b3 b4 b5 b6)
  where
  subtype = UnsafeSubtype

-- | Subtype with seven trailing arguments covered by one parametric 'Generic' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3 x4 x5 x6 x7. Generic (t x1 x2 x3 x4 x5 x6 x7), GSubtype (Rep (t a1 a2 a3 a4 a5 a6 a7)) (Rep (t b1 b2 b3 b4 b5 b6 b7))) =>
  t a1 a2 a3 a4 a5 a6 a7 <: Generically (t b1 b2 b3 b4 b5 b6 b7)
  where
  subtype = UnsafeSubtype

-- | Subtype with eight trailing arguments covered by one parametric 'Generic' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3 x4 x5 x6 x7 x8. Generic (t x1 x2 x3 x4 x5 x6 x7 x8), GSubtype (Rep (t a1 a2 a3 a4 a5 a6 a7 a8)) (Rep (t b1 b2 b3 b4 b5 b6 b7 b8))) =>
  t a1 a2 a3 a4 a5 a6 a7 a8 <: Generically (t b1 b2 b3 b4 b5 b6 b7 b8)
  where
  subtype = UnsafeSubtype

instance
  {-# INCOHERENT #-}
  ( Unsatisfiable
      ( 'Text "Cannot derive (<:) via Generically between "
          ':<>: 'ShowType a
          ':<>: 'Text " and "
          ':<>: 'ShowType b
          ':$$: 'Text "Both types must share a type constructor and differ in at most eight trailing arguments,"
          ':$$: 'Text "with one Generic instance covering every argument from the first differing one to the last."
          ':$$: 'Text "For one type constructor whose first differing argument is ninth from the end or further, convert with genericUpcast instead."
      )
  ) =>
  (a :: Type) <: Generically b
  where
  subtype = unsatisfiable

genericUpcast :: (GenericSubtype a b) => a %1 -> b
genericUpcast = to . gupcast . from

-- | Subtype @f a@ against @g b@ using 'Subtype1' and @a <: b@.
instance (Subtype1 f g, a <: b) => f a <: Generically1 g b where
  subtype = UnsafeSubtype

-- | Derive 'Subtype1' for a functor using one 'Generic1' instance.
instance
  (Generic1 (f :: k -> Type), Subtype1 (Rep1 f) (Rep1 f)) =>
  Subtype1 f (Generically1 f)
  where
  subtype1 _ = UnsafeSubtype

-- | Derive 'Subtype1' with one differing trailing argument covered by one parametric 'Generic1' instance.
instance
  {-# INCOHERENT #-}
  (forall x1. Generic1 (t x1), Subtype1 (Rep1 (t a1)) (Rep1 (t b1))) =>
  Subtype1 (t a1 :: k -> Type) (Generically1 (t b1))
  where
  subtype1 _ = UnsafeSubtype

-- | Derive 'Subtype1' with two differing trailing arguments covered by one parametric 'Generic1' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2. Generic1 (t x1 x2), Subtype1 (Rep1 (t a1 a2)) (Rep1 (t b1 b2))) =>
  Subtype1 (t a1 a2 :: k -> Type) (Generically1 (t b1 b2))
  where
  subtype1 _ = UnsafeSubtype

-- | Derive 'Subtype1' with three differing trailing arguments covered by one parametric 'Generic1' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3. Generic1 (t x1 x2 x3), Subtype1 (Rep1 (t a1 a2 a3)) (Rep1 (t b1 b2 b3))) =>
  Subtype1 (t a1 a2 a3 :: k -> Type) (Generically1 (t b1 b2 b3))
  where
  subtype1 _ = UnsafeSubtype

-- | Derive 'Subtype1' with four differing trailing arguments covered by one parametric 'Generic1' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3 x4. Generic1 (t x1 x2 x3 x4), Subtype1 (Rep1 (t a1 a2 a3 a4)) (Rep1 (t b1 b2 b3 b4))) =>
  Subtype1 (t a1 a2 a3 a4 :: k -> Type) (Generically1 (t b1 b2 b3 b4))
  where
  subtype1 _ = UnsafeSubtype

-- | Derive 'Subtype1' with five differing trailing arguments covered by one parametric 'Generic1' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3 x4 x5. Generic1 (t x1 x2 x3 x4 x5), Subtype1 (Rep1 (t a1 a2 a3 a4 a5)) (Rep1 (t b1 b2 b3 b4 b5))) =>
  Subtype1 (t a1 a2 a3 a4 a5 :: k -> Type) (Generically1 (t b1 b2 b3 b4 b5))
  where
  subtype1 _ = UnsafeSubtype

-- | Derive 'Subtype1' with six differing trailing arguments covered by one parametric 'Generic1' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3 x4 x5 x6. Generic1 (t x1 x2 x3 x4 x5 x6), Subtype1 (Rep1 (t a1 a2 a3 a4 a5 a6)) (Rep1 (t b1 b2 b3 b4 b5 b6))) =>
  Subtype1 (t a1 a2 a3 a4 a5 a6 :: k -> Type) (Generically1 (t b1 b2 b3 b4 b5 b6))
  where
  subtype1 _ = UnsafeSubtype

-- | Derive 'Subtype1' with seven differing trailing arguments covered by one parametric 'Generic1' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3 x4 x5 x6 x7. Generic1 (t x1 x2 x3 x4 x5 x6 x7), Subtype1 (Rep1 (t a1 a2 a3 a4 a5 a6 a7)) (Rep1 (t b1 b2 b3 b4 b5 b6 b7))) =>
  Subtype1 (t a1 a2 a3 a4 a5 a6 a7 :: k -> Type) (Generically1 (t b1 b2 b3 b4 b5 b6 b7))
  where
  subtype1 _ = UnsafeSubtype

-- | Derive 'Subtype1' with eight differing trailing arguments covered by one parametric 'Generic1' instance.
instance
  {-# INCOHERENT #-}
  (forall x1 x2 x3 x4 x5 x6 x7 x8. Generic1 (t x1 x2 x3 x4 x5 x6 x7 x8), Subtype1 (Rep1 (t a1 a2 a3 a4 a5 a6 a7 a8)) (Rep1 (t b1 b2 b3 b4 b5 b6 b7 b8))) =>
  Subtype1 (t a1 a2 a3 a4 a5 a6 a7 a8 :: k -> Type) (Generically1 (t b1 b2 b3 b4 b5 b6 b7 b8))
  where
  subtype1 _ = UnsafeSubtype

instance
  {-# INCOHERENT #-}
  ( Unsatisfiable
      ( 'Text "Cannot derive Subtype1 via Generically1 between "
          ':<>: 'ShowType f
          ':<>: 'Text " and "
          ':<>: 'ShowType g
          ':$$: 'Text "Both functors must share a type constructor and differ in at most eight trailing arguments,"
          ':$$: 'Text "with one Generic1 instance covering every argument from the first differing one to the last."
      )
  ) =>
  Subtype1 (f :: k -> Type) (Generically1 (g :: k -> Type))
  where
  subtype1 = unsatisfiable

instance Subtype1 Par1 Par1 where
  subtype1 _ = UnsafeSubtype

instance (c <: c') => Subtype1 (K1 i c) (K1 i c') where
  subtype1 _ = UnsafeSubtype

instance Subtype1 U1 U1 where
  subtype1 _ = UnsafeSubtype

instance Subtype1 V1 V1 where
  subtype1 _ = UnsafeSubtype

instance (Subtype1 f g) => Subtype1 (M1 i m f) (M1 i m g) where
  subtype1 _ = UnsafeSubtype

instance (Subtype1 f g) => Subtype1 (MP1 m f) (MP1 m g) where
  subtype1 _ = UnsafeSubtype

instance (Subtype1 f f', Subtype1 g g') => Subtype1 (f :*: g) (f' :*: g') where
  subtype1 _ = UnsafeSubtype

instance (Subtype1 l l', Subtype1 r r') => Subtype1 (l :+: r) (l' :+: r') where
  subtype1 _ = UnsafeSubtype

instance (Subtype1 f f', Subtype1 g g') => Subtype1 (f :.: g) (f' :.: g') where
  subtype1 _ = UnsafeSubtype

instance '[] <: ('[] :: [k]) where
  subtype = UnsafeSubtype

instance (a <: b, as <: bs) => (a ': as) <: (b ': bs) where
  subtype = UnsafeSubtype
