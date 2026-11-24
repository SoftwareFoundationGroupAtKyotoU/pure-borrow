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
import GHC.TypeError (Assert, ErrorMessage (..), TypeError)
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
Deriving via t'Generics.Linear.Generically' relates two applications of one type constructor that differ only in their last argument, through one instance of linear-generics' 'Generic' that serves every choice of it and only takes a value apart and puts it back, as the Template Haskell of @linear-generics@ generates.
A type whose last two arguments differ derives through t'Generics.Linear.Generically1' and 'Subtype1' instead.
-}
class a <: b where
  subtype :: SubtypeWitness a b

upcast :: (a <: b) => a %1 -> b
upcast = Unsafe.toLinear unsafeCoerce

{- | Lifting of '(<:)' to unary type constructors, analogous to @Eq1@: @'Subtype1' f g@ means that @f a@ is a subtype of @g b@ whenever @a@ is one of @b@.

Derive an instance via t'Generics.Linear.Generically1', for one functor or for two applications of one type constructor that differ only in their last argument, such as @View α@ and @View β@, through one instance of linear-generics' 'Generic1' that serves every choice of it and only takes a value apart and puts it back, as the Template Haskell of @linear-generics@ generates.
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
  Generically1 ((,) a') b'
  instance
    (a <: a', b <: b') => (a, b) <: (a', b')

deriving via
  Generically1 (Either a') b'
  instance
    (a <: a', b <: b') => Either a b <: Either a' b'

-- Trusted: neither route varies three arguments, and each component is upcast in place.
instance (a <: a', b <: b', c <: c') => (a, b, c) <: (a', b', c') where
  subtype = UnsafeSubtype

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

'genericUpcast' converts through 'from' and 'to', pairing the fields by position, so it accepts two separate instances, and two applications of a type that differ in more arguments than deriving via t'Generics.Linear.Generically' or t'Generics.Linear.Generically1' can vary.
-}
type GenericSubtype a b = (Generic a, Generic b, GSubtype (Rep a) (Rep b))

{-
Note [Subtyping through one Generic instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An instance @(Generic a, Generic b, GSubtype (Rep a) (Rep b)) => a <: Generically b@ would compare the representations of two separate 'Generic' instances, which two lawful instances can make disagree on what a position holds.
Two distinct types with metadata-free representations would be related although nothing relates their values, and two instantiations of one type constructor with separate instances, such as @Generic (P Int α β)@ and @Generic (P Bool α β)@ listing the fields in other orders, would let 'upcast' swap the lifetimes of two 'Mut's.

So the 'Generically' instance relates only @t a@ to @t b@, two applications of one type constructor that differ in their last argument, and requires @forall x. Generic (t x)@.
The quantified constraint holds only through one instance whose context holds for every @x@, so @from@ at @t a@ and @to@ at @t b@ come from one parametric instance, @to . from@ across the two is the identity on values, and 'GSubtype' checks each field.
The only other instance that matches it is the @INCOHERENT@ one through 'Coercible', which is more general, so it needs no overlap pragma of its own.

The 'Generically1' route for 'Subtype1' has the instance for one functor @f@, under @Generic1 f@, and one head relating @t a@ to @t b@ under @forall x. Generic1 (t x)@.
The head is for a lifetime that 'Generic1' cannot reach, such as the @α@ of @View α a@, since an instance of @Generic1 (View α)@ varies only the element.
The two overlap where @a@ and @b@ are one type, and both are sound there, so the head carries @INCOHERENT@.
A type whose last two arguments differ derives '(<:)' through 'Generically1', with 'Subtype1' of the type applied to all but its last argument, as the pairs and 'Either' of this module do.

There are no heads for more arguments, and no refusal instances for what no head covers: heads of several arities overlap one another, and GHC's choice among them could decide whether a program is accepted.
A type that differs in more arguments, or one whose last argument sits under a 'Mut' while its lifetime comes before it, as in @MutBox α a = MutBox (Mut α a) Int@, which 'Generically1' cannot take since 'Mut' has no 'Subtype1', gets its instance by hand, as the triples of this module do.

No witness is ever forced: 'upcast' and 'upcast1' ignore their dictionaries, and every method other than the refusal for 'Mut' returns 'UnsafeSubtype' without looking at its context.
A method that forced the witnesses of its components would loop on a recursive type such as a list, whose representation needs the list's own witness, and so on every type that holds one, such as 'NonEmpty'; it would also turn a dictionary stored in a constructor into a thunk.
A deferred class constraint in the context of an instance that resolution picks is therefore never raised, unlike a deferred equality, and tests of such a refusal, as of a trusted instance used the wrong way round, are typing-fail fixtures.

The abstract and linear types with hidden constructors carry 'Unsatisfiable' instances of 'Generic', and of 'Generic1' where their kind allows, so that no instance can be derived for them: its representation would expose the hidden constructor or leave out a phantom parameter, and the generic routes would rebuild the value outside its API.
They are 'Linearly', 'Now', 'EndToken', 'Alias', 'BO', 'Aff', 'SplitRecord', both record labels, 'UniqueSource', 'ChaseLevDeq', every vector and growable vector, and both hash maps with their 'InsertPlan'.
'Ref' needs no guard: it wraps an unlifted type, which no representation can hold.
Each guard defines 'Rep' or 'Rep1' as 'V1', so that a more specific instance written beside it conflicts with its family instance.
The family instances cost a type-family consistency check, some tens of milliseconds, in a module that imports two guarded modules of which neither imports the other.
-}

-- See Note [Subtyping through one Generic instance].

-- | Subtype @t a@ against @t b@, which differ only in the last argument, through one 'Generic' instance that serves every choice of it.
instance
  (forall x. Generic (t x), GSubtype (Rep (t a)) (Rep (t b))) =>
  t a <: Generically (t b)
  where
  subtype = UnsafeSubtype

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

-- See Note [Subtyping through one Generic instance].

-- | Derive 'Subtype1' between @t a@ and @t b@, which differ only in their last argument, as @View α@ and @View β@ do, through one 'Generic1' instance that serves every choice of it.
instance
  {-# INCOHERENT #-}
  (forall x. Generic1 (t x), Subtype1 (Rep1 (t a)) (Rep1 (t b))) =>
  Subtype1 (t a :: k -> Type) (Generically1 (t b))
  where
  subtype1 _ = UnsafeSubtype

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
