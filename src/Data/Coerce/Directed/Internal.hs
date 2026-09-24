{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
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

-- A phantom role would let DerivingVia coerce the reflexive instance from the
-- INCOHERENT @Coercible a b => a <: b@ into any other instance, such as one
-- lengthening a borrow's lifetime, without a warning.
-- DerivingVia coerces along the last class parameter only.
-- With that target representational, @deriving via X instance S <: T@ needs
-- an existing @S <: X@ and @X@ representationally equal to @T@ where it is
-- written, so the derived 'upcast' is an existing 'upcast' followed by a
-- 'Data.Coerce.coerce': deriving grants nothing that 'upcast' and 'coerce'
-- do not already.
-- That keeps the 'Generically' and 'AsCoercible' derivations working, and
-- rests on the same assumption as the INCOHERENT instance itself: no type
-- of this library gives a lifetime parameter a non-nominal role.
type role SubtypeWitness nominal representational

{- | Evidence that @a@ can be upcast to @b@.

The safe modules export only its synonym '(<:)', so no instance can be written or derived against them; see "Data.Coerce.Directed.Unsafe" for writing one.
-}
class Subtype a b where
  subtype :: SubtypeWitness a b

{- | @a <: b@: a value of @a@ can be used as a value of @b@, through 'upcast'.

It is a synonym, so that the safe modules can export it without letting anyone write an instance: an instance is a promise that 'upcast' cannot break an invariant, which the library cannot check, so a user can make it only through "Data.Coerce.Directed.Unsafe".
The library provides it

* between representationally equal types ('Data.Coerce.Coercible');
* componentwise for lists, 'Maybe', 'NonEmpty', 'Either', pairs and triples;
* for functions, contravariantly in the argument and covariantly in the result, where a linear function may stand in for an unrestricted one but not the other way round;
* for the borrows and their companions: @t'Control.Monad.Borrow.Pure.Share' α a '<:' t'Control.Monad.Borrow.Pure.Share' β b@ and @t'Control.Monad.Borrow.Pure.BO' α a '<:' t'Control.Monad.Borrow.Pure.BO' β b@ when @α >= β@ and @a '<:' b@, @t'Control.Monad.Borrow.Pure.Mut' α a '<:' t'Control.Monad.Borrow.Pure.Mut' β b@ when furthermore @b '<:' a@, and @t'Control.Monad.Borrow.Pure.Lend' α a '<:' t'Control.Monad.Borrow.Pure.Lend' β b@ and @t'Control.Monad.Borrow.Pure.After' α a '<:' t'Control.Monad.Borrow.Pure.After' β b@ when @α <= β@ and @a '<:' b@;
* for the bundles of "Control.Monad.Borrow.Pure.Experimental.Borrows", as for the borrows they bundle;
* for @t'Control.Monad.Borrow.Pure.Lifetime.Token.EndToken' α '<:' t'Control.Monad.Borrow.Pure.Lifetime.Token.EndToken' β@ when @α >= β@;
* to @'AsCoercible' b@ from any type representationally equal to @b@, and to @'Generically' b@ from any type of the same 'Generic' shape as @b@, as targets for @DerivingVia@.

For a type of your own, 'genericUpcast' converts between it and another with the same 'Generic' shape without any instance; both need the 'Generic' instances of linear-generics, derived with @Generics.Linear.TH.deriveGeneric@.

It takes both arguments, so it cannot be partially applied.
Where you need @(<:) a@, define a class of your own with it as the superclass, and use @SubtypeOf a@ instead:

@
class (a '<:' b) => SubtypeOf a b
instance (a '<:' b) => SubtypeOf a b
@

See Note [Sealing classes behind synonyms].
-}
type (<:) :: k -> k -> Constraint
type a <: b = Subtype a b

{-
Note [Sealing classes behind synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'(<:)', 'Control.Monad.Borrow.Pure.Lifetime.Token.Internal.End' and 'Control.Monad.Borrow.Pure.Lifetime.Internal.<=' are exported from the safe modules as synonyms of classes that those modules do not export.
GHC rejects an instance declaration whose head is a synonym ("Illegal instance for type synonym", GHC-53946), whether it is written by hand or by standalone deriving, so the synonyms carry every use of the constraints but no instance.

A deriving clause attached to a data declaration is what decides the shape of each synonym.
It names a class applied to all but its last argument, and GHC expands an eta-reduced synonym such as @type (<:) = Subtype@ there, so the clause derives an instance of the hidden class: with that synonym, @newtype T α = T (Mut Static R) deriving ((<:) (Mut α R)) via (T α)@ compiles from safe modules with no warning and lengthens any borrow to 'Static'.
A synonym that takes both arguments cannot appear partially applied, so GHC rejects such a clause with GHC-53946, also under @LiberalTypeSynonyms@ and behind a second synonym.
'End' and '(<=)' stay eta-reduced, which keeps them partially applicable: a deriving clause supplies the declared data type as the class's last argument, and that type's kind ends in 'Type', never in 'Control.Monad.Borrow.Pure.Lifetime.Internal.Lifetime', so GHC rejects the clause ("Cannot derive well-kinded instance", GHC-62016).

The seal stops instances, not evidence made up on the spot.
@GHC.Exts.withDict@ supplies a dictionary for any single-method class without an instance: @withDict \@(Mut α R '<:' Mut Static R) undefined upcast@ compiles from the safe modules and lengthens a borrow, since 'upcast' never looks at its evidence, and '(<=)' is no different.
GHC classifies @withDict@ as unsafe for Safe Haskell because it breaks coherence, and a program that uses it is outside this library's guarantee, like one that uses @unsafeCoerce@.
'Control.Monad.Borrow.Pure.BO.Internal.reclaim' forces its 'End' evidence anyway, because 'Control.Monad.Borrow.Pure.Lifetime.Token.withEnd', which the safe modules export, accepts a bottom token; that the same check also stops a bottom 'End' from @withDict@ is incidental.
-}

upcast :: (a <: b) => a %1 -> b
upcast = Unsafe.toLinear unsafeCoerce

instance {-# INCOHERENT #-} (Coercible a b) => Subtype a b where
  subtype = UnsafeSubtype

{- | A target for deriving 'Subtype' between representationally equal types, for use where their representation is hidden, such as outside the module that defines a newtype.

In that module, with "Data.Coerce.Directed.Unsafe" imported, write @deriving via 'AsCoercible' T instance 'Subtype' S T@.
-}
newtype AsCoercible a = AsCoercible {runAsCoercible :: a}

instance (Coercible a b) => Subtype a (AsCoercible b) where
  subtype = UnsafeSubtype

deriving via
  Generically [b]
  instance
    (a <: b) => Subtype [a] [b]

deriving via
  Generically (Maybe b)
  instance
    (a <: b) => Subtype (Maybe a) (Maybe b)

deriving via
  Generically (NonEmpty b)
  instance
    (a <: b) => Subtype (NonEmpty a) (NonEmpty b)

deriving via
  Generically (a', b')
  instance
    (a <: a', b <: b') => Subtype (a, b) (a', b')

deriving via
  Generically (Either a' b')
  instance
    (a <: a', b <: b') => Subtype (Either a b) (Either a' b')

deriving via
  Generically (a', b', c')
  instance
    (a <: a', b <: b', c <: c') => Subtype (a, b, c) (a', b', c')

instance
  (a' <: a, b <: b', MultiplicityLe p q) =>
  Subtype (a %p -> b) (a' %q -> b')
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

instance (GenericSubtype a b) => Subtype a (Generically b) where
  subtype = UnsafeSubtype

genericUpcast :: (GenericSubtype a b) => a %1 -> b
genericUpcast = to . gupcast . from

instance Subtype '[] ('[] :: [k]) where
  subtype = UnsafeSubtype

instance (a <: b, as <: bs) => Subtype (a ': as) (b ': bs) where
  subtype = UnsafeSubtype
