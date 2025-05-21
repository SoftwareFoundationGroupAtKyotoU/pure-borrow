{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Borrow.Pure.Affine.Internal (
  -- * Affine Types
  Affine (..),
  pop,
  pop#,
  popZero,

  -- ** Linear Generics
  GenericAffine,

  -- ** Internal utilities for implementing 'Affine
  AffinityWitness (..),

  -- * Affine modality
  Aff,
  aff,
  unaff,
  fromAffine,
) where

import Data.Int
import Data.Kind
import Data.Monoid qualified as Mon
import Data.Ord (Down)
import Data.Semigroup qualified as Sem
import Data.Word
import GHC.Base
import Generics.Linear
import Unsafe.Linear qualified as Unsafe

{- |
A linear resource that is _entirely_ allocated on GC heap and  can be regarded as 'Affine'.
It cannot be released promptly, but it is guaranteed to be released in the future by GC.

At first glance, this seems rather analogous to 'Prelude.Linear.Consumable', but its 'Prelude.Linear.consume' counterpart 'pop' is _not_ a member of 'Affine'.
The rationale is that the condition of being maintained by GC is much stronger than mere 'Consumable' types.

One counterexample is 'Foreign.Ptr' - it is just a pointer to a resource _outside GC heap resource_, and hence the real resouce @a@ will not be freed even after corresponding @'Foreign.Ptr' a@ itself is freed by the GC!

Despite the similarity, 'Affine' types are not necessarily a 'Consumable' and vice versa - one such example is @'Aff' a@.
This is an instance of 'Affine' regardless of type parameter @a@, and in particular, any @'Aff' (a -> b)@ is affine regardless of @a@ and @b@.
The independence of 'pop' from 'Affine' is inevitable for this instance - otherwise, we must call 'pop' for codomain types.

The invariant of "always maintained on GC heap" is so strong and it is the responsibilty of library implementors to declare the valid instance of 'Affine'.

To prevent the wrong implementation, 'Affine' has the hidden member constant 'affinityWitness'.
The type needed to implement is defined in "Control.Borrow.Pure.Affine.Internal", which is hidden in Hacddock.

Most users can use 'Generically' in combination with 'Generics.Linear.TH.deriveGeneric' to derive 'Affine' for the types that can safely be regarded as 'Affine'.
-}
type Affine :: forall {rep}. TYPE rep -> Constraint
class Affine a where
  affinityWitness :: AffinityWitness a

data AffinityWitness a = UnsafeAssumeAffinity

pop :: (Affine a) => a %1 -> ()
pop = Unsafe.toLinear \_ -> ()
{-# INLINE pop #-}

pop# :: forall (a :: UnliftedType). (Affine a) => a %1 -> ()
pop# = Unsafe.toLinear \_ -> ()
{-# INLINE pop# #-}

popZero :: forall (a :: ZeroBitType). (Affine a) => a %1 -> ()
popZero = Unsafe.toLinear \_ -> ()
{-# INLINE popZero #-}

newtype UnsafeAsAffine a = UnsafeAsAffine a

-- To suppress the warning.
_u :: UnsafeAsAffine ()
_u = UnsafeAsAffine ()

instance Affine (UnsafeAsAffine a) where
  affinityWitness = UnsafeAssumeAffinity
  {-# INLINE affinityWitness #-}

deriving via UnsafeAsAffine Int instance Affine Int

deriving via UnsafeAsAffine Int8 instance Affine Int8

deriving via UnsafeAsAffine Int16 instance Affine Int16

deriving via UnsafeAsAffine Int32 instance Affine Int32

deriving via UnsafeAsAffine Int64 instance Affine Int64

deriving via UnsafeAsAffine Word instance Affine Word

deriving via UnsafeAsAffine Word8 instance Affine Word8

deriving via UnsafeAsAffine Word16 instance Affine Word16

deriving via UnsafeAsAffine Word32 instance Affine Word32

deriving via UnsafeAsAffine Word64 instance Affine Word64

deriving via UnsafeAsAffine Char instance Affine Char

deriving via UnsafeAsAffine Bool instance Affine Bool

deriving via UnsafeAsAffine Sem.Any instance Affine Sem.Any

deriving via UnsafeAsAffine Sem.All instance Affine Sem.All

deriving via Generically (Maybe a) instance (Affine a) => Affine (Maybe a)

deriving via
  Generically (Either a b)
  instance
    (Affine a, Affine b) => Affine (Either a b)

deriving via UnsafeAsAffine () instance Affine ()

deriving via
  Generically (a, b)
  instance
    (Affine a, Affine b) => Affine (a, b)

deriving via
  Generically (a, b, c)
  instance
    (Affine a, Affine b, Affine c) => Affine (a, b, c)

deriving via
  Generically (a, b, c, d)
  instance
    (Affine a, Affine b, Affine c, Affine d) => Affine (a, b, c, d)

deriving via
  Generically (a, b, c, d, e)
  instance
    (Affine a, Affine b, Affine c, Affine d, Affine e) => Affine (a, b, c, d, e)

deriving via Generically (Sem.Sum a) instance (Affine a) => Affine (Sem.Sum a)

deriving via Generically (Sem.Product a) instance (Affine a) => Affine (Sem.Product a)

deriving via Generically (Sem.First a) instance (Affine a) => Affine (Sem.First a)

deriving via Generically (Sem.Last a) instance (Affine a) => Affine (Sem.Last a)

deriving via Generically (Sem.Dual a) instance (Affine a) => Affine (Sem.Dual a)

deriving via Generically (Down a) instance (Affine a) => Affine (Down a)

deriving via Generically [a] instance (Affine a) => Affine [a]

deriving via (Maybe a) instance (Affine a) => Affine (Mon.First a)

deriving via (Maybe a) instance (Affine a) => Affine (Mon.Last a)

data Aff a where
  UnsafeAff :: a -> Aff a

unaff :: Aff a %1 -> a
unaff (UnsafeAff a) = a
{-# INLINE unaff #-}

aff :: a -> Aff a
aff a = UnsafeAff a
{-# INLINE aff #-}

fromAffine :: (Affine a) => a %1 -> Aff a
fromAffine = Unsafe.toLinear UnsafeAff
{-# INLINE fromAffine #-}

instance (GenericAffine a) => Affine (Generically a) where
  affinityWitness = UnsafeAssumeAffinity
  {-# INLINE affinityWitness #-}

{- | A constraint synonym for types for which 'Affine' instance
can be safely derived via 'Generically'.
-}
class (Generic a, GAffine (Rep a)) => GenericAffine a

instance (Generic a, GAffine (Rep a)) => GenericAffine a

class (Generic1 f, GAffine (Rep1 f)) => GenericAffine1 f

instance (Generic1 f, GAffine (Rep1 f)) => GenericAffine1 f

type GAffine :: (k -> Type) -> Constraint
class GAffine f

instance (Affine a) => GAffine (K1 i a)

instance (GAffine f, GAffine g) => GAffine (f :+: g)

instance (GAffine f, GAffine g) => GAffine (f :*: g)

instance GAffine (MP1 Many f)

instance (GAffine f) => GAffine (MP1 One f)

instance GAffine V1

instance GAffine U1

instance (GAffine f) => GAffine (M1 i c f)
