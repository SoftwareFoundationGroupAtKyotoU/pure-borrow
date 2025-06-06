{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
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
  Affable (..),
  Aff (..),
  affu,
  unaff,
  pop,

  -- ** Linear Generics
  GenericAffable,
  GenericallyAffable (..),
) where

import Data.Comonad.Linear qualified as Data
import Data.Functor.Linear qualified as Data
import Data.Int
import Data.Kind
import Data.Monoid qualified as Mon
import Data.Semigroup qualified as Sem
import Data.Unrestricted.Linear
import Data.Word
import GHC.Base
import Generics.Linear
import Prelude.Linear qualified as PL
import Unsafe.Linear qualified as Unsafe

data Aff a where
  UnsafeAff :: a %One -> Aff a

unaff :: Aff a %1 -> a
unaff (UnsafeAff !a) = a
{-# INLINE unaff #-}

{- | You can bring unrestricted resources into 'Aff' context.

Note that, when you use 'aff' to bring a foreign resource (e.g. 'Foreign.Ptr'),
it is user's responsibility to ensure 'Forign.free' is called on the resource after the @'Aff' ('Foreign.Ptr' a)@ is popped.
-}
affu :: a -> Aff a
affu = UnsafeAff
{-# INLINE affu #-}

pop :: Aff a %1 -> ()
pop = Unsafe.toLinear (\(UnsafeAff !_) -> ())
{-# INLINE pop #-}

instance Consumable (Aff a) where
  consume = pop
  {-# INLINE consume #-}

instance Data.Functor Aff where
  fmap f (UnsafeAff a) = UnsafeAff (f a)
  {-# INLINE fmap #-}

instance Data.Comonad Aff where
  extract (UnsafeAff a) = a
  {-# INLINE extract #-}

  duplicate (UnsafeAff a) = UnsafeAff (UnsafeAff a)
  {-# INLINE duplicate #-}

instance Data.ComonadApply Aff where
  (UnsafeAff f) <@> (UnsafeAff a) = UnsafeAff (f a)
  {-# INLINE (<@>) #-}

type Affable :: Type -> Constraint
class Affable a where
  aff :: a %1 -> Aff a

instance (Movable a) => Affable (AsMovable a) where
  aff (AsMovable a) = move a PL.& \(Ur x) -> UnsafeAff (AsMovable x)
  {-# INLINE aff #-}

deriving via AsMovable (Ur a) instance Affable (Ur a)

deriving via AsMovable Bool instance Affable Bool

deriving via AsMovable Char instance Affable Char

deriving via AsMovable Int instance Affable Int

deriving via AsMovable Int8 instance Affable Int8

deriving via AsMovable Int16 instance Affable Int16

deriving via AsMovable Int32 instance Affable Int32

deriving via AsMovable Int64 instance Affable Int64

deriving via AsMovable Word instance Affable Word

deriving via AsMovable Word8 instance Affable Word8

deriving via AsMovable Word16 instance Affable Word16

deriving via AsMovable Word32 instance Affable Word32

deriving via AsMovable Word64 instance Affable Word64

newtype AsAffable a = AsAffable a

instance (Affable a) => Consumable (AsAffable a) where
  consume (AsAffable a) = pop (aff a)
  {-# INLINE consume #-}

deriving newtype instance Affable Sem.Any

deriving newtype instance Affable Sem.All

deriving via GenericallyAffable (Maybe a) instance (Affable a) => Affable (Maybe a)

deriving via
  GenericallyAffable (Either a b)
  instance
    (Affable a, Affable b) => Affable (Either a b)

deriving via GenericallyAffable () instance Affable ()

deriving via
  GenericallyAffable (a, b)
  instance
    (Affable a, Affable b) => Affable (a, b)

deriving via
  GenericallyAffable (a, b, c)
  instance
    (Affable a, Affable b, Affable c) => Affable (a, b, c)

deriving via
  GenericallyAffable (a, b, c, d)
  instance
    (Affable a, Affable b, Affable c, Affable d) => Affable (a, b, c, d)

deriving via
  GenericallyAffable (a, b, c, d, e)
  instance
    (Affable a, Affable b, Affable c, Affable d, Affable e) => Affable (a, b, c, d, e)

deriving via GenericallyAffable (Sem.Sum a) instance (Affable a) => Affable (Sem.Sum a)

deriving via GenericallyAffable (Sem.Product a) instance (Affable a) => Affable (Sem.Product a)

deriving via GenericallyAffable (Sem.First a) instance (Affable a) => Affable (Sem.First a)

deriving via GenericallyAffable (Sem.Last a) instance (Affable a) => Affable (Sem.Last a)

deriving via GenericallyAffable (Sem.Dual a) instance (Affable a) => Affable (Sem.Dual a)

deriving via GenericallyAffable [a] instance (Affable a) => Affable [a]

deriving via (Maybe a) instance (Affable a) => Affable (Mon.First a)

deriving via (Maybe a) instance (Affable a) => Affable (Mon.Last a)

-- * Generics

newtype GenericallyAffable a = GenericallyAffable a

unGenericallyAffable :: GenericallyAffable a %1 -> a
unGenericallyAffable (GenericallyAffable a) = a

deriving via
  AsAffable (GenericallyAffable a)
  instance
    (GenericAffable a) => Consumable (GenericallyAffable a)

instance (GenericAffable a) => Affable (GenericallyAffable a) where
  aff = Data.fmap GenericallyAffable PL.. genericAff PL.. unGenericallyAffable
  {-# INLINE aff #-}

genericAff :: (GenericAffable a) => a %1 -> Aff a
genericAff a = to Data.<$> gaff (from a)

{- | A constraint synonym for types for which 'Affable' instance
can be safely derived via 'Generically'.
-}
class (Generic a, GAffable (Rep a)) => GenericAffable a

instance (Generic a, GAffable (Rep a)) => GenericAffable a

type GAffable :: (k -> Type) -> Constraint
class GAffable f where
  gaff :: f a %1 -> Aff (f a)

instance (Affable a) => GAffable (K1 i a) where
  gaff (K1 a) = K1 Data.<$> aff a
  {-# INLINE gaff #-}

instance (GAffable f, GAffable g) => GAffable (f :+: g) where
  gaff (L1 x) = L1 Data.<$> gaff x
  gaff (R1 y) = R1 Data.<$> gaff y
  {-# INLINE gaff #-}

instance (GAffable f, GAffable g) => GAffable (f :*: g) where
  gaff (x :*: y) = (:*:) Data.<$> gaff x Data.<@> gaff y
  {-# INLINE gaff #-}

instance GAffable (MP1 Many f) where
  gaff (MP1 x) = UnsafeAff (MP1 x)
  {-# INLINE gaff #-}

instance (GAffable f) => GAffable (MP1 One f) where
  gaff (MP1 x) = MP1 Data.<$> gaff x
  {-# INLINE gaff #-}

instance GAffable V1 where
  gaff = \case {}
  {-# INLINE gaff #-}

instance GAffable U1 where
  gaff U1 = UnsafeAff U1
  {-# INLINE gaff #-}

instance (GAffable f) => GAffable (M1 i c f) where
  gaff (M1 x) = M1 Data.<$> gaff x
  {-# INLINE gaff #-}