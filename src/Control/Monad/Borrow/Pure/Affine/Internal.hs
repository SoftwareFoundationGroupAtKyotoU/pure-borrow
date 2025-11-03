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

module Control.Monad.Borrow.Pure.Affine.Internal (
  -- * Affine Types
  Affine (..),
  AsAffine (..),
  Aff (..),
  affu,
  unaff,
  pop,

  -- ** Linear Generics
  GenericAffine,
  GenericallyAffine (..),
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
  UnsafeAff :: !a %One -> Aff a

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
pop = Unsafe.toLinear (\(UnsafeAff _) -> ())
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

type Affine :: Type -> Constraint
class Affine a where
  aff :: a %1 -> Aff a

instance (Movable a) => Affine (AsMovable a) where
  aff (AsMovable a) = move a PL.& \(Ur x) -> UnsafeAff (AsMovable x)
  {-# INLINE aff #-}

deriving via AsMovable (Ur a) instance Affine (Ur a)

deriving via AsMovable Bool instance Affine Bool

deriving via AsMovable Char instance Affine Char

deriving via AsMovable Int instance Affine Int

deriving via AsMovable Int8 instance Affine Int8

deriving via AsMovable Int16 instance Affine Int16

deriving via AsMovable Int32 instance Affine Int32

deriving via AsMovable Int64 instance Affine Int64

deriving via AsMovable Word instance Affine Word

deriving via AsMovable Word8 instance Affine Word8

deriving via AsMovable Word16 instance Affine Word16

deriving via AsMovable Word32 instance Affine Word32

deriving via AsMovable Word64 instance Affine Word64

newtype AsAffine a = AsAffine a

instance (Affine a) => Consumable (AsAffine a) where
  consume (AsAffine a) = pop (aff a)
  {-# INLINE consume #-}

deriving newtype instance Affine Sem.Any

deriving newtype instance Affine Sem.All

deriving via GenericallyAffine (Maybe a) instance (Affine a) => Affine (Maybe a)

deriving via
  GenericallyAffine (Either a b)
  instance
    (Affine a, Affine b) => Affine (Either a b)

deriving via GenericallyAffine () instance Affine ()

deriving via
  GenericallyAffine (a, b)
  instance
    (Affine a, Affine b) => Affine (a, b)

deriving via
  GenericallyAffine (a, b, c)
  instance
    (Affine a, Affine b, Affine c) => Affine (a, b, c)

deriving via
  GenericallyAffine (a, b, c, d)
  instance
    (Affine a, Affine b, Affine c, Affine d) => Affine (a, b, c, d)

deriving via
  GenericallyAffine (a, b, c, d, e)
  instance
    (Affine a, Affine b, Affine c, Affine d, Affine e) => Affine (a, b, c, d, e)

deriving via GenericallyAffine (Sem.Sum a) instance (Affine a) => Affine (Sem.Sum a)

deriving via GenericallyAffine (Sem.Product a) instance (Affine a) => Affine (Sem.Product a)

deriving via GenericallyAffine (Sem.First a) instance (Affine a) => Affine (Sem.First a)

deriving via GenericallyAffine (Sem.Last a) instance (Affine a) => Affine (Sem.Last a)

deriving via GenericallyAffine (Sem.Dual a) instance (Affine a) => Affine (Sem.Dual a)

deriving via GenericallyAffine [a] instance (Affine a) => Affine [a]

deriving via (Maybe a) instance (Affine a) => Affine (Mon.First a)

deriving via (Maybe a) instance (Affine a) => Affine (Mon.Last a)

-- * Generics

{- | We need this instead of 'Generically' becuase
it gives a different 'Consumable' instance.
-}
newtype GenericallyAffine a = GenericallyAffine a

unGenericallyAffine :: GenericallyAffine a %1 -> a
unGenericallyAffine (GenericallyAffine a) = a

deriving via
  AsAffine (GenericallyAffine a)
  instance
    (GenericAffine a) => Consumable (GenericallyAffine a)

instance (GenericAffine a) => Affine (GenericallyAffine a) where
  aff = Data.fmap GenericallyAffine PL.. genericAff PL.. unGenericallyAffine
  {-# INLINE aff #-}

genericAff :: (GenericAffine a) => a %1 -> Aff a
genericAff a = to Data.<$> gaff (from a)

{- | A constraint synonym for types for which 'Affine' instance
can be safely derived via 'Generically'.
-}
class (Generic a, GAffine (Rep a)) => GenericAffine a

instance (Generic a, GAffine (Rep a)) => GenericAffine a

type GAffine :: (k -> Type) -> Constraint
class GAffine f where
  gaff :: f a %1 -> Aff (f a)

instance (Affine a) => GAffine (K1 i a) where
  gaff (K1 a) = K1 Data.<$> aff a
  {-# INLINE gaff #-}

instance (GAffine f, GAffine g) => GAffine (f :+: g) where
  gaff (L1 x) = L1 Data.<$> gaff x
  gaff (R1 y) = R1 Data.<$> gaff y
  {-# INLINE gaff #-}

instance (GAffine f, GAffine g) => GAffine (f :*: g) where
  gaff (x :*: y) = (:*:) Data.<$> gaff x Data.<@> gaff y
  {-# INLINE gaff #-}

instance GAffine (MP1 Many f) where
  gaff (MP1 x) = UnsafeAff (MP1 x)
  {-# INLINE gaff #-}

instance (GAffine f) => GAffine (MP1 One f) where
  gaff (MP1 x) = MP1 Data.<$> gaff x
  {-# INLINE gaff #-}

instance GAffine V1 where
  gaff = \case {}
  {-# INLINE gaff #-}

instance GAffine U1 where
  gaff U1 = UnsafeAff U1
  {-# INLINE gaff #-}

instance (GAffine f) => GAffine (M1 i c f) where
  gaff (M1 x) = M1 Data.<$> gaff x
  {-# INLINE gaff #-}
