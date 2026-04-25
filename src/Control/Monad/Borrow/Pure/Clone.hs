{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Borrow.Pure.Clone (
  Clone (..),
  genericClone,
  AsCopyable (..),
  Clone1 (..),
  clone1,
  GenericClone1,
  genericLiftClone,
  genericClone1,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Ref
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Data.Coerce (Coercible, coerce)
import Data.Data (Proxy)
import Data.Int
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Ref.Linear (freeRef)
import Data.Ref.Linear qualified as Ref
import Data.Word
import GHC.Exts (Multiplicity (..))
import Generics.Linear
import Numeric.Natural
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

{- | @'Clone' a@ is analogous o @'Copyable' a@, but requires cloned values
to be accessible only inside the @'BO' α@ monad.

The difference between 'Clone' and 'Copyable' is that the former allows for
cloning a shared borrow of a _mutable_ or _linear_ value, while the latter requires cloning a shared borrow of an _immutable_ value.
This is because we can leak @'Share' α a@ via 'Prelude.Linear.Movable' instance, and
hence it can outlive the original @'BO' α@ lifetime, which allows leaking mutable states inside @a@ into _unrestricted_ contexts, which destroys the soundness severly.
-}
class Clone a where
  clone :: Share α a %1 -> BO α a
  default clone :: (GenericClone a) => Share α a %1 -> BO α a
  clone = genericClone

instance (Dupable a) => Clone (Ref a) where
  clone = Unsafe.toLinear \(UnsafeAlias ref) -> Control.do
    !a <- Control.pure $ freeRef ref
    !a' <- Unsafe.toLinear (\(!_, !a') -> Control.pure a') $ dup a
    Ref.new a' Control.<$> askLinearly
  {-# INLINE clone #-}


newtype AsCopyable a = AsCopyable a
  deriving newtype (Copyable)

instance (Copyable a) => Clone (AsCopyable a) where
  clone = Control.pure . copy
  {-# INLINE clone #-}

deriving via AsCopyable Int instance Clone Int

deriving via AsCopyable Int8 instance Clone Int8

deriving via AsCopyable Int16 instance Clone Int16

deriving via AsCopyable Int32 instance Clone Int32

deriving via AsCopyable Int64 instance Clone Int64

deriving via AsCopyable Word instance Clone Word

deriving via AsCopyable Word8 instance Clone Word8

deriving via AsCopyable Word16 instance Clone Word16

deriving via AsCopyable Word32 instance Clone Word32

deriving via AsCopyable Word64 instance Clone Word64

deriving via AsCopyable Char instance Clone Char

deriving via AsCopyable Bool instance Clone Bool

deriving via AsCopyable Integer instance Clone Integer

deriving via AsCopyable Natural instance Clone Natural

deriving via AsCopyable Double instance Clone Double

deriving via AsCopyable Float instance Clone Float

deriving via AsCopyable () instance Clone ()

type GenericClone a = (Generic a, GClone (Rep a))

genericClone :: (GenericClone a) => Share α a %1 -> BO α a
{-# INLINE genericClone #-}
genericClone (UnsafeAlias x) = to Control.<$> gclone (UnsafeAlias (from x))

type GClone :: forall {k}. (k -> Type) -> Constraint
class GClone f where
  gclone :: Share α (f x) %1 -> BO α (f x)

instance (Clone a) => GClone (K1 i a) where
  gclone = coerceLin $ clone @a

instance (GClone f, GClone g) => GClone (f :*: g) where
  gclone (UnsafeAlias (f :*: g)) =
    (:*:) Control.<$> gclone (UnsafeAlias f) Control.<*> gclone (UnsafeAlias g)

instance (GClone f) => GClone (M1 i c f) where
  gclone = \case
    UnsafeAlias (M1 x) -> coerceLin $ gclone (UnsafeAlias x)

instance (GClone f) => GClone (MP1 'One f) where
  gclone = \case
    UnsafeAlias (MP1 x) -> MP1 Control.<$> gclone (UnsafeAlias x)

instance GClone (MP1 'Many f) where
  gclone = \case
    UnsafeAlias mp1 -> Control.pure mp1

instance (GClone f, GClone g) => GClone (f :+: g) where
  gclone = \case
    UnsafeAlias (L1 x) -> L1 Control.<$> gclone (UnsafeAlias x)
    UnsafeAlias (R1 x) -> R1 Control.<$> gclone (UnsafeAlias x)

instance GClone U1 where
  gclone = Control.pure . coerceLin . unsafeUnalias

instance GClone V1 where
  gclone = \case {} . unsafeUnalias

instance (GenericClone a) => Clone (Generically a) where
  clone = Control.fmap Generically . genericClone . unsafeMapAlias (\(Generically x) -> x)

deriving via
  Generically (a, b)
  instance
    (Clone a, Clone b) => Clone (a, b)

deriving via
  Generically (a, b, c)
  instance
    (Clone a, Clone b, Clone c) => Clone (a, b, c)

deriving via
  Generically (a, b, c, d)
  instance
    (Clone a, Clone b, Clone c, Clone d) => Clone (a, b, c, d)

deriving via
  Generically (a, b, c, d, e)
  instance
    (Clone a, Clone b, Clone c, Clone d, Clone e) => Clone (a, b, c, d, e)

deriving via
  Generically (Either a b)
  instance
    (Clone a, Clone b) => Clone (Either a b)

deriving via Generically [a] instance (Clone a) => Clone [a]

deriving via Generically (Maybe a) instance (Clone a) => Clone (Maybe a)

deriving via Generically (NonEmpty a) instance (Clone a) => Clone (NonEmpty a)

-- | Lifting of the 'Clone' operation to unary type constructors.
class Clone1 f where
  liftClone :: (Share α a %1 -> BO α b) -> Share α (f a) %1 -> BO α (f b)
  default liftClone :: (GenericClone1 f) => (Share α a %1 -> BO α b) -> Share α (f a) %1 -> BO α (f b)
  liftClone = genericLiftClone

clone1 :: (Clone1 f, Clone a) => Share α (f a) %1 -> BO α (f a)
{-# INLINE clone1 #-}
clone1 = liftClone clone

type GenericClone1 f = (Clone1 (Rep1 @Type f), Generic1 f)

genericLiftClone :: forall f a b α. (GenericClone1 f) => (Share α a %1 -> BO α b) -> Share α (f a) %1 -> BO α (f b)
{-# INLINE genericLiftClone #-}
genericLiftClone f (UnsafeAlias x) =
  to1 Control.<$> liftClone f (UnsafeAlias $ from1 x)

genericClone1 :: forall f a α. (GenericClone1 f, Clone a) => Share α (f a) %1 -> BO α (f a)
{-# INLINE genericClone1 #-}
genericClone1 = genericLiftClone clone

instance (GenericClone1 f) => Clone1 (Generically1 @Type f) where
  liftClone f = Control.fmap Generically1 . genericLiftClone f . coerceShr
  {-# INLINE liftClone #-}

instance (Clone a) => Clone1 (K1 i a) where
  liftClone _ = coerce $! clone @a
  {-# INLINE liftClone #-}

instance Clone1 Par1 where
  liftClone f = coerceLin . f . coerceShr
  {-# INLINE liftClone #-}

instance (Clone1 f) => Clone1 (M1 i c f) where
  liftClone f = Control.fmap M1 . liftClone f . coerceShr @_
  {-# INLINE liftClone #-}

instance (Clone1 f, Clone1 g) => Clone1 (f :*: g) where
  liftClone f (UnsafeAlias (f' :*: g')) =
    (:*:)
      Control.<$> liftClone f (UnsafeAlias f')
      Control.<*> liftClone f (UnsafeAlias g')

instance (Clone1 f, Clone1 g) => Clone1 (f :+: g) where
  liftClone f = \case
    UnsafeAlias (L1 x) -> Control.fmap L1 . liftClone f . coerceShr $ UnsafeAlias x
    UnsafeAlias (R1 x) -> Control.fmap R1 . liftClone f . coerceShr $ UnsafeAlias x
  {-# INLINE liftClone #-}

instance (Clone1 f, Clone1 g) => Clone1 (f :.: g) where
  liftClone f = \(UnsafeAlias (Comp1 x)) -> Control.fmap Comp1 . liftClone (liftClone f) $ UnsafeAlias x
  {-# INLINE liftClone #-}

instance Clone1 U1 where
  liftClone _ = coerceLin . gclone
  {-# INLINE liftClone #-}

instance Clone1 V1 where
  liftClone _ = \case {} . unsafeUnalias
  {-# INLINE liftClone #-}

coerceShr :: (Coercible a b) => Share α a %1 -> Share α b
coerceShr = Unsafe.toLinear \ !a -> coerce a

deriving via Generically1 Maybe instance Clone1 Maybe

deriving via Generically1 [] instance Clone1 []

deriving via Generically1 Proxy instance Clone1 Proxy

deriving via Generically1 NonEmpty instance Clone1 NonEmpty

deriving via Generically1 (Either a) instance (Clone a) => Clone1 (Either a)

deriving via Generically1 ((,) a) instance (Clone a) => Clone1 ((,) a)
