{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Monad.Borrow.Pure.Copyable (
  Copyable (..),
  copyMut,
  genericCopy,
  GenericCopyable,
  Copyable1 (..),
  AsCopyable1 (..),
  GenericCopyable1,
  copy1,
  genericCopy1,
  genericLiftCopy,
) where

import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Data.Array.Mutable.Linear (Array)
import Data.Int
import Data.Kind (Constraint, Type)
import Data.Ref.Linear (Ref)
import Data.Semigroup qualified as Sem
import Data.Vector.Mutable.Linear (Vector)
import Data.Word
import GHC.TypeError (ErrorMessage (..))
import Generics.Linear
import Numeric.Natural (Natural)
import Prelude.Linear
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)

class Copyable a where
  copy :: Borrow bk α a %1 -> a

instance (Copyable a) => Copyable (Ur a) where
  copy (UnsafeAlias (Ur !a)) = Ur $! copy $! UnsafeAlias a
  {-# INLINE copy #-}

instance
  (Unsatisfiable (ShowType (Ref a) :<>: Text " cannot be copied!")) =>
  Copyable (Ref a)
  where
  copy = unsatisfiable

instance
  (Unsatisfiable (ShowType (Array a) :<>: Text " cannot be copied!")) =>
  Copyable (Array a)
  where
  copy = unsatisfiable

instance
  (Unsatisfiable (ShowType (Vector a) :<>: Text " cannot be copied!")) =>
  Copyable (Vector a)
  where
  copy = unsatisfiable

newtype UnsafeAssumeNoVar a = UnsafeAssumeNoVar a

instance Copyable (UnsafeAssumeNoVar a) where
  copy = \(UnsafeAlias !a) -> a
  {-# INLINE copy #-}

deriving via UnsafeAssumeNoVar Int instance Copyable Int

deriving via UnsafeAssumeNoVar Int8 instance Copyable Int8

deriving via UnsafeAssumeNoVar Int16 instance Copyable Int16

deriving via UnsafeAssumeNoVar Int32 instance Copyable Int32

deriving via UnsafeAssumeNoVar Int64 instance Copyable Int64

deriving via UnsafeAssumeNoVar Word instance Copyable Word

deriving via UnsafeAssumeNoVar Word8 instance Copyable Word8

deriving via UnsafeAssumeNoVar Word16 instance Copyable Word16

deriving via UnsafeAssumeNoVar Word32 instance Copyable Word32

deriving via UnsafeAssumeNoVar Word64 instance Copyable Word64

deriving via UnsafeAssumeNoVar Integer instance Copyable Integer

deriving via UnsafeAssumeNoVar Natural instance Copyable Natural

deriving via UnsafeAssumeNoVar Float instance Copyable Float

deriving via UnsafeAssumeNoVar Double instance Copyable Double

deriving via UnsafeAssumeNoVar Char instance Copyable Char

deriving via UnsafeAssumeNoVar Bool instance Copyable Bool

type GenericCopyable a = (Generic a, GCopyable (Rep a))

genericCopy :: (GenericCopyable a) => Borrow bk α a %1 -> a
{-# INLINE genericCopy #-}
genericCopy (UnsafeAlias x) = to (gcopy (UnsafeAlias (from x)))

type GCopyable :: forall {k}. (k -> Type) -> Constraint
class GCopyable f where
  gcopy :: Borrow bk α (f x) %1 -> f x

instance (Copyable a) => GCopyable (K1 i a) where
  gcopy = \(UnsafeAlias (K1 !a)) -> K1 (copy (UnsafeAlias a))
  {-# INLINE gcopy #-}

instance (GCopyable f, GCopyable g) => GCopyable (f :*: g) where
  gcopy (UnsafeAlias (!f :*: !g)) =
    gcopy (UnsafeAlias f) :*: gcopy (UnsafeAlias g)

instance (GCopyable f) => GCopyable (M1 i c f) where
  gcopy = \case
    UnsafeAlias (M1 !x) -> M1 (gcopy (UnsafeAlias x))

instance (GCopyable f) => GCopyable (MP1 m f) where
  gcopy = \case
    UnsafeAlias (MP1 !x) -> MP1 (gcopy (UnsafeAlias x))

instance (GCopyable f, GCopyable g) => GCopyable (f :+: g) where
  gcopy = \case
    UnsafeAlias (L1 !x) -> L1 (gcopy (UnsafeAlias x))
    UnsafeAlias (R1 !x) -> R1 (gcopy (UnsafeAlias x))

instance GCopyable U1 where
  gcopy = \case
    UnsafeAlias U1 -> U1

instance GCopyable V1 where
  gcopy = \case {} . unsafeUnalias

instance (GenericCopyable a) => Copyable (Generically a) where
  copy = Generically . genericCopy . unsafeMapAlias (\(Generically x) -> x)

deriving via Generically () instance Copyable ()

deriving via
  Generically (Sum a)
  instance
    (Copyable a) => Copyable (Sum a)

deriving via
  Generically (Product a)
  instance
    (Copyable a) => Copyable (Product a)

deriving via
  Generically [a]
  instance
    (Copyable a) => Copyable [a]

deriving via
  Generically (Sem.Max a)
  instance
    (Copyable a) => Copyable (Sem.Max a)

deriving via
  Generically (Maybe a)
  instance
    (Copyable a) => Copyable (Maybe a)

deriving via
  Generically (Sem.Min a)
  instance
    (Copyable a) => Copyable (Sem.Min a)

deriving via
  Generically (a, b)
  instance
    (Copyable a, Copyable b) =>
    Copyable (a, b)

deriving via
  Generically (a, b, c)
  instance
    (Copyable a, Copyable b, Copyable c) =>
    Copyable (a, b, c)

deriving via
  Generically (a, b, c, d)
  instance
    (Copyable a, Copyable b, Copyable c, Copyable d) =>
    Copyable (a, b, c, d)

deriving via
  Generically (Either a b)
  instance
    (Copyable a, Copyable b) => Copyable (Either a b)

deriving via
  Generically (Sem.Arg a b)
  instance
    (Copyable a, Copyable b) => Copyable (Sem.Arg a b)

newtype AsCopyable1 f a = AsCopyable1 (f a)

instance (Copyable1 f, Copyable a) => Copyable (AsCopyable1 f a) where
  copy = AsCopyable1 . copy1 . unsafeMapAlias \(AsCopyable1 x) -> x
  {-# INLINE copy #-}

-- | Lifting of the 'Copyable' operation to unary type constructors.
class Copyable1 f where
  liftCopy :: (Borrow bk α a %1 -> b) -> Borrow bk α (f a) %1 -> f b

type GenericCopyable1 f = (Copyable1 (Rep1 @Type f), Generic1 f)

genericLiftCopy :: forall f bk a b α. (GenericCopyable1 f) => (Borrow bk α a %1 -> b) -> Borrow bk α (f a) %1 -> f b
{-# INLINE genericLiftCopy #-}
genericLiftCopy f (UnsafeAlias x) = to1 $ liftCopy f (UnsafeAlias $ from1 x)

genericCopy1 :: forall f a α. (GenericCopyable1 f, Copyable a) => Share α (f a) %1 -> f a
{-# INLINE genericCopy1 #-}
genericCopy1 = genericLiftCopy copy

copy1 :: (Copyable1 f, Copyable a) => Borrow bk α (f a) %1 -> f a
{-# INLINE copy1 #-}
copy1 = liftCopy copy

instance (GenericCopyable1 f) => Copyable1 (Generically1 @Type f) where
  liftCopy f = Generically1 . genericLiftCopy f . coerceLin
  {-# INLINE liftCopy #-}

instance (Copyable c) => Copyable1 (K1 i c) where
  liftCopy _ = coerceLin $! copy @c
  {-# INLINE liftCopy #-}

instance Copyable1 Par1 where
  liftCopy f = Par1 . f . coerceLin
  {-# INLINE liftCopy #-}

instance (Copyable1 f) => Copyable1 (M1 i c f) where
  liftCopy f = M1 . liftCopy f . coerceLin
  {-# INLINE liftCopy #-}

instance (Copyable1 l, Copyable1 r) => Copyable1 (l :*: r) where
  liftCopy f = \(UnsafeAlias (!l :*: !r)) ->
    let !l' = liftCopy f (UnsafeAlias l)
        !r' = liftCopy f (UnsafeAlias r)
     in l' :*: r'
  {-# INLINE liftCopy #-}

instance (Copyable1 f, Copyable1 g) => Copyable1 (f :.: g) where
  liftCopy f = \(UnsafeAlias (Comp1 x)) ->
    Comp1 . liftCopy (liftCopy f) $ UnsafeAlias x
  {-# INLINE liftCopy #-}

instance (Copyable1 l, Copyable1 r) => Copyable1 (l :+: r) where
  liftCopy f = \(UnsafeAlias sum) -> case sum of
    L1 !l -> L1 $! (liftCopy f (UnsafeAlias l))
    R1 !r -> R1 $! (liftCopy f (UnsafeAlias r))
  {-# INLINE liftCopy #-}

{- | A variant of 'copy' that returns 'Ur' wrapped copy of the value.
'Ur' wrapper was not necessary because 'Share' is always introduced unrestricted,
whereas 'Mut' is introduced linearly, so it is convenient to have 'Ur' wrapped version.
-}
copyMut :: (Copyable a) => Mut α a %1 -> Ur a
{-# INLINE copyMut #-}
copyMut mut =
  let !(Ur shr) = share mut
   in Ur (copy shr)
