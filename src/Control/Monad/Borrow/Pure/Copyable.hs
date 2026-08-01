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
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

import Control.Monad.Borrow.Pure.BO.Internal
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Data.Array.Mutable.Linear (Array)
import Data.Complex
import Data.Int
import Data.Kind (Constraint, Type)
import Data.Semigroup qualified as Sem
import Data.Vector.Mutable.Linear (Vector)
import Data.Word
import GHC.TypeError (ErrorMessage (..))
import Generics.Linear
import Numeric.Natural (Natural)
import Prelude.Linear
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)
import Unsafe.Linear qualified as Unsafe

-- | Values that can be copied from a live borrow.
class Copyable a where
  {- | Copy the borrowed value.

  Evaluating this method must complete the copy and return the result in
  weak head normal form. It must not return a thunk whose evaluation depends
  on the borrow remaining live. Composite instances must complete their
  component copies as well, so copying a recursive value traverses its finite
  structure before returning.
  -}
  copy :: Borrow bk α a %1 -> a

instance Copyable (Ur a) where
  copy (UnsafeAlias (Ur !a)) = Ur a
  {-# INLINE copy #-}

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

instance (Copyable a) => Copyable (Complex a) where
  copy = \(UnsafeAlias (!real :+ !imaginary)) ->
    let !realCopy = copy (UnsafeAlias real)
        !imaginaryCopy = copy (UnsafeAlias imaginary)
     in realCopy :+ imaginaryCopy
  {-# INLINE copy #-}

instance Consumable (Complex Double) where
  consume = Unsafe.toLinear \_ -> ()
  {-# INLINE consume #-}

instance Dupable (Complex Double) where
  dup2 = Unsafe.toLinear \value -> (value, value)
  {-# INLINE dup2 #-}

instance Movable (Complex Double) where
  move = Unsafe.toLinear \ !value -> Ur value
  {-# INLINE move #-}

deriving via Generically1 Complex instance Copyable1 Complex

type GenericCopyable a = (Generic a, GCopyable (Rep a))

genericCopy :: (GenericCopyable a) => Borrow bk α a %1 -> a
{-# INLINE genericCopy #-}
genericCopy (UnsafeAlias x) =
  let !representation = gcopy (UnsafeAlias (from x))
   in to $! representation

type GCopyable :: forall {k}. (k -> Type) -> Constraint
class GCopyable f where
  gcopy :: Borrow bk α (f x) %1 -> f x

instance (Copyable a) => GCopyable (K1 i a) where
  gcopy = \(UnsafeAlias (K1 !a)) -> K1 $! copy (UnsafeAlias a)
  {-# INLINE gcopy #-}

instance (GCopyable f, GCopyable g) => GCopyable (f :*: g) where
  gcopy (UnsafeAlias (!f :*: !g)) =
    let !fCopy = gcopy (UnsafeAlias f)
        !gCopy = gcopy (UnsafeAlias g)
     in fCopy :*: gCopy

instance (GCopyable f) => GCopyable (M1 i c f) where
  gcopy = \case
    UnsafeAlias (M1 !x) -> M1 $! gcopy (UnsafeAlias x)

instance (GCopyable f) => GCopyable (MP1 m f) where
  gcopy = \case
    UnsafeAlias (MP1 !x) -> MP1 $! gcopy (UnsafeAlias x)

instance (GCopyable f, GCopyable g) => GCopyable (f :+: g) where
  gcopy = \case
    UnsafeAlias (L1 !x) -> L1 $! gcopy (UnsafeAlias x)
    UnsafeAlias (R1 !x) -> R1 $! gcopy (UnsafeAlias x)

instance GCopyable U1 where
  gcopy = \case
    UnsafeAlias U1 -> U1

instance GCopyable V1 where
  gcopy = \case {} . unsafeUnalias

instance (GenericCopyable a) => Copyable (Generically a) where
  copy borrow =
    Generically $!
      genericCopy
        (unsafeMapAlias (\(Generically x) -> x) borrow)

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
  copy borrow =
    AsCopyable1 $!
      copy1
        (unsafeMapAlias (\(AsCopyable1 x) -> x) borrow)
  {-# INLINE copy #-}

-- | Lifting of the 'Copyable' operation to unary type constructors.
class Copyable1 f where
  {- | Copy every contained value and force each result to weak head normal
  form before returning. Recursive structures are traversed before the borrow
  can end.
  -}
  liftCopy :: (Borrow bk α a %1 -> b) -> Borrow bk α (f a) %1 -> f b

type GenericCopyable1 f = (Copyable1 (Rep1 @Type f), Generic1 f)

genericLiftCopy :: forall f bk α a b. (GenericCopyable1 f) => (Borrow bk α a %1 -> b) -> Borrow bk α (f a) %1 -> f b
{-# INLINE genericLiftCopy #-}
genericLiftCopy f (UnsafeAlias x) =
  let !representation = liftCopy f (UnsafeAlias $ from1 x)
   in to1 $! representation

genericCopy1 :: forall f a α. (GenericCopyable1 f, Copyable a) => Share α (f a) %1 -> f a
{-# INLINE genericCopy1 #-}
genericCopy1 = genericLiftCopy copy

copy1 :: (Copyable1 f, Copyable a) => Borrow bk α (f a) %1 -> f a
{-# INLINE copy1 #-}
copy1 borrow =
  let !copied = liftCopy copy borrow
   in copied

instance (GenericCopyable1 f) => Copyable1 (Generically1 @Type f) where
  liftCopy f borrow =
    Generically1 $! genericLiftCopy f (coerceLin borrow)
  {-# INLINE liftCopy #-}

instance (Copyable c) => Copyable1 (K1 i c) where
  liftCopy _ borrow =
    K1 $! copy @c (coerceLin borrow)
  {-# INLINE liftCopy #-}

instance Copyable1 Par1 where
  liftCopy f borrow =
    Par1 $! f (coerceLin borrow)
  {-# INLINE liftCopy #-}

instance (Copyable1 f) => Copyable1 (M1 i c f) where
  liftCopy f borrow =
    M1 $! liftCopy f (coerceLin borrow)
  {-# INLINE liftCopy #-}

instance (Copyable1 l, Copyable1 r) => Copyable1 (l :*: r) where
  liftCopy f = \(UnsafeAlias (!l :*: !r)) ->
    let !l' = liftCopy f (UnsafeAlias l)
        !r' = liftCopy f (UnsafeAlias r)
     in l' :*: r'
  {-# INLINE liftCopy #-}

instance (Copyable1 f, Copyable1 g) => Copyable1 (f :.: g) where
  liftCopy f = \(UnsafeAlias (Comp1 !x)) ->
    Comp1 $! liftCopy (liftCopy f) (UnsafeAlias x)
  {-# INLINE liftCopy #-}

instance (Copyable1 l, Copyable1 r) => Copyable1 (l :+: r) where
  liftCopy f = \(UnsafeAlias sum) -> case sum of
    L1 !l -> L1 $! (liftCopy f (UnsafeAlias l))
    R1 !r -> R1 $! (liftCopy f (UnsafeAlias r))
  {-# INLINE liftCopy #-}

instance Copyable1 U1 where
  liftCopy _ = \case
    UnsafeAlias U1 -> U1
  {-# INLINE liftCopy #-}

instance Copyable1 V1 where
  liftCopy _ = \case {} . unsafeUnalias
  {-# INLINE liftCopy #-}

{- | A variant of 'copy' that returns 'Ur' wrapped copy of the value.
'Ur' wrapper was not necessary because 'Share' is always introduced unrestricted,
whereas 'Mut' is introduced linearly, so it is convenient to have 'Ur' wrapped version.
-}
copyMut :: (Copyable a) => Mut α a %1 -> Ur a
{-# INLINE copyMut #-}
copyMut mut =
  let !(Ur shr) = share mut
   in Ur $! copy shr
