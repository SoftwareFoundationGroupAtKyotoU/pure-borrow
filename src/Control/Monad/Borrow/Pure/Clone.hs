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
import Control.Monad.Borrow.Pure.BO.Internal
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Data.Coerce (Coercible, coerce)
import Data.Complex (Complex)
import Data.Data (Proxy)
import Data.Int
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup qualified as Sem
import Data.Word
import GHC.Exts (Multiplicity (..))
import Generics.Linear
import Numeric.Natural
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

{- | @'Clone' a@ is analogous to @'Copyable' a@, but a clone is available only inside the @'BO' α@ monad.

The difference between 'Clone' and 'Copyable' is that the former allows for cloning a shared borrow of a /mutable/ or /linear/ value, while the latter requires cloning a shared borrow of an /immutable/ value.
This is because a @'Share' α a@ can be leaked through its t'Prelude.Linear.Movable' instance, and so outlive the lifetime @α@, which would leak the mutable state inside @a@ into /unrestricted/ contexts and destroy soundness.

A container that owns its contents, such as t'Data.Ref.Linear.Ref' or the boxed vectors, clones them with their own 'Clone', so its instance requires @'Clone' a@; one whose contents are GC-owned copies them and requires nothing of them.
A type gets an instance in one of these ways:

* a 'Copyable' type, with @deriving via t'AsCopyable' T instance 'Clone' T@;
* a record or sum type whose fields are 'Clone', from the default method, with @deriving anyclass instance 'Clone' T@ once it has the t'Generics.Linear.Generic' instance of linear-generics (@Generics.Linear.TH.deriveGeneric ''T@, which takes @TemplateHaskell@ and @TypeFamilies@, and linear-generics among the dependencies);
* an immutable, GC-owned value, such as a @Text@, by storing it as @t'Ur' Text@, whose 'Clone' shares its payload;
* a type that owns a resource of its own, with an instance written through "Control.Monad.Borrow.Pure.BO.Unsafe".

Such an instance must leave the original to its owner: it must neither consume the original, nor write to it, nor hand the clone any part of it that the original owns linearly or that can be written to, since shared borrows of it may be live, in this thread or in a 'Control.Monad.Borrow.Pure.parBO' sibling.
GC-owned, immutable parts may be shared, as the payload of an t'Ur' is.
It must finish the copy inside the 'BO' action it returns, before the lifetime of the borrow ends.
And each copy must depend on something of its own, or GHC merges the copies of two clones of one borrow into one, as it does in a loop that clones the same borrow.
Either allocate and fill the copy with IO actions in the state thread, as the vector instances do under 'Control.Monad.Borrow.Pure.BO.Unsafe.unsafeSystemIOToBO', or pass a t'Control.Monad.Borrow.Pure.Linearly' taken with 'Control.Monad.Borrow.Pure.BO.askLinearly' to the function that makes the copy, and make that function @NOINLINE@ and apply it through 'GHC.Exts.noinline', as below.
A pure copy is not enough however it is evaluated, even with @$!@ inside 'Control.Monad.Borrow.Pure.BO.Unsafe.unsafeSystemIOToBO'.
Neither is consuming the token beside the copy, nor passing it to a function that is only @NOINLINE@ or only @OPAQUE@: such a function's demand signature shows that it ignores the token's field, and GHC may then drop the token on the way to it.
For linear-base's arrays, whose elements are GC-owned, that gives the following, with "Control.Monad.Borrow.Pure" imported, @Alias (..)@ from "Control.Monad.Borrow.Pure.BO.Unsafe", 'Control.Monad.Borrow.Pure.BO.evaluateBO' from "Control.Monad.Borrow.Pure.BO", 'GHC.Exts.noinline', "Unsafe.Linear" as @Unsafe@ and "Data.Array.Mutable.Linear" as @Array@:

> instance Clone (Array a) where
>   clone = Unsafe.toLinear \(UnsafeAlias arr) -> Control.do
>     lin <- askLinearly
>     evaluateBO (copyArray arr lin)
>
> -- NOINLINE and applied through noinline, so that each copy depends on its own token.
> copyArray :: Array a -> Linearly %1 -> Array a
> {-# NOINLINE copyArray #-}
> copyArray = noinline \arr lin -> lin `lseq` sliceAll arr
>
> sliceAll :: Array a -> Array a
> sliceAll arr = case Array.size arr of
>   (Ur n, _) -> case Array.slice 0 n arr of
>     (_, copied) -> copied
-}
class Clone a where
  clone :: Share α a %1 -> BO α a
  default clone :: (GenericClone a) => Share α a %1 -> BO α a
  clone = genericClone

{- | Derive 'Clone' from 'Copyable': the clone is a 'copy' taken inside 'BO'.

Use it with @DerivingVia@ for types whose values are immutable, for example @deriving via AsCopyable T instance Clone T@.
-}
newtype AsCopyable a = AsCopyable a
  deriving newtype (Copyable)

instance (Copyable a) => Clone (AsCopyable a) where
  clone borrowed = Control.pure $! copy borrowed
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

-- The payload of 'Ur' is GC-owned, so the clone shares it rather than cloning it.
deriving via AsCopyable (Ur a) instance Clone (Ur a)

deriving via AsCopyable (Sum a) instance (Copyable a) => Clone (Sum a)

deriving via AsCopyable (Product a) instance (Copyable a) => Clone (Product a)

deriving via AsCopyable (Sem.Min a) instance (Copyable a) => Clone (Sem.Min a)

deriving via AsCopyable (Sem.Max a) instance (Copyable a) => Clone (Sem.Max a)

deriving via
  AsCopyable (Sem.Arg a b)
  instance
    (Copyable a, Copyable b) => Clone (Sem.Arg a b)

deriving via AsCopyable (Complex a) instance (Copyable a) => Clone (Complex a)

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
