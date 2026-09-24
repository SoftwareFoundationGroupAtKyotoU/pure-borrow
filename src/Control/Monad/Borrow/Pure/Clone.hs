{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
'Clone' copies, inside 'BO', a value reached through a shared borrow that 'Copyable' cannot copy because it is mutable or linear.

= Writing an instance by hand #hand-written#

Derive an instance when you can, in one of the ways listed under 'Clone'.
Write one by hand only for a type that holds a resource with no instance, and write it for a newtype of your own rather than as an orphan, which would clash with an instance that this library or the resource's package adds later.

Such an instance must leave the original to its owner, since shared borrows of it may be live, in this thread or in a 'Control.Monad.Borrow.Pure.parBO' sibling.
It must neither consume the original nor write to it, and must not hand the clone any part of it that the original owns linearly or that can be written to through a borrow of it.
GC-owned parts may be shared, as the payload of an t'Ur' and the elements of an t'Data.Array.Mutable.Linear.Array' are.

The copy must be new storage, complete inside the 'BO' action that the instance returns, before the lifetime of the borrow ends.
'Control.Monad.Borrow.Pure.BO.evaluateBO' evaluates only to the outermost constructor, so a function that makes the copy must return it complete at that point, with strict fields or @$!@ for each part it copies.
An operation that looks like a copy may not make one: linear-base's @Data.Vector.Mutable.Linear.slice 0@, for one, hands back a view of the same buffer.

Each copy must also depend on something of its own, or GHC merges the copies of two clones of one borrow into one, as it does in a loop that clones the same borrow.
Either allocate and fill the copy with IO actions in the state thread, as the vector instances do under 'Control.Monad.Borrow.Pure.BO.Unsafe.unsafeSystemIOToBO', or pass a t'Control.Monad.Borrow.Pure.Linearly' taken with 'Control.Monad.Borrow.Pure.BO.askLinearly' to the function that makes the copy, make that function @NOINLINE@, and define it as 'GHC.Exts.noinline' applied to a lambda, as @copyArray@ below is.
A pure copy is not enough however it is evaluated, even with @$!@ inside 'Control.Monad.Borrow.Pure.BO.Unsafe.unsafeSystemIOToBO'.
Neither is consuming the token beside the copy outside such a function, nor passing it to a function that is only @NOINLINE@ or only @OPAQUE@: such a function's demand signature shows that it ignores the token's field, and GHC may then drop the token on the way to it.
Inside the function, consuming the token is enough, since 'GHC.Exts.noinline' hides from GHC that the function ignores it.

The library's instance for linear-base's arrays is written this way, as follows.
Only @clone@, and the signature, the pragma, the 'GHC.Exts.noinline' and the 'Prelude.Linear.lseq' on the token of @copyArray@, are the pattern.
The rest of @copyArray@, and @sameStorage@, are how an array is copied; for a type of your own, call its own allocating copy there instead.
The array's own 'dup2' makes the copy in one pass and only reads the array it is given, but it does not say which of its results is the copy, so the instance compares their storage with the original's.
A container that owns its contents must not clone them with 'dup2' at all: see Note [Cloning the contents of a shared borrow] in @Data.Ref.Linear.Internal@.

> {-# LANGUAGE BlockArguments #-}
> {-# LANGUAGE ImportQualifiedPost #-}
> {-# LANGUAGE LinearTypes #-}
> {-# LANGUAGE MagicHash #-}
> {-# LANGUAGE QualifiedDo #-}
>
> import Control.Functor.Linear qualified as Control
> import Control.Monad.Borrow.Pure
> import Control.Monad.Borrow.Pure.BO (evaluateBO)
> import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..))
> import Data.Array.Mutable.Linear (Array)
> import Data.Array.Mutable.Linear.Internal qualified as ArrayInternal
> import Data.Array.Mutable.Unlifted.Linear qualified as Unlifted
> import GHC.Exts (isTrue#, noinline, sameMutableArray#)
> import Prelude.Linear (lseq, unur)
> import Unsafe.Linear qualified as Unsafe
>
> instance Clone (Array a) where
>   clone = Unsafe.toLinear \(UnsafeAlias arr) -> Control.do
>     lin <- askLinearly
>     -- Evaluated here, in this thread, so that the copy runs once.
>     evaluateBO (copyArray arr lin)
>
> -- NOINLINE and applied through noinline, so that each copy depends on its own token.
> copyArray :: Array a -> Linearly %1 -> Array a
> {-# NOINLINE copyArray #-}
> copyArray = noinline \arr lin ->
>   lin `lseq` case arr of
>     -- Read the storage once, and copy and compare that, whatever becomes of arr meanwhile.
>     ArrayInternal.Array storage -> case dup2 (ArrayInternal.Array storage) of
>       -- dup2 only reads the array, but does not say which of its results is the copy.
>       (first@(ArrayInternal.Array s1), second@(ArrayInternal.Array s2))
>         | not (sameStorage s1 storage) -> first
>         | not (sameStorage s2 storage) -> second
>         | otherwise -> error "Clone (Array a): dup2 returned the original array twice"
>
> -- | Whether two arrays are one, through linear-base's own accessor, whatever the representation of Array#.
> sameStorage :: Unlifted.Array# a -> Unlifted.Array# a -> Bool
> sameStorage x y =
>   unur (Unlifted.unArray# (\mx -> unur (Unlifted.unArray# (\my -> isTrue# (sameMutableArray# mx my)) y)) x)

= Contents that are not evaluated yet #lazy#

'clone' evaluates what it copies.
A value behind a shared borrow that is still an unevaluated call, in any field stored lazily, is therefore evaluated by the clone, and two 'Control.Monad.Borrow.Pure.parBO' branches that clone or read it at once can both run the call.
That is harmless for a call that only allocates, reads, or makes a single write.
It is not for one that reads what it writes, as linear-base's @Data.Array.Mutable.Linear.map@ or a chain of reads and writes does, nor for one that writes one place twice.
The second run then reads what the first one wrote, and a clone can copy what the second run has written so far: @Array.map (+ 1)@ adds 2 to some elements, a clone can copy an array halfway through the update or hold a value that the call wrote only on the way, and a @map@ that changes the element type crashes the program.
The owners of this library evaluate each value they take over linearly, once, to weak head normal form, before anything can share it: 'Data.Ref.Linear.new', the @fromList@ of the vectors, and their writes through a 'Control.Monad.Borrow.Pure.Mut'.
A field that the value holds lazily is not evaluated, however: a component of a pair, the payload of a 'Just', an element of a list or a field of a record, including one that 'Control.Monad.Borrow.Pure.splitPair' or another @split@ hands out.
Storing a value evaluates it, so a branch that stores a 'Control.Monad.Borrow.Pure.Share' of such a field in an owner, or writes it through a 'Control.Monad.Borrow.Pure.Mut', evaluates the field too, and runs the call while a sibling that reads or clones the field can run it as well.
The BO runner keeps evaluation demanded by its action behind its guard, including strict unboxed writes.
An explicit force outside the action, such as a bang on the argument of a user's wrapper function, is still outside that protection.
Evaluate such a call before the value is shared, with a strict field, @StrictData@, or the linear @$!@ of "Prelude.Linear" where the field is built, as in @Just PL.$! Array.map f arr@ with "Prelude.Linear" imported as @PL@; the @$!@ of "Prelude" does not take a linear argument.
@$!@ reaches only the outermost constructor, so evaluate each such call that a record, a list or a vector holds, not only the container.
-}
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
import Control.Monad.Borrow.Pure.Lifetime.Token (Linearly)
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Data.Array.Mutable.Linear (Array)
import Data.Array.Mutable.Linear.Internal qualified as ArrayInternal
import Data.Array.Mutable.Unlifted.Linear qualified as Unlifted
import Data.Coerce (Coercible, coerce)
import Data.Complex (Complex)
import Data.Data (Proxy)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Mutable.Linear.Internal qualified as LinearHashMap
import Data.Int
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid qualified as Monoid
import Data.Ord (Down (..))
import Data.Semigroup qualified as Sem
import Data.Set.Mutable.Linear.Internal qualified as LinearSet
import Data.Vector.Mutable.Linear.Internal qualified as LinearVector
import Data.Word
import GHC.Exts (Multiplicity (..), isTrue#, noinline, sameMutableArray#)
import Generics.Linear
import Numeric.Natural
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

{- | @'Clone' a@ is analogous to @'Copyable' a@, but a clone is available only inside the @'BO' α@ monad.

The difference between 'Clone' and 'Copyable' is that the former allows for cloning a shared borrow of a /mutable/ or /linear/ value, while the latter requires cloning a shared borrow of an /immutable/ value.
This is because a @'Share' α a@ can be leaked through its t'Prelude.Linear.Movable' instance, and so outlive the lifetime @α@, which would leak the mutable state inside @a@ into /unrestricted/ contexts and destroy soundness.

A container that owns its contents, such as t'Data.Ref.Linear.Ref' or the boxed and unboxed owning vectors, clones them with their own 'Clone', so its instance requires @'Clone' a@.
One whose contents are GC-owned copies its own storage, shares the contents, and requires nothing of them.
Linear-base's Array, Vector, HashMap and Set have GC-owned elements: their operations take elements unrestricted and hand them out in t'Ur'.

A type gets an instance in one of these ways:

* a 'Copyable' type, with @deriving via t'AsCopyable' T instance 'Clone' T@;
* a newtype over a type that has an instance, with @deriving newtype ('Clone')@;
* a record or sum type whose fields are 'Clone', from the default method, with @deriving anyclass instance 'Clone' T@ once it has the t'Generics.Linear.Generic' instance of linear-generics (@Generics.Linear.TH.deriveGeneric ''T@, which takes @DataKinds@, @TemplateHaskell@ and @TypeFamilies@, and linear-generics among the dependencies);
* an immutable, GC-owned value, such as a @Text@, by storing it as @t'Ur' Text@, whose 'Clone' shares its payload;
* a type that holds a resource with no instance, by hand, as [Writing an instance by hand]("Control.Monad.Borrow.Pure.Clone#hand-written") describes.

The deriving clauses take @DerivingStrategies@, and @DerivingVia@ or @DeriveAnyClass@ where they name that strategy.

'clone' evaluates what it copies, which matters for a value that is still an unevaluated call: see [Contents that are not evaluated yet]("Control.Monad.Borrow.Pure.Clone#lazy").
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

deriving newtype instance (Clone a) => Clone (Identity a)

deriving newtype instance (Clone a) => Clone (Down a)

deriving newtype instance (Clone a) => Clone (Const a b)

deriving newtype instance (Clone a) => Clone (Sem.Dual a)

deriving newtype instance (Clone a) => Clone (Sem.First a)

deriving newtype instance (Clone a) => Clone (Sem.Last a)

deriving newtype instance (Clone a) => Clone (Sem.WrappedMonoid a)

deriving newtype instance (Clone (f a)) => Clone (Monoid.Alt f a)

deriving via AsCopyable Monoid.Any instance Clone Monoid.Any

deriving via AsCopyable Monoid.All instance Clone Monoid.All

{- | \(O(n)\). Copy the array into a new one.

The elements are GC-owned, so the copy shares them, and nothing is required of them.
The original is only read, so any number of 'Control.Monad.Borrow.Pure.parBO' branches may clone the same evaluated array at once.
'clone' evaluates the array, however, and so runs whatever unevaluated call the array still is: see [Contents that are not evaluated yet]("Control.Monad.Borrow.Pure.Clone#lazy").
This instance is the example in [Writing an instance by hand]("Control.Monad.Borrow.Pure.Clone#hand-written").
-}
instance Clone (Array a) where
  clone = Unsafe.toLinear \(UnsafeAlias arr) -> Control.do
    lin <- askLinearly
    -- Evaluated here, in this thread, so that the copy runs once.
    evaluateBO (copyArray arr lin)

-- NOINLINE and applied through noinline, so that each copy depends on its own token.
copyArray :: Array a -> Linearly %1 -> Array a
{-# NOINLINE copyArray #-}
copyArray = noinline \arr lin ->
  lin `lseq` case arr of
    -- Read the storage once, and copy and compare that, whatever becomes of arr meanwhile.
    ArrayInternal.Array storage -> case dup2 (ArrayInternal.Array storage) of
      -- dup2 only reads the array, but does not say which of its results is the copy.
      (first@(ArrayInternal.Array s1), second@(ArrayInternal.Array s2))
        | not (sameStorage s1 storage) -> first
        | not (sameStorage s2 storage) -> second
        | otherwise -> error "Clone (Array a): dup2 returned the original array twice"

-- | Whether two arrays are one, through linear-base's own accessor, whatever the representation of Array#.
sameStorage :: Unlifted.Array# a -> Unlifted.Array# a -> Bool
sameStorage x y =
  unur (Unlifted.unArray# (\mx -> unur (Unlifted.unArray# (\my -> isTrue# (sameMutableArray# mx my)) y)) x)

{-
Note [Cloning linear-base containers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Linear-base's Vector, HashMap and Set own their backing arrays, but their element operations take unrestricted arguments and return Ur values.
Their clones therefore copy the complete backing array, including spare capacity, and share the GC-owned elements without requiring Clone on them.
Vector's Vec fields are strict through StrictData; HashMap's fields have explicit bangs, and Set is a newtype over HashMap.
Forcing these wrappers exposes a completed backing array rather than an unevaluated in-place operation in a lazy field.
These representation and ownership assumptions hold in linear-base 0.7.0 and 0.8.1; the test suite pins the unrestricted element signatures.
Array's clone completes its copy in BO and obtains a fresh allocation token for every call, including repeated clones of the same shared borrow.
-}

-- | Copy the backing array, preserving capacity and sharing GC-owned elements.
instance Clone (LinearVector.Vector a) where
  clone = Unsafe.toLinear \(UnsafeAlias (LinearVector.Vec count buffer)) ->
    LinearVector.Vec count Control.<$> clone (UnsafeAlias buffer)
  {-# INLINE clone #-}

-- | Copy the slot array, sharing GC-owned keys and values.
instance Clone (LinearHashMap.HashMap k v) where
  clone = Unsafe.toLinear \(UnsafeAlias (LinearHashMap.HashMap count capacity buffer)) ->
    LinearHashMap.HashMap count capacity Control.<$> clone (UnsafeAlias buffer)
  {-# INLINE clone #-}

-- | Copy the backing hash map, sharing GC-owned elements.
instance Clone (LinearSet.Set a) where
  clone = Unsafe.toLinear \(UnsafeAlias (LinearSet.Set table)) ->
    LinearSet.Set Control.<$> clone (UnsafeAlias table)
  {-# INLINE clone #-}

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
  Generically (a, b, c, d, e, f)
  instance
    (Clone a, Clone b, Clone c, Clone d, Clone e, Clone f) => Clone (a, b, c, d, e, f)

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
