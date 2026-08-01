{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted.Internal (
  module Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted.Internal,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Clone
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
import Data.Unrestricted.Linear qualified as Ur
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Mutable (RealWorld)
import GHC.Exts qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.TypeError
import Prelude.Linear hiding (head, last, splitAt)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

-- | Linearly owned mutable vector of unrestricted elements.
newtype Vector v a = Vector {content :: G.Mutable v RealWorld a}

type role Vector nominal nominal

-- | Construct a fixed-size view over a raw mutable-vector slice.
unsafeFromMutableSlice ::
  (G.Vector v a) =>
  Int ->
  Int ->
  G.Mutable v RealWorld a %1 ->
  Vector v a
{-# INLINE unsafeFromMutableSlice #-}
unsafeFromMutableSlice offset length_ =
  Unsafe.toLinear \buffer ->
    Vector (GM.unsafeSlice offset length_ buffer)

instance LinearOnly (Vector v a) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance
  (Unsatisfiable (ShowType (Vector v a) :<>: Text " cannot be copied!")) =>
  Copyable (Vector v a)
  where
  copy = unsatisfiable

instance Consumable (Vector v a) where
  consume = Unsafe.toLinear \_ -> ()
  {-# INLINE consume #-}

instance (G.Vector v a) => Clone (Vector v a) where
  clone = Unsafe.toLinear \(UnsafeAlias (Vector vector)) ->
    Vector Control.<$> unsafeSystemIOToBO (GM.clone vector)
  {-# INLINE clone #-}

-- | \(O(1)\). Construct an empty vector.
empty :: (G.Vector v a) => Linearly %1 -> Vector v a
{-# NOINLINE empty #-}
empty =
  GHC.noinline \linear ->
    linear `lseq` Vector (unsafePerformIO (GM.unsafeNew 0))

{- | \(O(n)\). Construct a vector containing @count@ copies of a value.

As in @vector@, a negative count produces an empty vector.
-}
constant ::
  (G.Vector v a) =>
  Int ->
  a ->
  Linearly %1 ->
  Vector v a
{-# NOINLINE constant #-}
constant =
  GHC.noinline \count value linear ->
    linear `lseq` Vector (unsafePerformIO (GM.replicate count value))

-- | \(O(n)\). Copy an unrestricted list into a new vector.
fromList ::
  (G.Vector v a) =>
  [a] ->
  Linearly %1 ->
  Vector v a
{-# NOINLINE fromList #-}
fromList =
  GHC.noinline \values linear ->
    linear `lseq`
      Vector
        (unsafePerformIO (G.thaw (G.fromList values)))

-- | \(O(n)\). Copy an immutable vector into a new owner.
fromVector ::
  (G.Vector v a) =>
  v a ->
  Linearly %1 ->
  Vector v a
{-# NOINLINE fromVector #-}
fromVector =
  GHC.noinline \source linear ->
    linear `lseq` Vector (unsafePerformIO (G.thaw source))

{- | \(O(1)\). Unsafely take ownership of an immutable vector.

The caller must ensure that no alias of the source vector, including an
overlapping slice, is ever observed again.
-}
unsafeFromVector ::
  (G.Vector v a) =>
  v a %1 ->
  Linearly %1 ->
  Vector v a
{-# NOINLINE unsafeFromVector #-}
unsafeFromVector =
  GHC.noinline $
    Unsafe.toLinear \source linear ->
      linear `lseq` Vector (unsafePerformIO (G.unsafeThaw source))

{- | \(O(1)\). Unsafely take ownership of a mutable vector.

The caller must not retain any alias or overlapping slice of the input. The
entire adopted slice must be initialized.
-}
unsafeFromMutable ::
  (G.Vector v a) =>
  G.Mutable v state a %1 ->
  Linearly %1 ->
  Vector v a
{-# INLINE unsafeFromMutable #-}
unsafeFromMutable =
  Unsafe.toLinear \source linear ->
    linear `lseq` Vector (Unsafe.coerce source)

-- | \(O(1)\). Consume the owner and freeze its backing storage.
toVector ::
  (G.Vector v a) =>
  Vector v a %1 ->
  Ur (v a)
{-# NOINLINE toVector #-}
toVector =
  GHC.noinline $
    Unsafe.toLinear \(Vector vector) ->
      Ur (unsafePerformIO (G.unsafeFreeze vector))

-- | \(O(n)\). Consume the owner and return its elements as a list.
toList ::
  (G.Vector v a) =>
  Vector v a %1 ->
  Ur [a]
{-# INLINE toList #-}
toList = Ur.lift G.toList . toVector

{- | \(O(n)\). Copy a live vector into an immutable vector and thread its
borrow.
-}
copyToVector ::
  (G.Vector v a, α >= β) =>
  Borrow bk α (Vector v a) %1 ->
  BO β (Ur (v a), Borrow bk α (Vector v a))
{-# INLINE copyToVector #-}
copyToVector =
  Unsafe.toLinear \vectorBorrow@(UnsafeAlias (Vector vector)) ->
    unsafeSystemIOToBO do
      snapshot <- G.freeze vector
      NonLinear.pure (Ur snapshot, vectorBorrow)

-- | \(O(1)\). Return the number of elements and thread the vector borrow.
size ::
  (G.Vector v a) =>
  Borrow bk α (Vector v a) %1 ->
  (Ur Int, Borrow bk α (Vector v a))
{-# INLINE size #-}
size =
  Unsafe.toLinear \vectorBorrow@(UnsafeAlias (Vector vector)) ->
    (Ur (GM.length vector), vectorBorrow)

-- | Read the element at an index and thread the vector borrow.
get ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  Borrow bk α (Vector v a) %1 ->
  BO β (Ur a, Borrow bk α (Vector v a))
{-# INLINE get #-}
get index vector =
  case size vector of
    (Ur length_, vector)
      | index < 0 || index >= length_ ->
          error
            ( "get: index "
                <> show index
                <> " out of bounds for length "
                <> show length_
            )
            vector
      | otherwise -> unsafeGet index vector

-- | Unchecked 'get'. The index must satisfy @0 <= index < size@.
unsafeGet ::
  (G.Vector v a, α >= β) =>
  Int ->
  Borrow bk α (Vector v a) %1 ->
  BO β (Ur a, Borrow bk α (Vector v a))
{-# INLINE unsafeGet #-}
unsafeGet index =
  Unsafe.toLinear \vectorBorrow@(UnsafeAlias (Vector vector)) ->
    unsafeSystemIOToBO do
      value <- GM.unsafeRead vector index
      NonLinear.pure (Ur value, vectorBorrow)

-- | Read the first element and thread the vector borrow.
head ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Borrow bk α (Vector v a) %1 ->
  BO β (Ur a, Borrow bk α (Vector v a))
{-# INLINE head #-}
head = get 0

-- | Unchecked 'head'. The vector must be non-empty.
unsafeHead ::
  (G.Vector v a, α >= β) =>
  Borrow bk α (Vector v a) %1 ->
  BO β (Ur a, Borrow bk α (Vector v a))
{-# INLINE unsafeHead #-}
unsafeHead = unsafeGet 0

-- | Read the last element and thread the vector borrow.
last ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Borrow bk α (Vector v a) %1 ->
  BO β (Ur a, Borrow bk α (Vector v a))
{-# INLINE last #-}
last vector =
  case size vector of
    (Ur length_, vector)
      | length_ <= 0 -> error "last: empty vector" vector
      | otherwise -> unsafeGet (length_ - 1) vector

-- | Unchecked 'last'. The vector must be non-empty.
unsafeLast ::
  (G.Vector v a, α >= β) =>
  Borrow bk α (Vector v a) %1 ->
  BO β (Ur a, Borrow bk α (Vector v a))
{-# INLINE unsafeLast #-}
unsafeLast vector =
  case size vector of
    (Ur length_, vector) -> unsafeGet (length_ - 1) vector

-- | Read an element through a shared borrow.
copyAt ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  Share α (Vector v a) ->
  BO β (Ur a)
{-# INLINE copyAt #-}
copyAt index vector = Control.do
  (value, vector) <- get index vector
  Control.pure (consume vector `lseq` value)

-- | Read an element and retain the mutable vector borrow.
copyAtMut ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  Mut α (Vector v a) %1 ->
  BO β (Ur a, Mut α (Vector v a))
{-# INLINE copyAtMut #-}
copyAtMut = get

-- | Replace an element and return the displaced value.
set ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  a ->
  Mut α (Vector v a) %1 ->
  BO β (Ur a, Mut α (Vector v a))
{-# INLINE set #-}
set index value array =
  case size array of
    (Ur length_, array)
      | index < 0 || index >= length_ ->
          error
            ( "set: index "
                <> show index
                <> " out of bounds for length "
                <> show length_
            )
            array
      | otherwise -> unsafeSet index value array

-- | Unchecked 'set'. The index must satisfy @0 <= index < size@.
unsafeSet ::
  (G.Vector v a, α >= β) =>
  Int ->
  a ->
  Mut α (Vector v a) %1 ->
  BO β (Ur a, Mut α (Vector v a))
{-# INLINE unsafeSet #-}
unsafeSet index value =
  Unsafe.toLinear \array@(UnsafeAlias (Vector vector)) ->
    unsafeSystemIOToBO do
      oldValue <- GM.unsafeExchange vector index value
      NonLinear.pure (Ur oldValue, array)

-- | Replace an element and discard the displaced unrestricted value.
write ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  a ->
  Mut α (Vector v a) %1 ->
  BO β (Mut α (Vector v a))
{-# INLINE write #-}
write index value array =
  case size array of
    (Ur length_, array)
      | index < 0 || index >= length_ ->
          error
            ( "write: index "
                <> show index
                <> " out of bounds for length "
                <> show length_
            )
            array
      | otherwise -> unsafeWrite index value array

-- | Unchecked 'write'. The index must satisfy @0 <= index < size@.
unsafeWrite ::
  (G.Vector v a, α >= β) =>
  Int ->
  a ->
  Mut α (Vector v a) %1 ->
  BO β (Mut α (Vector v a))
{-# INLINE unsafeWrite #-}
unsafeWrite index value =
  Unsafe.toLinear \array@(UnsafeAlias (Vector vector)) ->
    unsafeSystemIOToBO do
      GM.unsafeWrite vector index value
      NonLinear.pure array

{- | Transform an element and return an auxiliary unrestricted result.

The callback receives and returns unrestricted values. The mutable vector
borrow is unavailable until the replacement has been written. This is a
normal-return guarantee; no owner recovery is promised after an exception.
-}
update ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  (a -> BO β (Ur result, Ur a)) ->
  Mut α (Vector v a) %1 ->
  BO β (Ur result, Mut α (Vector v a))
{-# INLINE update #-}
update index action array =
  case size array of
    (Ur length_, array)
      | index < 0 || index >= length_ ->
          error
            ( "update: index "
                <> show index
                <> " out of bounds for length "
                <> show length_
            )
            array
      | otherwise -> unsafeUpdate index action array

{- | Unchecked 'update'. The index must satisfy @0 <= index < size@.

The callback receives and returns unrestricted values. The mutable vector
borrow is unavailable until the replacement has been written. This is a
normal-return guarantee; no owner recovery is promised after an exception.
-}
unsafeUpdate ::
  (G.Vector v a, α >= β) =>
  Int ->
  (a -> BO β (Ur result, Ur a)) ->
  Mut α (Vector v a) %1 ->
  BO β (Ur result, Mut α (Vector v a))
{-# INLINE unsafeUpdate #-}
unsafeUpdate index action =
  Unsafe.toLinear \(UnsafeAlias array@(Vector vector)) -> Control.do
    Ur value <-
      unsafeSystemIOToBO do
        value <- GM.unsafeRead vector index
        NonLinear.pure (Ur value)
    (result, Ur updatedValue) <- action value
    () <- unsafeSystemIOToBO (GM.unsafeWrite vector index updatedValue)
    Control.pure (result, UnsafeAlias array)

-- | Transform an element.
modify ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  (a -> a) ->
  Mut α (Vector v a) %1 ->
  BO β (Mut α (Vector v a))
{-# INLINE modify #-}
modify index function array = Control.do
  (Ur (), array) <-
    update
      index
      (\value -> Control.pure (Ur (), Ur (function value)))
      array
  Control.pure array

-- | Swap two elements.
swap ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Mut α (Vector v a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (Vector v a))
{-# INLINE swap #-}
swap array first second =
  case size array of
    (Ur length_, array)
      | first
          < 0
          || first
          >= length_
          || second
          < 0
          || second
          >= length_ ->
          error
            ( "swap: indices "
                <> show (first, second)
                <> " out of bounds for length "
                <> show length_
            )
            array
      | otherwise -> unsafeSwap array first second

-- | Unchecked 'swap'. Both indices must satisfy @0 <= index < size@.
unsafeSwap ::
  (G.Vector v a, α >= β) =>
  Mut α (Vector v a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (Vector v a))
{-# INLINE unsafeSwap #-}
unsafeSwap =
  Unsafe.toLinear \array@(UnsafeAlias (Vector vector)) first second ->
    unsafeSystemIOToBO do
      GM.unsafeSwap vector first second
      NonLinear.pure array

{- | Split a borrow into two disjoint fixed ranges without copying.

The index is clamped to @[0, size]@, matching @vector@'s 'GM.splitAt'.
For a custom backend, safety requires its two returned slices not to overlap.
-}
splitAt ::
  (G.Vector v a) =>
  Int ->
  Borrow bk α (Vector v a) %1 ->
  ( Borrow bk α (Vector v a)
  , Borrow bk α (Vector v a)
  )
{-# INLINE splitAt #-}
splitAt index =
  Unsafe.toLinear \(UnsafeAlias (Vector vector)) ->
    case GM.splitAt index vector of
      (left, right) ->
        (UnsafeAlias (Vector left), UnsafeAlias (Vector right))
