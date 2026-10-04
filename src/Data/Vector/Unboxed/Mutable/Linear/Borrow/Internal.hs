{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Vector.Unboxed.Mutable.Linear.Borrow.Internal (
  module Data.Vector.Unboxed.Mutable.Linear.Borrow.Internal,
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
import Control.Monad.Borrow.Pure.Utils (evaluatingBundle)
import Data.Unrestricted.Linear qualified as Ur
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.Exts qualified as GHC
import GHC.IO (evaluate, unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.TypeError
import Prelude.Linear hiding (head, last, splitAt)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

-- | Unboxed linear mutable vector.
newtype Vector a = Vector {content :: UM.IOVector a}

type role Vector nominal

-- | Construct a fixed-size view over a raw mutable-vector slice.
unsafeFromMutableSlice ::
  (U.Unbox a) =>
  Int ->
  Int ->
  UM.IOVector a %1 ->
  Vector a
{-# INLINE unsafeFromMutableSlice #-}
unsafeFromMutableSlice =
  Unsafe.toLinear3 \offset length_ buffer ->
    Vector (UM.unsafeSlice offset length_ buffer)

instance LinearOnly (Vector a) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance
  (Unsatisfiable (ShowType (Vector a) :<>: Text " cannot be copied!" :$$: Text "It is mutable: clone a shared borrow of it inside BO with 'clone' instead.")) =>
  Copyable (Vector a)
  where
  copy = unsatisfiable

{- | Clone every element through its own 'Clone' into independent storage.
An unboxed representation may contain linear resources, so copying only the buffer is insufficient.
See Note [Cloning the contents of a shared borrow] in Data.Ref.Linear.Internal and the lazy-field caveat in "Control.Monad.Borrow.Pure.Clone#lazy".
-}
instance (U.Unbox a, Clone a) => Clone (Vector a) where
  clone :: forall α. Share α (Vector a) %1 -> BO α (Vector a)
  clone = Unsafe.toLinear \(UnsafeAlias (Vector buffer)) -> unsafeSystemIOToBO do
    let !count = UM.length buffer
    target <- UM.unsafeNew count
    let go !index
          | index >= count = NonLinear.pure ()
          | otherwise = do
              value <- UM.unsafeRead buffer index
              !copied <- unsafeBOToSystemIO (clone @a @α (UnsafeAlias value))
              UM.unsafeWrite target index copied
              go (index + 1)
    go 0
    NonLinear.pure (Vector target)
  {-# INLINE clone #-}

instance (U.Unbox a, Consumable a) => Consumable (Vector a) where
  consume =
    Unsafe.toLinear \(Vector vector) ->
      unsafePerformIO (consumeElements 0 (UM.length vector) vector)
  -- The traversal only reads the buffer, but it runs under
  -- 'unsafePerformIO'. Inlining would let GHC duplicate that call across use
  -- sites, or float it out of a scope, and each copy would consume the
  -- elements again; 'NOINLINE' keeps exactly one occurrence.
  {-# NOINLINE consume #-}

consumeElements ::
  (U.Unbox a, Consumable a) =>
  Int ->
  Int ->
  UM.IOVector a ->
  IO ()
{-# INLINE consumeElements #-}
consumeElements !index !length_ vector
  | index >= length_ = NonLinear.pure ()
  | otherwise = do
      value <- UM.unsafeRead vector index
      let !() = consume value
      consumeElements (index + 1) length_ vector

-- | \(O(1)\). Construct an empty vector.
empty :: (U.Unbox a) => Linearly %1 -> Vector a
{-# NOINLINE empty #-}
empty =
  GHC.noinline \linear ->
    linear `lseq` Vector (unsafePerformIO (UM.unsafeNew 0))

{- | \(O(n)\). Construct a vector containing @count@ copies of a value.

As in @vector@, a negative count produces an empty vector.
-}
constant ::
  (U.Unbox a) =>
  Int ->
  a ->
  Linearly %1 ->
  Vector a
{-# NOINLINE constant #-}
constant =
  GHC.noinline \count value linear ->
    linear `lseq` Vector (unsafePerformIO (UM.replicate count value))

{- | \(O(n)\). Move the elements of a linear list into a new vector.

Each element is evaluated to weak head normal form as it is stored, which matters for an element type whose 'U.Unbox' representation is boxed and lazy, such as @DoNotUnboxLazy@: see [Contents that are not evaluated yet]("Control.Monad.Borrow.Pure.Clone#lazy").
-}
fromList ::
  (U.Unbox a) =>
  [a] %1 ->
  Linearly %1 ->
  Vector a
{-# NOINLINE fromList #-}
fromList =
  GHC.noinline $
    Unsafe.toLinear \values linear ->
      linear `lseq` Vector (unsafePerformIO (thawEvaluated values))

{- | Store the elements of a list in a new mutable vector, each evaluated to weak head normal form.

Run inside 'unsafePerformIO', after its 'GHC.noDuplicate#', both the list and its elements are evaluated once: see Note [Stored contents are evaluated after noDuplicate#] in "Data.Ref.Linear.Unlifted.Internal".
'evaluate' keeps the demand on the list from GHC, and the list is taken as it is produced, one cell at a time, as 'U.fromList' takes it.
-}
thawEvaluated :: (U.Unbox a) => [a] -> IO (UM.IOVector a)
{-# INLINE thawEvaluated #-}
thawEvaluated values = evaluate (G.unstream (evaluatingBundle values)) NonLinear.>>= U.unsafeThaw

-- | \(O(n)\). Copy an immutable unboxed vector into a new owner.
fromVector ::
  (U.Unbox a) =>
  U.Vector a ->
  Linearly %1 ->
  Vector a
{-# NOINLINE fromVector #-}
fromVector =
  GHC.noinline \source linear ->
    linear `lseq` Vector (unsafePerformIO (U.thaw source))

{- | \(O(1)\). Unsafely take ownership of an immutable unboxed vector.

The caller must ensure that no alias of the source vector, including an
overlapping slice or compiler-introduced sharing, is ever observed again.
-}
unsafeFromVector ::
  (U.Unbox a) =>
  U.Vector a %1 ->
  Linearly %1 ->
  Vector a
{-# NOINLINE unsafeFromVector #-}
unsafeFromVector =
  GHC.noinline $
    Unsafe.toLinear \source linear ->
      linear `lseq` Vector (unsafePerformIO (U.unsafeThaw source))

{- | \(O(1)\). Unsafely take ownership of a mutable unboxed vector.

The caller must not retain any alias or overlapping slice of the input. The
entire adopted slice must be fully initialized. The state parameter is erased
only because ownership has been transferred.
-}
unsafeFromMutable ::
  (U.Unbox a) =>
  UM.MVector state a %1 ->
  Linearly %1 ->
  Vector a
{-# INLINE unsafeFromMutable #-}
unsafeFromMutable =
  Unsafe.toLinear2 \source linear ->
    linear `lseq` Vector (Unsafe.coerce source)

{- | \(O(n)\). Move every element into GC ownership, then freeze the storage.

'Movable' authorizes transferring the consumed elements into the unrestricted
immutable vector. Each 'move' may perform a deep copy.
-}
toVector ::
  (U.Unbox a, Movable a) =>
  Vector a %1 ->
  Ur (U.Vector a)
{-# NOINLINE toVector #-}
toVector =
  GHC.noinline $
    Unsafe.toLinear \(Vector vector) ->
      let !frozen =
            unsafePerformIO do
              moveElements 0 (UM.length vector) vector
              U.unsafeFreeze vector
       in Ur frozen

-- | \(O(n)\). Consume the owner and materialize its elements as a list.
toList ::
  (U.Unbox a, Movable a) =>
  Vector a %1 ->
  Ur [a]
{-# INLINE toList #-}
toList = Ur.lift U.toList . toVector

moveElements ::
  (U.Unbox a, Movable a) =>
  Int ->
  Int ->
  UM.IOVector a ->
  IO ()
{-# INLINE moveElements #-}
moveElements !index !length_ vector
  | index >= length_ = NonLinear.pure ()
  | otherwise = do
      value <- UM.unsafeRead vector index
      case move value of
        Ur !moved -> UM.unsafeWrite vector index moved
      moveElements (index + 1) length_ vector

{- | \(O(n)\). Copy a live vector into an immutable vector and thread its borrow.

Unlike 'toVector', this leaves the owner live and therefore performs a copy.
-}
copyToVector ::
  (U.Unbox a, Copyable a, α >= β) =>
  Borrow bk α (Vector a) %1 ->
  BO β (Ur (U.Vector a), Borrow bk α (Vector a))
{-# INLINE copyToVector #-}
copyToVector =
  Unsafe.toLinear \array@(UnsafeAlias (Vector vector)) ->
    unsafeSystemIOToBO do
      target <- UM.unsafeNew (UM.length vector)
      copyElements 0 (UM.length vector) vector target
      snapshot <- U.unsafeFreeze target
      NonLinear.pure (Ur snapshot, array)

copyElements ::
  (U.Unbox a, Copyable a) =>
  Int ->
  Int ->
  UM.IOVector a ->
  UM.IOVector a ->
  IO ()
{-# INLINE copyElements #-}
copyElements !index !length_ source target
  | index >= length_ = NonLinear.pure ()
  | otherwise = do
      value <- UM.unsafeRead source index
      let !copied = copy (UnsafeAlias value)
      UM.unsafeWrite target index copied
      copyElements (index + 1) length_ source target

-- | \(O(1)\). Return the number of elements and thread the vector borrow.
size ::
  (U.Unbox a) =>
  Borrow bk α (Vector a) %1 ->
  (Ur Int, Borrow bk α (Vector a))
{-# INLINE size #-}
size =
  Unsafe.toLinear \array@(UnsafeAlias (Vector vector)) ->
    (Ur (UM.length vector), array)

-- | Borrow the element at an index.
get ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Int ->
  Borrow bk α (Vector a) %1 ->
  BO β (Borrow bk α a)
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
  (U.Unbox a, α >= β) =>
  Int ->
  Borrow bk α (Vector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeGet #-}
unsafeGet =
  Unsafe.toLinear2 \index (UnsafeAlias (Vector vector)) ->
    UnsafeAlias Control.<$> unsafeSystemIOToBO (UM.unsafeRead vector index)

-- | Borrow the first element.
head ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Borrow bk α (Vector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE head #-}
head = get 0

-- | Unchecked 'head'. The vector must be non-empty.
unsafeHead ::
  (U.Unbox a, α >= β) =>
  Borrow bk α (Vector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeHead #-}
unsafeHead = unsafeGet 0

-- | Borrow the last element.
last ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Borrow bk α (Vector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE last #-}
last vector =
  case size vector of
    (Ur length_, vector)
      | length_ <= 0 -> error "last: empty vector" vector
      | otherwise -> unsafeGet (length_ - 1) vector

-- | Unchecked 'last'. The vector must be non-empty.
unsafeLast ::
  (U.Unbox a, α >= β) =>
  Borrow bk α (Vector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeLast #-}
unsafeLast vector =
  case size vector of
    (Ur length_, vector) -> unsafeGet (length_ - 1) vector

-- | Copy an element through a shared borrow.
copyAt ::
  (HasCallStack, U.Unbox a, Copyable a, α >= β) =>
  Int ->
  Share α (Vector a) ->
  BO β (Ur a)
{-# INLINE copyAt #-}
copyAt index vector = Control.do
  Ur !element <- move Control.<$> get index vector
  Control.pure $! Ur $! copy element

{- | Copy an element and retain the mutable vector borrow.

The raw read is only a temporary alias. 'copy' consumes it to produce the
authorized unrestricted result while the mutable vector remains exclusive.
-}
copyAtMut ::
  (HasCallStack, U.Unbox a, Copyable a, α >= β) =>
  Int ->
  Mut α (Vector a) %1 ->
  BO β (Ur a, Mut α (Vector a))
{-# INLINE copyAtMut #-}
copyAtMut =
  Unsafe.toLinear2 \index vector@(UnsafeAlias (Vector buffer)) ->
    let !length_ = UM.length buffer
     in if index < 0 || index >= length_
          then
            error
              ( "get: index "
                  <> show index
                  <> " out of bounds for length "
                  <> show length_
              )
              vector
          else unsafeSystemIOToBO do
            !value <- UM.unsafeRead buffer index
            let !copied = copy (UnsafeAlias value)
            NonLinear.pure (Ur copied, vector)

-- | Replace an element and return the displaced value linearly.
set ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Int ->
  a %1 ->
  Mut α (Vector a) %1 ->
  BO β (a, Mut α (Vector a))
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
            value
            array
      | otherwise -> unsafeSet index value array

{- | Unchecked 'set'. The index must satisfy @0 <= index < size@.

The exchange completes before the displaced value is returned.
-}
unsafeSet ::
  (U.Unbox a, α >= β) =>
  Int ->
  a %1 ->
  Mut α (Vector a) %1 ->
  BO β (a, Mut α (Vector a))
{-# INLINE unsafeSet #-}
unsafeSet =
  -- The value is evaluated with a bang, which GHC can turn into strictness of a caller, unlike the boxed vectors' writes: hiding the demand would make a caller that computes an unboxed element build a thunk for it, which tripled the allocation of an update loop.
  -- See Note [Stored contents are evaluated after noDuplicate#] in "Data.Ref.Linear.Unlifted.Internal".
  Unsafe.toLinear3 \index !value array@(UnsafeAlias (Vector vector)) ->
    unsafeSystemIOToBO do
      !oldValue <- UM.unsafeExchange vector index value
      NonLinear.pure (oldValue, array)

-- | Linearly transform an element and return an auxiliary result.
update ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Int ->
  (a %1 -> BO β (result, a)) %1 ->
  Mut α (Vector a) %1 ->
  BO β (result, Mut α (Vector a))
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
            action
            array
      | otherwise -> unsafeUpdate index action array

{- | Unchecked 'update'. The index must satisfy @0 <= index < size@.

The mutable vector borrow remains unavailable while the callback owns the raw
element. The callback must return exactly one replacement, which is written
strictly before the vector borrow is restored. This is a normal-return
guarantee; no exceptional owner recovery is claimed.
-}
unsafeUpdate ::
  (U.Unbox a, α >= β) =>
  Int ->
  (a %1 -> BO β (result, a)) %1 ->
  Mut α (Vector a) %1 ->
  BO β (result, Mut α (Vector a))
{-# INLINE unsafeUpdate #-}
unsafeUpdate index =
  Unsafe.toLinear2 \action (UnsafeAlias array@(Vector vector)) -> Control.do
    value <- unsafeSystemIOToBO (UM.unsafeRead vector index)
    (!result, !updatedValue) <- action value
    () <-
      unsafeSystemIOToBO
        (Unsafe.toLinear3 UM.unsafeWrite vector index updatedValue)
    Control.pure (result, UnsafeAlias array)

-- | Linearly transform an element.
modify ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Int ->
  (a %1 -> a) %1 ->
  Mut α (Vector a) %1 ->
  BO β (Mut α (Vector a))
{-# INLINE modify #-}
modify index function array = Control.do
  ((), array) <-
    update
      index
      (\value -> Control.pure ((), function value))
      array
  Control.pure array

-- | Swap two elements.
swap ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Mut α (Vector a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (Vector a))
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
  (U.Unbox a, α >= β) =>
  Mut α (Vector a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (Vector a))
{-# INLINE unsafeSwap #-}
unsafeSwap =
  Unsafe.toLinear3 \array@(UnsafeAlias (Vector vector)) first second ->
    unsafeSystemIOToBO do
      UM.unsafeSwap vector first second
      NonLinear.pure array

{- | Split a borrow into two disjoint fixed ranges without copying.

The index is clamped to @[0, size]@, matching @vector@'s 'UM.splitAt'.
-}
splitAt ::
  (U.Unbox a) =>
  Int %1 ->
  Borrow bk α (Vector a) %1 ->
  ( Borrow bk α (Vector a)
  , Borrow bk α (Vector a)
  )
{-# INLINE splitAt #-}
splitAt =
  Unsafe.toLinear2 \index (UnsafeAlias (Vector vector)) ->
    case UM.splitAt index vector of
      (left, right) ->
        (UnsafeAlias (Vector left), UnsafeAlias (Vector right))
