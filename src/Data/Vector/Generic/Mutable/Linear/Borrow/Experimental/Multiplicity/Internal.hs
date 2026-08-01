{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Vector.Generic.Mutable.Linear.Borrow.Experimental.Multiplicity.Internal (
  module Data.Vector.Generic.Mutable.Linear.Borrow.Experimental.Multiplicity.Internal,
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
import Data.Kind (Constraint, Type)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Mutable (RealWorld)
import GHC.Exts (Multiplicity (..))
import GHC.Exts qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.TypeError
import Prelude.Linear hiding (head, last, splitAt)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

-- | Linearly owned mutable vector with mode-indexed element ownership.
type Vector :: GHC.Multiplicity -> (Type -> Type) -> Type -> Type
newtype Vector p v a = Vector {content :: G.Mutable v RealWorld a}

-- | Linear mutable vector, owning the elements exclusively.
type OwningVector = Vector One

-- | Linear mutable vector, but the elements are bound unrestricted.
type UrVector = Vector Many

type role Vector nominal nominal nominal

-- | A linearly owned result for 'One', or a GC-owned result for 'Many'.
type Bound :: Multiplicity -> Type -> Type
type family Bound p a = result where
  Bound One a = a
  Bound Many a = Ur a

-- | 'Consumable' when the multiplicity is 'One', and always satisfied when the multiplicity is 'Many'.
type PossiblyConsumable :: Multiplicity -> Type -> Constraint
type family PossiblyConsumable p a where
  PossiblyConsumable One a = (Consumable a)
  PossiblyConsumable Many a = ()

-- | 'Movable' when the multiplicity is 'One', and always satisfied when the multiplicity is 'Many'.
type PossiblyMovable :: Multiplicity -> Type -> Constraint
type family PossiblyMovable p a where
  PossiblyMovable One a = (Movable a)
  PossiblyMovable Many a = ()

-- | 'Copyable' when the multiplicity is 'One', and always satisfied when the multiplicity is 'Many'.
type PossiblyCopyable :: Multiplicity -> Type -> Constraint
type family PossiblyCopyable p a where
  PossiblyCopyable One a = (Copyable a)
  PossiblyCopyable Many a = ()

-- | Result of reading an element while respecting the vector's element ownership.
type GetResult :: Multiplicity -> BorrowKind -> Lifetime -> (Type -> Type) -> Type -> Type
type family GetResult p bk α v a where
  GetResult One bk α v a = Borrow bk α a
  GetResult Many bk α v a =
    (Ur a, Borrow bk α (Vector Many v a))

-- | Mode-indexed element update callback.
type UpdateAction :: Multiplicity -> Lifetime -> Type -> Type -> Type
type UpdateAction p β result a =
  a %p -> BO β (Bound p result, Bound p a)

-- | Construct a fixed-size view over a raw mutable-vector slice.
unsafeFromMutableSlice ::
  (G.Vector v a) =>
  Int ->
  Int ->
  G.Mutable v RealWorld a %1 ->
  Vector p v a
{-# INLINE unsafeFromMutableSlice #-}
unsafeFromMutableSlice offset length_ =
  Unsafe.toLinear \buffer ->
    Vector (GM.unsafeSlice offset length_ buffer)

instance LinearOnly (Vector p v a) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance
  (Unsatisfiable (ShowType (Vector p v a) :<>: Text " cannot be copied!")) =>
  Copyable (Vector p v a)
  where
  copy = unsatisfiable

instance (G.Vector v a) => Consumable (Vector Many v a) where
  consume = Unsafe.toLinear \_ -> ()
  {-# INLINE consume #-}

instance (G.Vector v a, Consumable a) => Consumable (Vector One v a) where
  consume =
    Unsafe.toLinear \(Vector vector) ->
      unsafePerformIO (consumeElements 0 (GM.length vector) vector)
  -- Only the 'One' instance releases elements, and it does so under
  -- 'unsafePerformIO'. Inlining would let GHC duplicate that call across use
  -- sites, or float it out of a scope, and each copy would consume the
  -- elements again; 'NOINLINE' keeps exactly one occurrence. The 'Many'
  -- instance above owns nothing and stays 'INLINE'.
  {-# NOINLINE consume #-}

consumeElements ::
  (G.Vector v a, Consumable a) =>
  Int ->
  Int ->
  G.Mutable v RealWorld a ->
  IO ()
{-# INLINE consumeElements #-}
consumeElements !index !length_ vector
  | index >= length_ = NonLinear.pure ()
  | otherwise = do
      value <- GM.unsafeRead vector index
      let !() = consume value
      consumeElements (index + 1) length_ vector

instance (G.Vector v a) => Clone (Vector Many v a) where
  clone = Unsafe.toLinear \(UnsafeAlias (Vector vector)) ->
    Vector Control.<$> unsafeSystemIOToBO (GM.clone vector)
  {-# INLINE clone #-}

instance (G.Vector v a, Dupable a) => Clone (Vector One v a) where
  clone = Unsafe.toLinear \(UnsafeAlias (Vector source)) ->
    Vector Control.<$> unsafeSystemIOToBO do
      let !length_ = GM.length source
      target <- GM.unsafeNew length_
      let go !index
            | index >= length_ = NonLinear.pure ()
            | otherwise = do
                value <- GM.unsafeRead source index
                let (!_, !clonedValue) = dup value
                GM.unsafeWrite target index clonedValue
                go (index + 1)
      go 0
      NonLinear.pure target
  {-# INLINE clone #-}

-- | \(O(1)\). Construct an empty vector.
empty :: (G.Vector v a) => Linearly %1 -> Vector p v a
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
  Vector Many v a
{-# NOINLINE constant #-}
constant =
  GHC.noinline \count value linear ->
    linear `lseq` Vector (unsafePerformIO (GM.replicate count value))

-- | \(O(n)\). Materialize a list whose binding multiplicity matches the mode.
fromList ::
  (G.Vector v a) =>
  [a] %p ->
  Linearly %1 ->
  Vector p v a
{-# NOINLINE fromList #-}
fromList =
  GHC.noinline $ Unsafe.toLinear \values linear ->
    linear `lseq`
      Vector
        (unsafePerformIO (G.thaw (G.fromList values)))

-- | \(O(n)\). Copy an immutable vector into a new owner.
fromVector ::
  (G.Vector v a) =>
  v a ->
  Linearly %1 ->
  Vector Many v a
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
  Vector p v a
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
  Vector p v a
{-# INLINE unsafeFromMutable #-}
unsafeFromMutable =
  Unsafe.toLinear \source linear ->
    linear `lseq` Vector (Unsafe.coerce source)

type KnownMultiplicity :: Multiplicity -> Constraint

-- | Internal evidence selecting the ownership-sensitive implementation.
class KnownMultiplicity p where
  mMove :: a %1 -> Bound p a

  -- | \(O(1)\). Consume the owner and freeze its backing storage.
  toVector :: (G.Vector v a, PossiblyMovable p a) => Vector p v a %1 -> Ur (v a)

  mCopy :: forall a. (PossiblyCopyable p a) => a -> Bound p a

  mUnbind :: Bound p a %1 -> a

  copyVector ::
    (G.Vector v a, PossiblyCopyable p a) =>
    v a ->
    Ur (v a)

  mUnsafeWrite ::
    forall v a α β.
    (G.Vector v a, α >= β, PossiblyConsumable p a) =>
    Int ->
    a %p ->
    Mut α (Vector p v a) %1 ->
    BO β (Mut α (Vector p v a))

  mUnsafeGet ::
    forall bk α β v a.
    (G.Vector v a, α >= β) =>
    Int ->
    Borrow bk α (Vector p v a) %1 ->
    BO β (GetResult p bk α v a)

instance KnownMultiplicity Many where
  {-# SPECIALIZE instance KnownMultiplicity Many #-}
  mMove = Unsafe.toLinear Ur
  {-# INLINE mMove #-}

  toVector = Unsafe.toLinear \v -> Ur $ unsafeToVector v
  {-# INLINE toVector #-}

  mCopy = Unsafe.toLinear Ur
  {-# INLINE mCopy #-}

  mUnbind = \(Ur value) -> value
  {-# INLINE mUnbind #-}

  copyVector = Unsafe.toLinear Ur
  {-# INLINE copyVector #-}

  mUnsafeWrite =
    Unsafe.toLinear3 \index value array@(UnsafeAlias (Vector vector)) ->
      unsafeSystemIOToBO do
        GM.unsafeWrite vector index value
        NonLinear.pure array
  {-# INLINE mUnsafeWrite #-}

  mUnsafeGet index =
    Unsafe.toLinear \vectorBorrow@(UnsafeAlias (Vector vector)) ->
      unsafeSystemIOToBO do
        value <- GM.unsafeRead vector index
        NonLinear.pure (Ur value, vectorBorrow)
  {-# INLINE mUnsafeGet #-}

instance KnownMultiplicity One where
  {-# SPECIALIZE instance KnownMultiplicity One #-}
  mMove = id
  {-# INLINE mMove #-}
  toVector = Unsafe.toLinear \(v :: Vector One v a) ->
    G.mapM (Unsafe.toLinear move) (unsafeToVector v)
  {-# INLINE toVector #-}

  mCopy = \value -> copy (UnsafeAlias value)
  {-# INLINE mCopy #-}

  mUnbind = id
  {-# INLINE mUnbind #-}

  copyVector =
    G.mapM \value ->
      Ur $! copy (UnsafeAlias value)
  {-# INLINE copyVector #-}

  mUnsafeWrite =
    Unsafe.toLinear3 \index value array@(UnsafeAlias (Vector vector)) ->
      unsafeSystemIOToBO do
        oldValue <- GM.unsafeExchange vector index value
        let !() = consume oldValue
        NonLinear.pure array
  {-# INLINE mUnsafeWrite #-}

  mUnsafeGet index =
    Unsafe.toLinear \(UnsafeAlias (Vector vector)) ->
      UnsafeAlias Control.<$> unsafeSystemIOToBO (GM.unsafeRead vector index)
  {-# INLINE mUnsafeGet #-}

unsafeToVector ::
  (G.Vector v a) =>
  Vector p v a %1 ->
  v a
{-# NOINLINE unsafeToVector #-}
unsafeToVector =
  GHC.noinline $
    Unsafe.toLinear \(Vector vector) ->
      unsafePerformIO (G.unsafeFreeze vector)

unsafeToList :: (G.Vector v a) => Vector p v a %1 -> [a]
{-# NOINLINE unsafeToList #-}
unsafeToList = GHC.noinline $
  Unsafe.toLinear \(Vector vector) ->
    G.toList $! unsafePerformIO (G.unsafeFreeze vector)

-- | \(O(n)\). Consume the owner and materialize its elements as a list.
toList :: (KnownMultiplicity p, G.Vector v a) => Vector p v a %1 -> Bound p [a]
{-# INLINE toList #-}
{-# SPECIALIZE INLINE toList :: (G.Vector v a) => Vector Many v a %1 -> Ur [a] #-}
{-# SPECIALIZE INLINE toList :: (G.Vector v a) => Vector One v a %1 -> [a] #-}
toList (v :: Vector p v a) = mMove @p (unsafeToList v)

{- | \(O(n)\). Copy a live vector into an immutable vector and thread its
borrow.
-}
copyToVector ::
  ( G.Vector v a
  , α >= β
  , KnownMultiplicity p
  , PossiblyCopyable p a
  ) =>
  Borrow bk α (Vector p v a) %1 ->
  BO β (Ur (v a), Borrow bk α (Vector p v a))
{-# INLINE copyToVector #-}
copyToVector =
  Unsafe.toLinear \vectorBorrow@(UnsafeAlias (Vector vector) :: Borrow bk α (Vector p v a)) ->
    unsafeSystemIOToBO do
      snapshot <- G.freeze vector
      case copyVector @p snapshot of
        Ur !copied -> NonLinear.pure (Ur copied, vectorBorrow)

-- | \(O(1)\). Return the number of elements and thread the vector borrow.
size ::
  (G.Vector v a) =>
  Borrow bk α (Vector p v a) %1 ->
  (Ur Int, Borrow bk α (Vector p v a))
{-# INLINE size #-}
size =
  Unsafe.toLinear \vectorBorrow@(UnsafeAlias (Vector vector)) ->
    (Ur (GM.length vector), vectorBorrow)

-- | Read the element at an index and thread the vector borrow.
get ::
  (HasCallStack, G.Vector v a, α >= β, KnownMultiplicity p) =>
  Int ->
  Borrow bk α (Vector p v a) %1 ->
  BO β (GetResult p bk α v a)
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
  forall p v a α β bk.
  (G.Vector v a, α >= β, KnownMultiplicity p) =>
  Int ->
  Borrow bk α (Vector p v a) %1 ->
  BO β (GetResult p bk α v a)
{-# INLINE unsafeGet #-}
unsafeGet = mUnsafeGet @p

-- | Read the first element and thread the vector borrow.
head ::
  (HasCallStack, G.Vector v a, α >= β, KnownMultiplicity p) =>
  Borrow bk α (Vector p v a) %1 ->
  BO β (GetResult p bk α v a)
{-# INLINE head #-}
head = get 0

-- | Unchecked 'head'. The vector must be non-empty.
unsafeHead ::
  (G.Vector v a, α >= β, KnownMultiplicity p) =>
  Borrow bk α (Vector p v a) %1 ->
  BO β (GetResult p bk α v a)
{-# INLINE unsafeHead #-}
unsafeHead = unsafeGet 0

-- | Read the last element and thread the vector borrow.
last ::
  (HasCallStack, G.Vector v a, α >= β, KnownMultiplicity p) =>
  Borrow bk α (Vector p v a) %1 ->
  BO β (GetResult p bk α v a)
{-# INLINE last #-}
last vector =
  case size vector of
    (Ur length_, vector)
      | length_ <= 0 -> error "last: empty vector" vector
      | otherwise -> unsafeGet (length_ - 1) vector

-- | Unchecked 'last'. The vector must be non-empty.
unsafeLast ::
  (G.Vector v a, α >= β, KnownMultiplicity p) =>
  Borrow bk α (Vector p v a) %1 ->
  BO β (GetResult p bk α v a)
{-# INLINE unsafeLast #-}
unsafeLast vector =
  case size vector of
    (Ur length_, vector) -> unsafeGet (length_ - 1) vector

-- | Read an element through a shared borrow.
copyAt ::
  ( HasCallStack
  , G.Vector v a
  , α >= β
  , KnownMultiplicity p
  , PossiblyCopyable p a
  ) =>
  Int ->
  Share α (Vector p v a) ->
  BO β (Bound p a)
{-# INLINE copyAt #-}
copyAt =
  Unsafe.toLinear2 \index vector@(UnsafeAlias (Vector buffer) :: Share α (Vector p v a)) ->
    let !length_ = GM.length buffer
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
            !value <- GM.unsafeRead buffer index
            let !copied = mCopy @p value
            NonLinear.pure copied

-- | Read an element and retain the mutable vector borrow.
copyAtMut ::
  ( HasCallStack
  , G.Vector v a
  , α >= β
  , KnownMultiplicity p
  , PossiblyCopyable p a
  ) =>
  Int ->
  Mut α (Vector p v a) %1 ->
  BO β (Bound p a, Mut α (Vector p v a))
{-# INLINE copyAtMut #-}
copyAtMut = Unsafe.toLinear2 \i mut@(UnsafeAlias (Vector v) :: Mut α (Vector p v a)) ->
  let !len = GM.length v
   in if i < 0 || i >= len
        then error ("get: index " <> show i <> " out of bound: " <> show len) mut
        else unsafeSystemIOToBO do
          !a <- GM.unsafeRead v i
          let !copied = mCopy @p a
          NonLinear.pure (copied, mut)

-- | Replace an element and return the displaced value.
set ::
  (HasCallStack, G.Vector v a, α >= β, KnownMultiplicity p) =>
  Int ->
  a %p ->
  Mut α (Vector p v a) %1 ->
  BO β (Bound p a, Mut α (Vector p v a))
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
            value
      | otherwise -> unsafeSet index value array

-- | Unchecked 'set'. The index must satisfy @0 <= index < size@.
unsafeSet ::
  forall p v a α β.
  (G.Vector v a, α >= β, KnownMultiplicity p) =>
  Int ->
  a %p ->
  Mut α (Vector p v a) %1 ->
  BO β (Bound p a, Mut α (Vector p v a))
{-# INLINE unsafeSet #-}
unsafeSet =
  Unsafe.toLinear3 \index !value array@(UnsafeAlias (Vector vector) :: Mut α (Vector p v a)) ->
    unsafeSystemIOToBO do
      oldValue <- GM.unsafeExchange vector index value
      NonLinear.pure (mMove @p oldValue, array)

-- | Replace an element and consume the displaced value.
write ::
  ( HasCallStack
  , G.Vector v a
  , α >= β
  , KnownMultiplicity p
  , PossiblyConsumable p a
  ) =>
  Int ->
  a %p ->
  Mut α (Vector p v a) %1 ->
  BO β (Mut α (Vector p v a))
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
            value
      | otherwise -> unsafeWrite index value array

-- | Unchecked 'write'. The index must satisfy @0 <= index < size@.
unsafeWrite ::
  forall p v a α β.
  ( G.Vector v a
  , α >= β
  , KnownMultiplicity p
  , PossiblyConsumable p a
  ) =>
  Int ->
  a %p ->
  Mut α (Vector p v a) %1 ->
  BO β (Mut α (Vector p v a))
{-# INLINE unsafeWrite #-}
unsafeWrite = mUnsafeWrite @p

{- | Transform an element and return an auxiliary result.

For an owning vector, the callback and element are linear. For an unrestricted
vector, both are unrestricted and the callback returns 'Ur'-wrapped results.
The mutable vector borrow is unavailable until the replacement has been
written. This is a normal-return guarantee; no owner recovery is promised
after an exception.
-}
update ::
  forall p v a α β result.
  (HasCallStack, G.Vector v a, α >= β, KnownMultiplicity p) =>
  Int ->
  UpdateAction p β result a %p ->
  Mut α (Vector p v a) %1 ->
  BO β (Bound p result, Mut α (Vector p v a))
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
            action
      | otherwise ->
          unsafeUpdate @p @v @a @α @β @result index action array

{- | Unchecked 'update'. The index must satisfy @0 <= index < size@.

The ownership guarantees are the same as for 'update'.
-}
unsafeUpdate ::
  forall p v a α β result.
  (G.Vector v a, α >= β, KnownMultiplicity p) =>
  Int ->
  UpdateAction p β result a %p ->
  Mut α (Vector p v a) %1 ->
  BO β (Bound p result, Mut α (Vector p v a))
{-# INLINE unsafeUpdate #-}
unsafeUpdate index action =
  Unsafe.toLinear \(UnsafeAlias array@(Vector vector)) -> Control.do
    value <- unsafeSystemIOToBO (GM.unsafeRead vector index)
    (result, updatedValue) <- Unsafe.toLinear action value
    let !unboundValue = mUnbind @p updatedValue
    () <-
      unsafeSystemIOToBO
        (Unsafe.toLinear3 GM.unsafeWrite vector index unboundValue)
    Control.pure (result, UnsafeAlias array)

-- | Transform an element.
modify ::
  forall p v a α β.
  (HasCallStack, G.Vector v a, α >= β, KnownMultiplicity p) =>
  Int ->
  (a %p -> a) %p ->
  Mut α (Vector p v a) %1 ->
  BO β (Mut α (Vector p v a))
{-# INLINE modify #-}
modify index function array = Control.do
  (unit, array) <-
    update @p @v @a @α @β @()
      index
      ( Unsafe.toLinear \value ->
          Control.pure
            ( mMove @p ()
            , mMove @p (Unsafe.toLinear function value)
            )
      )
      array
  case mUnbind @p unit of
    () -> Control.pure array

-- | Swap two elements.
swap ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Mut α (Vector p v a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (Vector p v a))
{-# INLINE swap #-}
swap array first second =
  case size array of
    (Ur length_, array)
      | first < 0 || first >= length_ || second < 0 || second >= length_ ->
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
  Mut α (Vector p v a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (Vector p v a))
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
  Borrow bk α (Vector p v a) %1 ->
  ( Borrow bk α (Vector p v a)
  , Borrow bk α (Vector p v a)
  )
{-# INLINE splitAt #-}
splitAt index =
  Unsafe.toLinear \(UnsafeAlias (Vector vector)) ->
    case GM.splitAt index vector of
      (left, right) ->
        (UnsafeAlias (Vector left), UnsafeAlias (Vector right))
