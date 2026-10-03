{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Vector.Mutable.Growable.Linear.Borrow.Internal (
  module Data.Vector.Mutable.Growable.Linear.Borrow.Internal,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Internal (unsafeSrunBO_)
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
import Data.IntSet qualified as IntSet
import Data.Ref.Linear.Internal qualified as Ref
import Data.Unrestricted.Linear qualified as Ur
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable.Linear.Borrow qualified as Fixed
import Data.Vector.Mutable.Linear.Borrow.Internal qualified as Fixed.Internal
import GHC.Exts qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.TypeError
import Prelude.Linear hiding (getContents, head, last)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

data Header a where
  Header ::
    {-# UNPACK #-} !Int ->
    !(MV.IOVector a) %1 ->
    Header a

-- | A linearly owned boxed vector with a stable header and replaceable backing allocation.
data GrowableVector a where
  GrowableVector :: !(Ref.Ref (Header a)) %1 -> GrowableVector a

type role Header nominal

type role GrowableVector nominal

instance LinearOnly (GrowableVector a) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance
  (Unsatisfiable (ShowType (GrowableVector a) :<>: Text " cannot be copied!")) =>
  Copyable (GrowableVector a)
  where
  copy = unsatisfiable

instance (Consumable a) => Consumable (GrowableVector a) where
  consume =
    Unsafe.toLinear \(GrowableVector ref) ->
      case Ref.free ref of
        Header logicalSize buffer -> consumeInitialized logicalSize buffer
  -- Reaches 'unsafePerformIO' through 'consumeInitialized'. See the note
  -- there: this must not be duplicated across call sites.
  {-# NOINLINE consume #-}

allocateBuffer :: Int -> Linearly %1 -> MV.IOVector a
{-# NOINLINE allocateBuffer #-}
allocateBuffer =
  GHC.noinline \count linear ->
    linear `lseq` unsafePerformIO (MV.unsafeNew count)

cloneBuffer :: V.Vector a -> Linearly %1 -> MV.IOVector a
{-# NOINLINE cloneBuffer #-}
cloneBuffer =
  GHC.noinline \source linear ->
    linear `lseq` unsafePerformIO (V.thaw source)

-- | \(O(1)\). Construct an empty vector with zero capacity.
empty :: Linearly %1 -> GrowableVector a
{-# NOINLINE empty #-}
empty = withCapacity 0

-- | \(O(n)\). Construct @n@ initialized elements. The count must be non-negative.
constant ::
  Int ->
  a ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE constant #-}
constant = GHC.noinline \count value linear ->
  fromVector (V.replicate count value) linear

-- | \(O(n)\). Construct a vector from a list.
fromList ::
  [a] ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE fromList #-}
fromList = GHC.noinline \values linear ->
  fromVector (V.fromList values) linear

{- | \(O(1)\). Construct an empty vector with the requested capacity.

The capacity must be non-negative. No element in the spare allocation is
considered initialized.
-}
withCapacity :: (HasCallStack) => Int -> Linearly %1 -> GrowableVector a
{-# NOINLINE withCapacity #-}
withCapacity = GHC.noinline \requested linear ->
  if requested < 0
    then error ("withCapacity: negative capacity " <> show requested) linear
    else
      dup linear & \(bufferLinear, refLinear) ->
        GrowableVector
          (Ref.new (Header 0 (allocateBuffer requested bufferLinear)) refLinear)

-- | \(O(n)\). Copy all elements of an immutable boxed vector.
fromVector ::
  V.Vector a ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE fromVector #-}
fromVector = GHC.noinline \source linear ->
  dup linear & \(bufferLinear, refLinear) ->
    GrowableVector
      ( Ref.new
          (Header (V.length source) (cloneBuffer source bufferLinear))
          refLinear
      )

{- | \(O(1)\). Take ownership of a boxed mutable vector without copying.

The complete source is treated as initialized. The caller must not retain any
alias that can access the source allocation.
-}
unsafeFromMutable ::
  MV.MVector state a %1 ->
  Linearly %1 ->
  GrowableVector a
{-# INLINE unsafeFromMutable #-}
unsafeFromMutable =
  Unsafe.toLinear \source linear ->
    GrowableVector
      ( Ref.new
          (Header (MV.length source) (Unsafe.coerce source))
          linear
      )

{- | \(O(1)\). Unsafely take ownership of an immutable boxed vector's storage.

The complete source is treated as initialized. No immutable alias may be read
after this operation, because subsequent growable mutation reuses its storage.
-}
unsafeFromVector ::
  V.Vector a %1 ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE unsafeFromVector #-}
unsafeFromVector =
  GHC.noinline $
    Unsafe.toLinear \source linear ->
      GrowableVector
        ( Ref.new
            ( Header
                (V.length source)
                (unsafePerformIO (V.unsafeThaw source))
            )
            linear
        )

{- | \(O(n)\). Move every initialized element into GC ownership, then freeze
exactly that prefix.

Spare capacity is neither exposed nor materialized.
-}
toVector ::
  (Movable a) =>
  GrowableVector a %1 ->
  Ur (V.Vector a)
{-# NOINLINE toVector #-}
toVector =
  GHC.noinline $
    Unsafe.toLinear \(GrowableVector ref) ->
      case Ref.free ref of
        Header logicalSize buffer ->
          let !frozen =
                unsafePerformIO do
                  moveInitialized logicalSize buffer
                  V.unsafeFreeze (MV.unsafeTake logicalSize buffer)
           in Ur frozen

-- | \(O(n)\). Consume the owner and materialize its initialized prefix as a list.
toList ::
  (Movable a) =>
  GrowableVector a %1 ->
  Ur [a]
{-# INLINE toList #-}
toList = Ur.lift V.toList . toVector

moveInitialized ::
  (Movable a) =>
  Int ->
  MV.IOVector a ->
  NonLinear.IO ()
{-# INLINE moveInitialized #-}
moveInitialized !logicalSize buffer = go 0
  where
    go !index
      | index >= logicalSize = NonLinear.pure ()
      | otherwise = do
          value <- MV.unsafeRead buffer index
          case move value of
            Ur !moved -> MV.unsafeWrite buffer index moved
          go (index + 1)

{- | Consume the initialized prefix, releasing each element exactly once.

The traversal only reads the buffer, but it runs under 'unsafePerformIO'.
That makes the binding a trusted boundary rather than an ordinary pure
function: were it inlined, GHC could duplicate the call across use sites, or
float it out of a scope, and each copy would consume the elements again. The
'NOINLINE' keeps exactly one occurrence, so the exactly-once discipline the
linear types promise is preserved in the generated code too.
-}
consumeInitialized ::
  (Consumable a) =>
  Int ->
  MV.IOVector a %1 ->
  ()
{-# NOINLINE consumeInitialized #-}
consumeInitialized =
  Unsafe.toLinear2 \logicalSize buffer ->
    let go !index
          | index >= logicalSize = NonLinear.pure ()
          | otherwise = do
              value <- MV.unsafeRead buffer index
              let !() = consume value
              go (index + 1)
     in unsafePerformIO (go 0)

{-
Note [Growable header reads]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every read of the header goes through 'readHeader', which reads the header 'Ref.Ref' inside 'BO', so the read is ordered by the state token after every earlier effect of the computation.
A pure read of the 'Ref.Ref' is a function of the reference alone, so GHC may share one evaluation between two reads (CSE), float it, or evaluate it after later writes.
Through a 'Share' used past the end of its lifetime, that returned stale sizes and contents.
Reading inside @'BO' β@ with @α >= β@ also confines every read to a live lifetime: 'pureAfter' and 'After' offer no 'BO' to run the read in.

'readHeader' duplicates the buffer handle, so its result is only a view.
Callers treat the buffer as a borrow of the kind and lifetime of the growable borrow they read it through, and keep threading that borrow unchanged.
An element operation reads the header once and uses the snapshot for both its bounds check and its access; nothing can change the header in between, since the caller holds the borrow.
'withHeader' reads and writes the header inside 'BO' as well, so a later read is ordered after the write by the state token rather than by evaluation order.

Consuming a whole owner ('consume', 'toVector') still reads the header with the pure 'Ref.free': the owner is then gone, so no later read of the same reference exists to go stale, and nothing else reads the header reference purely, so no earlier read exists to stand in for it either.
An owner reclaimed after a scope is handed back through the barrier of Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal", so that free is not the same expression as anything before the scope.
-}

-- | Read the header inside 'BO'. See Note [Growable header reads].
readHeader :: GrowableVector a -> BO β (Ur (Int, MV.IOVector a))
{-# INLINE readHeader #-}
readHeader (GrowableVector ref) =
  (Unsafe.toLinear \(Header logicalSize buffer) -> Ur (logicalSize, buffer))
    Control.<$> Ref.unsafeReadRefBO ref

-- | Read, transform and write back the header inside 'BO'. See Note [Growable header reads].
withHeader ::
  (α >= β) =>
  (Header a %1 -> BO β (result, Header a)) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (result, Mut α (GrowableVector a))
{-# INLINE withHeader #-}
withHeader =
  Unsafe.toLinear2 \action vector@(UnsafeAlias (GrowableVector ref)) -> Control.do
    header <- Ref.unsafeReadRefBO ref
    (result, header) <- action header
    () <- Ref.unsafeWriteRefBO ref header
    Control.pure (result, vector)

-- | \(O(1)\). Return the number of initialized elements and thread the borrow.
size ::
  (α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Ur Int, Borrow bk α (GrowableVector a))
{-# INLINE size #-}
size =
  Unsafe.toLinear \vector@(UnsafeAlias growable) -> Control.do
    Ur (logicalSize, _) <- readHeader growable
    Control.pure (Ur logicalSize, vector)

-- | \(O(1)\). Return the backing allocation size and thread the borrow.
capacity ::
  (α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Ur Int, Borrow bk α (GrowableVector a))
{-# INLINE capacity #-}
capacity =
  Unsafe.toLinear \vector@(UnsafeAlias growable) -> Control.do
    Ur (_, buffer) <- readHeader growable
    Control.pure (Ur (MV.length buffer), vector)

{- | Borrow the element at an index in the initialized prefix.

This consumes the growable borrow. The growable owner can be recovered only
through its enclosing lender after the returned element borrow ends. Use
'withContent' for repeated no-growth access.
-}
get ::
  (HasCallStack, α >= β) =>
  Int ->
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE get #-}
get index =
  Unsafe.toLinear \(UnsafeAlias growable) -> Control.do
    Ur (logicalSize, buffer) <- readHeader growable
    if index < 0 || index >= logicalSize
      then
        error
          ( "get: index "
              <> show index
              <> " out of bounds for length "
              <> show logicalSize
          )
      else
        UnsafeAlias
          Control.<$> unsafeSystemIOToBO (MV.unsafeRead buffer index)

-- | Unchecked 'get'. The index must satisfy @0 <= index < size@.
unsafeGet ::
  (α >= β) =>
  Int ->
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeGet #-}
unsafeGet index =
  Unsafe.toLinear \(UnsafeAlias growable) -> Control.do
    Ur (_, buffer) <- readHeader growable
    UnsafeAlias
      Control.<$> unsafeSystemIOToBO (MV.unsafeRead buffer index)

-- | Borrow the first initialized element. Fails when the vector is empty.
head ::
  (HasCallStack, α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE head #-}
head = get 0

-- | Unchecked 'head'. The vector must be non-empty.
unsafeHead ::
  (α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeHead #-}
unsafeHead = unsafeGet 0

-- | Borrow the last initialized element. Fails when the vector is empty.
last ::
  (HasCallStack, α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE last #-}
last =
  Unsafe.toLinear \(UnsafeAlias growable) -> Control.do
    Ur (logicalSize, buffer) <- readHeader growable
    if logicalSize <= 0
      then error "last: empty vector"
      else
        UnsafeAlias
          Control.<$> unsafeSystemIOToBO (MV.unsafeRead buffer (logicalSize - 1))

-- | Unchecked 'last'. The vector must be non-empty.
unsafeLast ::
  (α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeLast #-}
unsafeLast =
  Unsafe.toLinear \(UnsafeAlias growable) -> Control.do
    Ur (logicalSize, buffer) <- readHeader growable
    UnsafeAlias
      Control.<$> unsafeSystemIOToBO (MV.unsafeRead buffer (logicalSize - 1))

-- | Copy the element at an index through a shared borrow.
copyAt ::
  (HasCallStack, Copyable a, α >= β) =>
  Int ->
  Share α (GrowableVector a) ->
  BO β (Ur a)
{-# INLINE copyAt #-}
copyAt index (UnsafeAlias growable) = Control.do
  Ur (logicalSize, buffer) <- readHeader growable
  if index < 0 || index >= logicalSize
    then
      error
        ( "copyAt: index "
            <> show index
            <> " out of bounds for length "
            <> show logicalSize
        )
    else copyElement buffer index

-- | Unchecked 'copyAt'. The index must satisfy @0 <= index < size@.
unsafeCopyAt ::
  (Copyable a, α >= β) =>
  Int ->
  Share α (GrowableVector a) ->
  BO β (Ur a)
{-# INLINE unsafeCopyAt #-}
unsafeCopyAt index (UnsafeAlias growable) = Control.do
  Ur (_, buffer) <- readHeader growable
  copyElement buffer index

-- | Copy an element out of a buffer view obtained from 'readHeader'.
copyElement :: (Copyable a) => MV.IOVector a -> Int -> BO β (Ur a)
{-# INLINE copyElement #-}
copyElement buffer index = unsafeSystemIOToBO do
  !value <- MV.unsafeRead buffer index
  let !copied = copy (UnsafeAlias value)
  NonLinear.pure (Ur copied)

-- | Copy the element at an index and return the mutable growable borrow.
copyAtMut ::
  (HasCallStack, Copyable a, α >= β) =>
  Int ->
  Mut α (GrowableVector a) %1 ->
  BO β (Ur a, Mut α (GrowableVector a))
{-# INLINE copyAtMut #-}
copyAtMut index =
  Unsafe.toLinear \vector@(UnsafeAlias growable) -> Control.do
    Ur (logicalSize, buffer) <- readHeader growable
    if index < 0 || index >= logicalSize
      then
        error
          ( "copyAtMut: index "
              <> show index
              <> " out of bounds for length "
              <> show logicalSize
          )
      else (\(Ur copied) -> (Ur copied, vector)) Control.<$> copyElement buffer index

-- | Unchecked 'copyAtMut'. The index must satisfy @0 <= index < size@.
unsafeCopyAtMut ::
  (Copyable a, α >= β) =>
  Int ->
  Mut α (GrowableVector a) %1 ->
  BO β (Ur a, Mut α (GrowableVector a))
{-# INLINE unsafeCopyAtMut #-}
unsafeCopyAtMut index =
  Unsafe.toLinear \vector@(UnsafeAlias growable) -> Control.do
    Ur (_, buffer) <- readHeader growable
    (\(Ur copied) -> (Ur copied, vector)) Control.<$> copyElement buffer index

-- | Replace an initialized element and return the displaced value.
set ::
  (HasCallStack, α >= β) =>
  Int ->
  a %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (a, Mut α (GrowableVector a))
{-# INLINE set #-}
set index =
  Unsafe.toLinear2 \ !value vector@(UnsafeAlias growable) -> Control.do
    Ur (logicalSize, buffer) <- readHeader growable
    if index < 0 || index >= logicalSize
      then
        error
          ( "set: index "
              <> show index
              <> " out of bounds for length "
              <> show logicalSize
          )
      else unsafeSystemIOToBO do
        !oldValue <- MV.unsafeRead buffer index
        MV.unsafeWrite buffer index value
        NonLinear.pure (oldValue, vector)

-- | Unchecked 'set'. The index must satisfy @0 <= index < size@.
unsafeSet ::
  (α >= β) =>
  Int ->
  a %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (a, Mut α (GrowableVector a))
{-# INLINE unsafeSet #-}
unsafeSet index =
  Unsafe.toLinear2 \ !value vector@(UnsafeAlias growable) -> Control.do
    Ur (_, buffer) <- readHeader growable
    unsafeSystemIOToBO do
      !oldValue <- MV.unsafeRead buffer index
      MV.unsafeWrite buffer index value
      NonLinear.pure (oldValue, vector)

-- | Linearly transform an initialized element and return an auxiliary result.
update ::
  (HasCallStack, α >= β) =>
  Int ->
  (a %1 -> BO β (result, a)) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (result, Mut α (GrowableVector a))
{-# INLINE update #-}
update index =
  Unsafe.toLinear2 \action vector@(UnsafeAlias growable) -> Control.do
    Ur (logicalSize, buffer) <- readHeader growable
    if index < 0 || index >= logicalSize
      then
        error
          ( "update: index "
              <> show index
              <> " out of bounds for length "
              <> show logicalSize
          )
      else updateElement buffer index action vector

-- | Unchecked 'update'. The index must satisfy @0 <= index < size@.
unsafeUpdate ::
  (α >= β) =>
  Int ->
  (a %1 -> BO β (result, a)) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (result, Mut α (GrowableVector a))
{-# INLINE unsafeUpdate #-}
unsafeUpdate index =
  Unsafe.toLinear2 \action vector@(UnsafeAlias growable) -> Control.do
    Ur (_, buffer) <- readHeader growable
    updateElement buffer index action vector

-- | Update an element of a buffer view obtained from 'readHeader', then hand back the growable borrow.
updateElement ::
  MV.IOVector a ->
  Int ->
  (a %1 -> BO β (result, a)) ->
  Mut α (GrowableVector a) ->
  BO β (result, Mut α (GrowableVector a))
{-# INLINE updateElement #-}
updateElement buffer index action vector = Control.do
  value <- unsafeSystemIOToBO (MV.unsafeRead buffer index)
  (!result, !updatedValue) <- action value
  () <- writeElement buffer index updatedValue
  Control.pure (result, vector)

-- | Write an element into a buffer view obtained from 'readHeader'.
writeElement :: MV.IOVector a -> Int -> a %1 -> BO β ()
{-# INLINE writeElement #-}
writeElement buffer index =
  Unsafe.toLinear \value -> unsafeSystemIOToBO (MV.unsafeWrite buffer index value)

-- | Linearly transform an initialized element.
modify ::
  (HasCallStack, α >= β) =>
  Int ->
  (a %1 -> a) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE modify #-}
modify index function vector = Control.do
  ((), vector) <-
    update
      index
      (Control.pure . ((),) . function)
      vector
  Control.pure vector

-- | Unchecked 'swap'. Both indices must satisfy @0 <= index < size@.
unsafeSwap ::
  (α >= β) =>
  Mut α (GrowableVector a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (GrowableVector a))
{-# INLINE unsafeSwap #-}
unsafeSwap =
  Unsafe.toLinear3 \vector@(UnsafeAlias growable) first second -> Control.do
    Ur (_, buffer) <- readHeader growable
    unsafeSystemIOToBO do
      MV.unsafeSwap buffer first second
      NonLinear.pure vector

-- | Swap two initialized elements.
swap ::
  (HasCallStack, α >= β) =>
  Mut α (GrowableVector a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (GrowableVector a))
{-# INLINE swap #-}
swap =
  Unsafe.toLinear3 \vector@(UnsafeAlias growable) first second -> Control.do
    Ur (logicalSize, buffer) <- readHeader growable
    if first < 0 || first >= logicalSize || second < 0 || second >= logicalSize
      then
        error
          ( "swap: indices "
              <> show (first, second)
              <> " out of bounds for length "
              <> show logicalSize
          )
      else unsafeSystemIOToBO do
        MV.unsafeSwap buffer first second
        NonLinear.pure vector

{- | Borrow several initialized elements mutably without validation.

Every index must satisfy @0 <= index < size@, and the indices must be
pairwise distinct. Violating distinctness can create aliased mutable borrows
and a data race when they are used in parallel.
-}
unsafeIndicesMut ::
  (α >= β) =>
  Mut α (GrowableVector a) %1 ->
  [Int] %1 ->
  BO β [Mut α a]
{-# INLINE unsafeIndicesMut #-}
unsafeIndicesMut vector indices = Control.do
  contents <- getContents vector
  Fixed.unsafeIndicesMut contents indices

{- | Borrow several initialized elements mutably.

Fails if any index is out of bounds or if an index occurs more than once.
-}
indicesMut ::
  (HasCallStack, α >= β) =>
  Mut α (GrowableVector a) %1 ->
  [Int] %1 ->
  BO β [Mut α a]
{-# INLINE indicesMut #-}
indicesMut vector indices = Control.do
  (Ur logicalSize, vector) <- size vector
  case move indices of
    Ur indices
      | NonLinear.any (\index -> index < 0 || index >= logicalSize) indices ->
          error
            ( "indicesMut: indices out of bounds: "
                <> show indices
                <> " for length "
                <> show logicalSize
            )
            vector
      | NonLinear.length indices
          > IntSet.size (IntSet.fromList indices) ->
          error ("indicesMut: duplicate indices: " <> show indices) vector
      | otherwise -> unsafeIndicesMut vector indices

{- | Ensure that the absolute capacity is at least the requested value.

The requested capacity must be non-negative. Logical size and initialized
contents do not change. Reallocation destructively transfers the initialized
prefix into fresh storage.
-}
reserve ::
  (HasCallStack, α >= β) =>
  Int ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE reserve #-}
reserve requested vector
  | requested < 0 =
      error ("reserve: negative capacity " <> show requested) vector
  | otherwise = Control.do
      ((), vector) <-
        withHeader
          ( Unsafe.toLinear \(Header logicalSize buffer) -> Control.do
              grown <- growTo logicalSize requested buffer
              Control.pure ((), Header logicalSize grown)
          )
          vector
      Control.pure vector

{- | Ensure capacity for at least the current size plus the requested amount.

The additional amount must be non-negative. Logical size and initialized
contents do not change.
-}
reserveAdditional ::
  (HasCallStack, α >= β) =>
  Int ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE reserveAdditional #-}
reserveAdditional additional vector
  | additional < 0 =
      error
        ("reserveAdditional: negative additional capacity " <> show additional)
        vector
  | otherwise = Control.do
      ((), vector) <-
        withHeader
          ( Unsafe.toLinear \(Header logicalSize buffer) ->
              let !required =
                    checkedAdd "reserveAdditional" logicalSize additional
               in Control.do
                    grown <- growTo logicalSize required buffer
                    Control.pure ((), Header logicalSize grown)
          )
          vector
      Control.pure vector

{- | Append one linearly supplied element to the initialized prefix.

Reallocation, when required, destructively transfers the old initialized
prefix into fresh storage.
-}
push ::
  (HasCallStack, α >= β) =>
  a %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE push #-}
push =
  Unsafe.toLinear2 \ !value vector -> Control.do
    ((), vector) <-
      withHeader
        ( Unsafe.toLinear \(Header logicalSize buffer) ->
            let !required = checkedAdd "push" logicalSize 1
                !target = growthTarget (MV.length buffer) required
             in Control.do
                  grown <- growTo logicalSize target buffer
                  grown <- writeAt logicalSize value grown
                  Control.pure ((), Header required grown)
        )
        vector
    Control.pure vector

-- | Append copies of all elements of an immutable boxed vector.
extend ::
  (HasCallStack, α >= β) =>
  V.Vector a ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE extend #-}
extend source vector = Control.do
  ((), vector) <-
    withHeader
      ( Unsafe.toLinear \(Header logicalSize buffer) ->
          let !sourceSize = V.length source
              !required = checkedAdd "extend" logicalSize sourceSize
              !target = growthTarget (MV.length buffer) required
           in Control.do
                grown <- growTo logicalSize target buffer
                grown <- copyImmutableInto source logicalSize grown
                Control.pure ((), Header required grown)
      )
      vector
  Control.pure vector

copyImmutable :: V.Vector a -> Int -> MV.IOVector a -> NonLinear.IO ()
{-# INLINE copyImmutable #-}
copyImmutable source offset target =
  V.copy (MV.unsafeSlice offset (V.length source) target) source

copyImmutableInto ::
  V.Vector a ->
  Int ->
  MV.IOVector a %1 ->
  BO β (MV.IOVector a)
{-# INLINE copyImmutableInto #-}
copyImmutableInto source offset =
  Unsafe.toLinear \target -> unsafeSystemIOToBO do
    copyImmutable source offset target
    NonLinear.pure target

writeAt ::
  Int ->
  a %1 ->
  MV.IOVector a %1 ->
  BO β (MV.IOVector a)
{-# INLINE writeAt #-}
writeAt =
  Unsafe.toLinear3 \index value target -> unsafeSystemIOToBO do
    MV.unsafeWrite target index value
    NonLinear.pure target

growTo ::
  Int ->
  Int ->
  MV.IOVector a %1 ->
  BO β (MV.IOVector a)
{-# INLINE growTo #-}
growTo =
  Unsafe.toLinear3 \logicalSize requested buffer ->
    let !oldCapacity = MV.length buffer
     in if requested <= oldCapacity
          then Control.pure buffer
          else unsafeSystemIOToBO do
            grown <- MV.unsafeNew requested
            MV.unsafeCopy
              (MV.unsafeTake logicalSize grown)
              (MV.unsafeTake logicalSize buffer)
            NonLinear.pure grown

growthTarget :: Int -> Int -> Int
{-# INLINE growthTarget #-}
growthTarget oldCapacity required
  | required <= oldCapacity = oldCapacity
  | oldCapacity <= 0 = required `max` 1
  | oldCapacity > maxBound `quot` 2 = required
  | otherwise = required `max` (oldCapacity * 2)

checkedAdd :: (HasCallStack) => NonLinear.String -> Int -> Int -> Int
{-# INLINE checkedAdd #-}
checkedAdd operation left right
  | right > maxBound - left =
      error (operation <> ": capacity overflow")
  | otherwise = left + right

{- | Project a growable borrow to a fixed borrow of its initialized prefix.

This consumes one occurrence of the growable borrow, preserves its borrow kind and lifetime, and performs one header read inside 'BO', so the prefix it sees is the one current at this point of the computation.
The result exposes neither spare capacity nor growth.
A mutable result may be split using the fixed-vector API; the mutable growable owner becomes recoverable only after every resulting fixed borrow has ended.
A shared result is bound linearly, like the result of any 'BO' action, while readers such as 'Data.Vector.Mutable.Linear.Borrow.copyAt' take a 'Share' unrestricted, so 'move' it before its first read: @Ur content \<- move Control.\<$\> getContents shared@.

Where a transaction branches, prefer projecting once at its entry -- @content <- 'getContents' borrow@ -- over projecting separately inside each branch.
Both are correct and consume the growable occurrence exactly once; the entry form simply performs one header read rather than one per branch.
-}
getContents ::
  (α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α (Fixed.Vector a))
{-# INLINE getContents #-}
getContents =
  Unsafe.toLinear \(UnsafeAlias growable) -> Control.do
    Ur (logicalSize, buffer) <- readHeader growable
    Control.pure $! UnsafeAlias (Fixed.Internal.unsafeFromMutableSlice 0 logicalSize buffer)

{-
Note [Uniformly linear content callback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ideally the callback arrow would use @BorrowMultiplicity bk@, making a
shared callback unrestricted. GHC 9.12 rejects that signature because type
families cannot witness multiplicity equality (GHC #19517). Keep one linear
callback occurrence for both borrow kinds until that limitation is removed;
shared callers can use 'move' to recover unrestricted use.
-}

{- | Borrow the fixed initialized prefix in a rank-2 no-growth scope.

The callback and returned growable borrow preserve the input borrow kind. The
callback receives one linear occurrence for either kind; use 'move' on shared
content when unrestricted use is desired. For a mutable input, the growable
borrow is restored only after the callback result is produced and the fixed
view has ended.

See Note [Uniformly linear content callback] for why the callback stays linear
for a shared borrow too.
-}
withContent ::
  Borrow bk α (GrowableVector a) %1 ->
  ( forall β.
    Borrow bk (β /\ α) (Fixed.Vector a) %1 ->
    BO (β /\ α) result
  ) %1 ->
  BO α (result, Borrow bk α (GrowableVector a))
{-# INLINE withContent #-}
withContent =
  Unsafe.toLinear2 \vector action ->
    -- The growable borrow is handed back through `reviveAlias`, as the scalar delimiters do: see Note [Restoring a borrow must break its Core identity] in "Control.Monad.Borrow.Pure.BO.Internal".
    unsafeSrunBO_ Control.do
      contents <- getContents (Unsafe.coerce vector)
      result <- action contents
      (result,) Control.<$> reviveAlias vector

-- | A result-discarding variant of 'withContent'.
withContent_ ::
  (Consumable result) =>
  Borrow bk α (GrowableVector a) %1 ->
  ( forall β.
    Borrow bk (β /\ α) (Fixed.Vector a) %1 ->
    BO (β /\ α) result
  ) %1 ->
  BO α (Borrow bk α (GrowableVector a))
{-# INLINE withContent_ #-}
withContent_ vector action =
  withContent vector action Control.<&> \(result, vector) ->
    consume result `lseq` vector
