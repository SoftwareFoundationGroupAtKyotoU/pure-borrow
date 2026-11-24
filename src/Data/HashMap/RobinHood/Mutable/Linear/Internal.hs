{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Trusted representation and operations of the Robin Hood hash table.
module Data.HashMap.RobinHood.Mutable.Linear.Internal (
  module Data.HashMap.RobinHood.Mutable.Linear.Internal,
) where

import Control.Functor.Linear (asks, runReader)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO (askLinearly, evaluateBO)
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..))
import Control.Monad.Borrow.Pure.Clone (Clone (..))
import Control.Monad.Borrow.Pure.Lifetime.Token (Linearly, withLinearly)
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Array.Mutable.Linear qualified as LA
import Data.Bits (finiteBitSize, unsafeShiftR, xor, (.&.))
import Data.Foldable qualified as NonLinear
import Data.Functor.Linear qualified as Data
import Data.Hashable (Hashable (..))
import Data.Semigroup (Max (..))
import Data.Unrestricted.Linear qualified as Ur
import Data.Word (Word32, Word64, Word8)
import GHC.Base (Type, UnliftedType)
import GHC.Exts qualified as GHC
import GHC.TypeError (ErrorMessage (..), Unsatisfiable, unsatisfiable)
import Generics.Linear (Generic (..), Generic1 (..), V1)
import Math.NumberTheory.Logarithms (intLog2')
import Prelude.Linear hiding (insert, lookup)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

{-
Note [Array substrate]
~~~~~~~~~~~~~~~~~~~~~~
The slot storage is @linear-base@'s 'LA.Array', not this package's own
'Data.Vector.Mutable.Linear.Borrow.Vector'.

The table's operations are ordinary linear functions, not 'BO' actions: a
probe reads and writes slots while only threading the owned table through
@%1 ->@. 'LA.Array' is the substrate that supports exactly that, because its
reads and writes are @runRW#@-wrapped primops that inline into the probe loop.
Our own 'Data.Vector.Mutable.Linear.Borrow.Vector' is built for the 'BO' world
instead: outside 'BO' its accessors would each have to go through
'unsafePerformIO', which "AGENTS.md" then requires to be 'NOINLINE' -- one
non-inlinable call per probe step, in the hottest loop of the structure.

Bridging the two substrates therefore stays a deliberate choice, not an
oversight: a borrow-aware view of the table lives in
"Data.HashMap.RobinHood.Mutable.Linear.Borrow", which holds this owned table
behind a linear 'Data.Ref.Linear.Ref'.
-}

{-
Note [Element ownership]
~~~~~~~~~~~~~~~~~~~~~~~~
This table is *not* element-owning, in the sense of "AGENTS.md": its keys and
values are GC-owned and bound nonlinearly, and it linearly owns only its
backing array. That is what makes 'lookup' able to hand back @'Ur' v@ without
a 'Movable' constraint, 'dup2' able to stop at a shallow clone of the slot
array, and 'consume' able to drop the table in \(O(1)\).

Do not relax 'Slot' to bind its value linearly in order to store mutable
resources: every accessor here would then be duplicating a linearly owned
value into an 'Ur'. A map over linearly owned values needs a different
representation, and its materialization has to 'move' every entry.
-}

-- | Distance of an occupied slot from the bucket its fingerprint hashes to.
newtype DIB = DIB Word8
  deriving newtype
    ( NonLinear.Eq
    , NonLinear.Ord
    , NonLinear.Num
    , NonLinear.Enum
    , NonLinear.Real
    , NonLinear.Integral
    , Additive
    , Show
    )

-- | Cached mixed hash of a key, for fast rejection and for rehashing without hashing again.
newtype Fingerprint = Fingerprint Int
  deriving newtype (NonLinear.Eq, NonLinear.Ord, Show)

-- | Compute the fingerprint of a key: its hash, mixed so that every bit of the hash reaches the bits that pick a bucket.
fingerprint :: (Hashable k) => k -> Fingerprint
{-# INLINE fingerprint #-}
-- See Note [Mixing the hash].
fingerprint = Fingerprint NonLinear.. mixHash NonLinear.. hash

-- | A bijection on the values of an 'Int' in which every bit of the argument affects every bit of the result.
mixHash :: Int -> Int
{-# INLINE mixHash #-}
mixHash h
  | finiteBitSize h NonLinear.== 64 = NonLinear.fromIntegral (mixWord64 (NonLinear.fromIntegral h))
  | otherwise = NonLinear.fromIntegral (mixWord32 (NonLinear.fromIntegral h))

-- | The 64-bit finalizer of MurmurHash3.
mixWord64 :: Word64 -> Word64
{-# INLINE mixWord64 #-}
mixWord64 w = shift33 (shift33 (shift33 w NonLinear.* 0xff51afd7ed558ccd) NonLinear.* 0xc4ceb9fe1a85ec53)
  where
    shift33 :: Word64 -> Word64
    shift33 x = x `xor` (x `unsafeShiftR` 33)

-- | The 32-bit finalizer of MurmurHash3.
mixWord32 :: Word32 -> Word32
{-# INLINE mixWord32 #-}
mixWord32 w = shift16 (shift13 (shift16 w NonLinear.* 0x85ebca6b) NonLinear.* 0xc2b2ae35)
  where
    shift16, shift13 :: Word32 -> Word32
    shift16 x = x `xor` (x `unsafeShiftR` 16)
    shift13 x = x `xor` (x `unsafeShiftR` 13)

{-
Note [Mixing the hash]
~~~~~~~~~~~~~~~~~~~~~~
A key's bucket is the low bits of its fingerprint, but @hashable@ hashes an 'Int' or a 'Word' to itself and a 'Double' to its bits.
Unmixed, 'Int' keys that are multiples of a large power of two, or 'Double' keys that are whole numbers, would share their low bits and pile into one bucket of any table smaller than that power of two.
So 'fingerprint' passes the hash through 'mixHash', the finalizer of MurmurHash3 for the width of an 'Int', a bijection whose every output bit depends on every input bit.
Keys with different hashes then get unrelated buckets, while keys with equal hashes still get equal fingerprints, which is all that the fast rejection in 'probeKeyForAlter' relies on.
The price is locality: sequential 'Int' keys no longer fill consecutive slots.

One multiplication with the bucket taken from the top bits would be cheaper and would keep sequential keys from colliding, but it keeps the regular pattern of evenly spaced keys: for some spacings they line up, and the table doubles again and again to pull them apart.
-}

-- | Bucket index of a fingerprint. The capacity must be a power of two.
fingerprintBucket :: Fingerprint -> Int -> Int
{-# INLINE fingerprintBucket #-}
fingerprintBucket (Fingerprint h) capa = h .&. (capa - 1)

-- | A slot of the table, holding the fingerprint and DIB beside the entry.
data Slot k v where
  Empty :: Slot k v
  Occupied ::
    {-# UNPACK #-} !Fingerprint ->
    {-# UNPACK #-} !DIB ->
    !k ->
    !v ->
    Slot k v
  deriving (Show)

{- | Whether back-shift deletion must stop at this slot.

A slot stops the shift when it is empty, or when it already sits in its own
bucket and so cannot be moved closer to it.
-}
isStopSlot :: Slot k v -> NonLinear.Bool
{-# INLINE isStopSlot #-}
isStopSlot Empty = NonLinear.True
isStopSlot (Occupied _ dib _ _) = dib NonLinear.== 0

-- | Move an occupied slot one bucket closer to its own.
decrementSlot :: Slot k v %1 -> Slot k v
{-# INLINE decrementSlot #-}
decrementSlot Empty = Empty
decrementSlot (Occupied fp dib k v) = Occupied fp (dib NonLinear.- 1) k v

-- | The DIB of an occupied slot.
slotDIB :: Slot k v -> Maybe DIB
{-# INLINE slotDIB #-}
slotDIB Empty = Nothing
slotDIB (Occupied _ dib _ _) = Just dib

{- | The slot array.

This is a newtype rather than a synonym so that the 'LinearOnly' instance the
table needs is declared here, beside the type, instead of orphaned onto
@linear-base@'s 'LA.Array'.
-}
newtype Slots k v = Slots (LA.Array (Slot k v))

instance LinearOnly (Slots k v) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance Consumable (Slots k v) where
  consume (Slots arr) = consume arr
  {-# INLINE consume #-}

{- | \(O(n)\). Clone the slot array.

Keys and values are GC-owned, so a shallow copy of the backing store is a
complete duplication.
-}
instance Dupable (Slots k v) where
  -- See Note [Element ownership].
  dup2 (Slots arr) =
    dup2 arr & \(arr1, arr2) -> (Slots arr1, Slots arr2)
  {-# INLINE dup2 #-}

{- | \(O(n)\). Allocate a slot array of the given size, all 'Empty'.

'NOINLINE', and applied through 'GHC.noinline', for the same reason
@linear-base@'s own allocation primitives are: were the allocation duplicated
across use sites, or floated out of a loop, two tables would share one
backing store. The 'Unsafe.toLinear' escapes the continuation-passing shape of
'LA.alloc'; the array it releases is freshly allocated and unaliased, and the
consumed 'Linearly' is what makes handing it out linearly sound.
-}
allocSlots :: Int -> Linearly %1 -> Slots k v
{-# NOINLINE allocSlots #-}
allocSlots = GHC.noinline \count linear ->
  linear `lseq` Slots (unur (LA.alloc count Empty (Unsafe.toLinear Ur)))

-- | A hash table that linearly owns its backing store, with GC-owned keys and values.
data HashMap k v where
  -- See Note [Element ownership].
  HashMap ::
    -- | Number of live entries.
    {-# UNPACK #-} !Int ->
    -- | Number of buckets. Always a power of two.
    {-# UNPACK #-} !Int ->
    -- | An over-approximation of the largest DIB in the table.
    {-# UNPACK #-} !(Max DIB) ->
    -- | Slots, holding fingerprint, DIB and entry together.
    !(Slots k v) %1 ->
    HashMap k v

-- Nominal in the key: the slots are placed by the hash of the key type.
type role HashMap nominal representational

-- Guard against Template Haskell deriving a representation that exposes the hidden constructor.
instance
  ( Unsatisfiable
      ( 'ShowType (HashMap k v)
          ':<>: 'Text " has no Generic instance:"
          ':$$: 'Text "it is an abstract linear hash map, whose constructor is hidden."
      )
  ) =>
  Generic (HashMap k v)
  where
  type Rep (HashMap k v) = V1
  from = unsatisfiable
  to = unsatisfiable

instance
  ( Unsatisfiable
      ( 'ShowType (HashMap k)
          ':<>: 'Text " has no Generic1 instance:"
          ':$$: 'Text "it is an abstract linear hash map, whose constructor is hidden."
      )
  ) =>
  Generic1 (HashMap k)
  where
  type Rep1 (HashMap k) = V1
  from1 = unsatisfiable
  to1 = unsatisfiable

{- | A suspended unsuccessful lookup that can be resumed as an insertion.

The table must not be mutated between 'lookupForInsert' and
'unsafeInsertPrepared'.
-}
data InsertPlan k
  = InsertPlan
      !k
      {-# UNPACK #-} !Fingerprint
      {-# UNPACK #-} !Int
      !NonLinear.Bool
      {-# UNPACK #-} !DIB
      !(Maybe DIB)

-- Guard against Template Haskell deriving a representation that exposes the hidden constructor.
instance
  ( Unsatisfiable
      ( 'ShowType (InsertPlan k)
          ':<>: 'Text " has no Generic instance:"
          ':$$: 'Text "it is an abstract hash-map insertion plan, whose constructor is hidden."
      )
  ) =>
  Generic (InsertPlan k)
  where
  type Rep (InsertPlan k) = V1
  from = unsatisfiable
  to = unsatisfiable

instance
  ( Unsatisfiable
      ( 'ShowType InsertPlan
          ':<>: 'Text " has no Generic1 instance:"
          ':$$: 'Text "it is an abstract hash-map insertion plan, whose constructor is hidden."
      )
  ) =>
  Generic1 InsertPlan
  where
  type Rep1 InsertPlan = V1
  from1 = unsatisfiable
  to1 = unsatisfiable

instance Consumable (HashMap k v) where
  consume (HashMap _ _ _ slots) = consume slots
  {-# INLINE consume #-}

instance Dupable (HashMap k v) where
  dup2 (HashMap size capa maxDIB slots) =
    let %1 !(slots1, slots2) = dup slots
     in (HashMap size capa maxDIB slots1, HashMap size capa maxDIB slots2)
  {-# INLINE dup2 #-}

{- | \(O(n)\). Copies the table, sharing its GC-owned keys and values.

The table is only read, so any number of 'Control.Monad.Borrow.Pure.parBO' branches may clone the same table at once.
-}
instance Clone (HashMap k v) where
  -- See Note [Element ownership].
  clone = Unsafe.toLinear \(UnsafeAlias table) -> Control.do
    linear <- askLinearly
    evaluateBO (copyUnconsumed table linear)
  {-# INLINE clone #-}

{- | \(O(n)\). A copy of a table that stays with its owner, for cloning it through a shared borrow.

The table is not consumed: 'LA.size' and 'LA.slice' hand its slot array back unchanged, as the second and the first component of their results, and both are dropped here.
Evaluate the copy inside the lifetime of the borrow, as 'clone' does with 'evaluateBO', so that it is taken before the owner can write to the table again.

'NOINLINE', applied through 'GHC.noinline', and taking a 'Linearly' of its own, for the reason 'allocSlots' is: without a token, two copies of one table are the same expression, and GHC shared one copy between the clones of a loop, which then wrote to one slot array.
-}
copyUnconsumed :: HashMap k v -> Linearly %1 -> HashMap k v
{-# NOINLINE copyUnconsumed #-}
copyUnconsumed = GHC.noinline \(HashMap size capa maxDIB (Slots arr)) linear ->
  linear `lseq` case LA.size arr of
    (Ur physCapa, _) -> case LA.slice 0 physCapa arr of
      (_, copied) -> HashMap size capa maxDIB (Slots copied)

instance
  (Unsatisfiable ('Text "HashMap is only usable in linear context")) =>
  Movable (HashMap k v)
  where
  move = unsatisfiable

instance LinearOnly (HashMap k v) where
  linearOnly = UnsafeLinearOnly

-- | The table grows once this fraction of its buckets is occupied.
maxLoadFactor :: NonLinear.Double
maxLoadFactor = 0.75

{- | The largest DIB the table tolerates before growing.

The slot array is over-allocated by this many buckets, so a probe that starts
in the last bucket can run to the DIB limit without wrapping around.
-}
maxDibLimit :: DIB
maxDibLimit = 127

{- | \(O(n)\). An empty table sized for at least the given number of entries.

The bucket count is rounded up to a power of two.
-}
new :: Int -> Linearly %1 -> HashMap k v
new capa = runReader Control.do
  let !capa' = 2 ^ intLog2' (2 * max 1 capa - 1)
      !physCapa = capa' + fromIntegral maxDibLimit
  slots <- asks $ allocSlots physCapa
  Control.pure $ HashMap 0 capa' 0 slots

{- | \(O(n)\). Consume the table, folding a monoid over its entries.

Iteration order is unspecified.
-}
foldMapWithKey ::
  forall w k v.
  (Monoid w) =>
  (k -> v -> w) ->
  HashMap k v %1 ->
  w
foldMapWithKey f (HashMap size capa _ slots) = go 0 0 slots mempty
  where
    physCapa = capa + fromIntegral maxDibLimit
    go :: Int -> Int -> Slots k v %1 -> w -> w
    go !i !count !slots !acc
      | count == size || i == physCapa = slots `lseq` acc
      | otherwise =
          unsafeGetSlot i slots & \case
            (Ur (Occupied _ _ k v), slots') ->
              go (i + 1) (count + 1) slots' (acc <> f k v)
            (Ur Empty, slots') ->
              go (i + 1) count slots' acc

unsafeGetSlot :: Int -> Slots k v %1 -> (Ur (Slot k v), Slots k v)
{-# INLINE unsafeGetSlot #-}
unsafeGetSlot i (Slots arr) =
  LA.unsafeGet i arr & \(slot, arr) -> (slot, Slots arr)

unsafeSetSlot :: Int -> Slot k v -> Slots k v %1 -> Slots k v
{-# INLINE unsafeSetSlot #-}
-- Strict in the slot, so that the array never holds a thunk.
unsafeSetSlot i !slot (Slots arr) = Slots (LA.unsafeSet i slot arr)

-- | \(O(1)\) amortized. Insert an entry, returning the value it displaced.
insert :: (Hashable k) => k -> v -> HashMap k v %1 -> (Ur (Maybe v), HashMap k v)
insert k v =
  unswapper
    . alterF (\mval -> Swapper (Ur (Just v)) mval) k

-- | \(O(n)\) amortized. Insert every entry, later keys winning over earlier.
insertMany ::
  (Hashable k) =>
  [(k, v)] ->
  HashMap k v %1 ->
  HashMap k v
{-# INLINE insertMany #-}
insertMany kvs hm =
  appEndo
    ( getDual
        ( NonLinear.foldMap'
            (\(!k, !v) -> Dual $ Endo $ uncurry lseq . insert k v)
            kvs
        )
    )
    hm

-- | \(O(1)\) amortized. Insert, update or delete the entry at a key.
alter ::
  forall k v.
  (Hashable k) =>
  (Maybe v -> Maybe v) ->
  k ->
  HashMap k v %1 ->
  HashMap k v
alter f k hm =
  case probeKeyForAlter k hm of
    (# NotFound st, hm #) ->
      -- Absent: only an insertion can change anything.
      case f Nothing of
        Nothing -> hm
        Just !v -> probeForInsert k v st hm
    (# Found loc@Location {foundAt, slotFp, slotDIB, val}, hm #) ->
      case f (Just val) of
        Nothing -> deleteFrom loc hm
        (Just !v) ->
          -- Present: the slot keeps its fingerprint and DIB.
          hm & \(HashMap size capa maxDIB slots) -> DataFlow.do
            slots <- unsafeSetSlot foundAt (Occupied slotFp slotDIB k v) slots
            HashMap size capa maxDIB slots

-- | \(O(1)\). The number of live entries.
size :: HashMap k v %1 -> (Ur Int, HashMap k v)
{-# INLINE size #-}
size (HashMap sz capa maxDIB slots) = (Ur sz, HashMap sz capa maxDIB slots)

-- | \(O(1)\). The number of buckets.
capacity :: HashMap k v %1 -> (Ur Int, HashMap k v)
{-# INLINE capacity #-}
capacity (HashMap sz capa maxDIB slots) = (Ur capa, HashMap sz capa maxDIB slots)

{- | \(O(n)\) amortized, for \(n\) the capacity of the table with fewer entries. Insert every entry of the second table into the first.

A key present in both takes the second table's value, as with 'insert'.
-}
extend :: (Hashable k) => HashMap k v %1 -> HashMap k v %1 -> HashMap k v
extend target donor = case (size target, size donor) of
  ((Ur targetSize, target), (Ur donorSize, donor)) ->
    -- The smaller table's entries move into the larger one.
    if donorSize NonLinear.<= targetSize
      then moveEntriesWith insertReplacing donor target
      else moveEntriesWith insertAbsent target donor

-- | Insert every entry of the first table into the second with the given insertion, consuming the first.
moveEntriesWith ::
  forall k v.
  (k -> v -> HashMap k v %1 -> HashMap k v) ->
  HashMap k v %1 ->
  HashMap k v %1 ->
  HashMap k v
{-# INLINE moveEntriesWith #-}
moveEntriesWith place (HashMap size capa _ slots) = go 0 0 slots
  where
    physCapa = capa + fromIntegral maxDibLimit
    go :: Int -> Int -> Slots k v %1 -> HashMap k v %1 -> HashMap k v
    go !i !count !slots !dest
      | count == size || i == physCapa = slots `lseq` dest
      | otherwise =
          unsafeGetSlot i slots & \case
            (Ur (Occupied _ _ k v), slots') -> go (i + 1) (count + 1) slots' (place k v dest)
            (Ur Empty, slots') -> go (i + 1) count slots' dest

-- | Insert an entry, replacing the value at its key, if any.
insertReplacing :: (Hashable k) => k -> v -> HashMap k v %1 -> HashMap k v
{-# INLINE insertReplacing #-}
insertReplacing k v hm = case probeKeyForAlter k hm of
  (# NotFound st, hm #) -> probeForInsert k v st hm
  (# Found Location {foundAt, slotFp, slotDIB}, hm #) ->
    hm & \(HashMap size capa maxDIB slots) -> DataFlow.do
      slots <- unsafeSetSlot foundAt (Occupied slotFp slotDIB k v) slots
      HashMap size capa maxDIB slots

-- | Insert an entry only where its key is absent, keeping the value already there otherwise.
insertAbsent :: (Hashable k) => k -> v -> HashMap k v %1 -> HashMap k v
{-# INLINE insertAbsent #-}
insertAbsent k v hm = case probeKeyForAlter k hm of
  (# NotFound st, hm #) -> probeForInsert k v st hm
  (# Found _, hm #) -> hm

-- | \(O(1)\) amortized. 'alter' with the replacement produced in a functor.
alterF ::
  (Hashable k, Control.Functor f) =>
  (Maybe v -> f (Ur (Maybe v))) %1 ->
  k ->
  HashMap k v %1 ->
  f (HashMap k v)
alterF f k hm =
  case probeKeyForAlter k hm of
    (# NotFound st, hm #) ->
      -- Absent: only an insertion can change anything.
      f Nothing Control.<&> \case
        Ur Nothing -> hm
        Ur (Just !v) -> probeForInsert k v st hm
    (# Found loc@Location {foundAt, slotFp, slotDIB, val}, hm #) ->
      f (Just val) Control.<&> \case
        Ur Nothing -> deleteFrom loc hm
        Ur (Just !v) ->
          -- Present: the slot keeps its fingerprint and DIB.
          hm & \(HashMap size capa maxDIB slots) -> DataFlow.do
            slots <- unsafeSetSlot foundAt (Occupied slotFp slotDIB k v) slots
            HashMap size capa maxDIB slots

{- | \(O(1)\) amortized. Look a key up, and on a miss suspend the probe.

The suspended probe can be resumed as an insertion by
'unsafeInsertPrepared', which then costs no second traversal.
-}
lookupForInsert ::
  (Hashable k) =>
  k ->
  HashMap k v %1 ->
  (Ur (Either v (InsertPlan k)), HashMap k v)
{-# INLINE lookupForInsert #-}
lookupForInsert k hm = case probeKeyForAlter k hm of
  (# Found Location {val}, hm #) -> (Ur (Left val), hm)
  (# NotFound ProbeSuspended {..}, hm #) ->
    ( Ur
        ( Right
            ( InsertPlan
                k
                searchFp
                offset
                (case endType of Vacant -> NonLinear.True; Paused -> NonLinear.False)
                dibAtMiss
                cachedDIB
            )
        )
    , hm
    )

{- | \(O(1)\) amortized. Insert using a plan returned by 'lookupForInsert'.

The table must be the same table, with no intervening mutation.
Violating this precondition can corrupt the table, and read or write outside its slot array.
-}
unsafeInsertPrepared :: InsertPlan k -> v -> HashMap k v %1 -> HashMap k v
{-# INLINE unsafeInsertPrepared #-}
unsafeInsertPrepared (InsertPlan k searchFp offset vacant dibAtMiss cachedDIB) v =
  probeForInsert
    k
    v
    ProbeSuspended
      { searchFp
      , offset
      , endType = if vacant then Vacant else Paused
      , dibAtMiss
      , cachedDIB
      }

-- | A functor that carries the displaced value beside the result.
data Swapper v a where
  Swapper :: a %1 -> Maybe v -> Swapper v a

unswapper :: Swapper v a %1 -> (Ur (Maybe v), a)
{-# INLINE unswapper #-}
unswapper (Swapper l b) = (Ur b, l)

instance Data.Functor (Swapper v) where
  {-# SPECIALIZE instance Data.Functor (Swapper v) #-}
  fmap f = \(Swapper l b) -> Swapper (f l) (b :: Maybe v)
  {-# INLINE fmap #-}

instance Control.Functor (Swapper v) where
  {-# SPECIALIZE instance Control.Functor (Swapper v) #-}
  fmap f = \(Swapper l b) -> Swapper (f l) (b :: Maybe v)
  {-# INLINE fmap #-}

{- | Remove a located entry, shifting the run behind it back one bucket.

Backward-shift deletion keeps every remaining entry reachable without
tombstones: the shift stops at the first slot that is empty or already sits in
its own bucket.
-}
deleteFrom :: Location v -> HashMap k v %1 -> HashMap k v
deleteFrom Location {..} (HashMap size capa maxDIB slots) = go foundAt slots
  where
    physMax = capa + fromIntegral maxDibLimit - 1
    go :: Int -> Slots k v %1 -> HashMap k v
    go !i !slots
      | i == physMax = DataFlow.do
          slots <- unsafeSetSlot i Empty slots
          HashMap (size - 1) capa maxDIB slots
      | otherwise =
          unsafeGetSlot (i + 1) slots & \(Ur nextSlot, slots) ->
            if isStopSlot nextSlot
              then DataFlow.do
                slots <- unsafeSetSlot i Empty slots
                HashMap (size - 1) capa maxDIB slots
              else DataFlow.do
                slots <- unsafeSetSlot i (decrementSlot nextSlot) slots
                go (i + 1) slots

-- | Complete an insertion from the point where its probe stopped.
probeForInsert ::
  forall k v.
  k -> v -> ProbeSuspended -> HashMap k v %1 -> HashMap k v
{-# INLINE probeForInsert #-}
probeForInsert !k !v ProbeSuspended {..} (HashMap size capa maxDIB slots)
  | dibAtMiss NonLinear.> maxDibLimit || fromIntegral (size + 1) / fromIntegral capa >= maxLoadFactor =
      grow size capa searchFp k v slots
  | otherwise = case endType of
      Vacant -> DataFlow.do
        slots <- unsafeSetSlot offset (Occupied searchFp dibAtMiss k v) slots
        HashMap (size + 1) capa (maxDIB NonLinear.<> Max dibAtMiss) slots
      Paused
        | offset == physCapa -> grow size capa searchFp k v slots
        | otherwise -> case cachedDIB of
            Nothing ->
              -- A vacant slot; cannot arise from a paused probe, but is
              -- harmless to handle.
              DataFlow.do
                slots <- unsafeSetSlot offset (Occupied searchFp dibAtMiss k v) slots
                HashMap (size + 1) capa (maxDIB NonLinear.<> Max dibAtMiss) slots
            Just existingDib ->
              if existingDib NonLinear.< dibAtMiss
                then
                  unsafeGetSlot offset slots & \case
                    (Ur (Occupied existingFp _ k' v'), slots) -> DataFlow.do
                      -- Take from the rich and give to the poor.
                      slots <- unsafeSetSlot offset (Occupied searchFp dibAtMiss k v) slots
                      go size capa (Max dibAtMiss NonLinear.<> maxDIB) existingFp (existingDib + 1) k' v' (offset + 1) slots
                    (Ur Empty, slots) -> error "probeForInsert: impossible Empty slot" slots
                else
                  -- An entry must not move on past the DIB limit.
                  if dibAtMiss NonLinear.>= maxDibLimit
                    then grow size capa searchFp k v slots
                    else go size capa maxDIB searchFp (dibAtMiss + 1) k v (offset + 1) slots
  where
    physCapa :: Int
    physCapa = capa + fromIntegral maxDibLimit

    grow :: Int -> Int -> Fingerprint -> k -> v -> Slots k v %1 -> HashMap k v
    -- See Note [Growing to separate colliding keys].
    grow !size !capa !fp newK newV slots =
      let !limit = maxBucketsPerEntry * (size + 1)
       in if capa * 2 NonLinear.> limit
            then NonLinear.errorWithoutStackTrace collidingKeysMessage slots
            else
              withLinearly slots & \(lin, slots) ->
                rehashInto limit size physCapa fp newK newV 0 0 slots (new (capa * 2) lin)

    go ::
      Int ->
      Int ->
      Max DIB ->
      Fingerprint ->
      DIB ->
      k ->
      v ->
      Int ->
      Slots k v %1 ->
      HashMap k v
    -- Invariant: curMaxDIB <= maxDibLimit
    -- Invariant: newDIB <= maxDibLimit
    go !size !capa !curMaxDIB !newFp !newDib !newK !newV !i !slots =
      if i == physCapa
        then grow size capa newFp newK newV slots
        else
          unsafeGetSlot i slots
            & \case
              (Ur Empty, slots) ->
                DataFlow.do
                  slots <- unsafeSetSlot i (Occupied newFp newDib newK newV) slots
                  HashMap (size + 1) capa (curMaxDIB NonLinear.<> Max newDib) slots
              (Ur (Occupied existingFp existingDib k' v'), slots) ->
                if existingDib NonLinear.< newDib
                  then DataFlow.do
                    -- Take from the rich and give to the poor.
                    slots <- unsafeSetSlot i (Occupied newFp newDib newK newV) slots
                    -- existingDib < newDib
                    -- <==> existingDib + 1 <= newDib <= maxDibLimit
                    -- hence the invariant is maintained.
                    go size capa (Max newDib NonLinear.<> curMaxDIB) existingFp (existingDib + 1) k' v' (i + 1) slots
                  else
                    -- An entry must not move on past the DIB limit, where no lookup would find it.
                    if newDib NonLinear.>= maxDibLimit
                      then grow size capa newFp newK newV slots
                      else go size capa curMaxDIB newFp (newDib + 1) newK newV (i + 1) slots

{- | Insert a key known to be absent, reusing its cached fingerprint.

This is the rehashing path: it never has to hash a key again.
The table grows as needed, up to the given capacity, to separate keys that collide in it.
-}
insertFreshWithFingerprint :: Int -> Fingerprint -> k -> v -> HashMap k v %1 -> HashMap k v
{-# INLINE insertFreshWithFingerprint #-}
insertFreshWithFingerprint !limit !fp !k !v (HashMap size capa maxDIB slots) =
  let !start = fingerprintBucket fp capa
      !physCapa = capa + fromIntegral maxDibLimit
   in goFreshF limit size capa physCapa maxDIB 0 fp k v start slots

-- | The probe loop of 'insertFreshWithFingerprint'.
goFreshF ::
  Int -> -- largest capacity the table may grow to
  Int -> -- size
  Int -> -- capa
  Int -> -- physCapa
  Max DIB -> -- current max DIB
  DIB -> -- current DIB of the entry being inserted
  Fingerprint -> -- fingerprint of the entry being inserted
  k ->
  v ->
  Int -> -- current index
  Slots k v %1 ->
  HashMap k v
goFreshF !limit !size !capa !physCapa !curMaxDIB !dib !fp !k !v !i !slots
  | i == physCapa || dib NonLinear.> maxDibLimit =
      growFresh limit size capa physCapa fp k v slots
  | otherwise =
      unsafeGetSlot i slots & \case
        (Ur Empty, slots') -> DataFlow.do
          slots' <- unsafeSetSlot i (Occupied fp dib k v) slots'
          HashMap (size + 1) capa (curMaxDIB NonLinear.<> Max dib) slots'
        (Ur (Occupied existingFp existingDib k' v'), slots') ->
          if existingDib NonLinear.< dib
            then DataFlow.do
              -- Take from the rich and give to the poor.
              slots' <- unsafeSetSlot i (Occupied fp dib k v) slots'
              goFreshF limit size capa physCapa (curMaxDIB NonLinear.<> Max dib) (existingDib + 1) existingFp k' v' (i + 1) slots'
            else
              goFreshF limit size capa physCapa curMaxDIB (dib + 1) fp k v (i + 1) slots'

{- | Grow a table being filled by a rehash, which has no room for an entry within the DIB limit.

The @size@ entries of the slot array move into a table twice as large, and then the entry in hand, so none is lost.
-}
growFresh ::
  Int -> -- largest capacity the table may grow to
  Int -> -- size
  Int -> -- capa
  Int -> -- physCapa
  Fingerprint -> -- fingerprint of the entry in hand
  k ->
  v ->
  Slots k v %1 ->
  HashMap k v
-- See Note [Growing to separate colliding keys].
growFresh !limit !size !capa !physCapa !fp !k !v slots =
  if capa * 2 NonLinear.> limit
    then NonLinear.errorWithoutStackTrace collidingKeysMessage slots
    else
      withLinearly slots & \(lin, slots) ->
        rehashInto limit size physCapa fp k v 0 0 slots (new (capa * 2) lin)

{- | How many buckets a table may grow to for each of its entries, the one being inserted included.

A table grows past its load factor only to separate keys that collide in it, which keys with different hashes practically never need.
-}
maxBucketsPerEntry :: Int
maxBucketsPerEntry = 64

-- | The error of an insertion whose key collides with more keys than a table can hold together.
collidingKeysMessage :: NonLinear.String
collidingKeysMessage = "Data.HashMap.RobinHood: too many keys collide: more than 128 keys have the same hash, or many hashes are each shared by dozens of keys; this is expected to be rare, so please report it to the developers of pure-borrow, with a reproduction if possible (see \"Colliding keys\" in the module's documentation)"

{-
Note [Growing to separate colliding keys]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A bucket is a slot at which a probe may start: a table of capacity @capa@ has @capa@ of them, and 'fingerprintBucket' gives a key its own.
The slot array has 'maxDibLimit' more slots at its end, which are no buckets.

A table grows, to twice as many buckets, for one of two reasons.
The first is that it is full: three quarters of its buckets are occupied, by 'maxLoadFactor', which is 4/3 buckets per entry.
Doubling it then gives 8/3, so a table that grows only when full stays between 4/3 and 8/3 buckets per entry, however large it gets.
The second is that an entry finds no slot within 'maxDibLimit' of its bucket, because the entries crowding after that bucket would push it further away.
Sharing a bucket does not cause that by itself: 128 keys of one hash fit, in their bucket and the 127 slots after it.
This second kind of growth exists to separate keys that collide, whatever the load, and with the hash mixed it practically never happens to keys of different hashes (Note [Mixing the hash]).

A rehash moves every entry into a table twice as large with 'insertFreshWithFingerprint'.
When an entry finds no slot there either, 'growFresh' moves the entries placed so far, and then the one in hand, into a table twice as large again, so no entry is lost.

No growth takes a table past 'maxBucketsPerEntry' buckets per entry; an insertion that would need one raises 'collidingKeysMessage' instead.
That stops the growth for keys that no table can separate, as 129 keys of one hash are, and bounds the memory that keys made to collide can claim.
From a table that only ever grew when full, it leaves four or five doublings of the second kind: 64 is 24 times 8/3 and 48 times 4/3.
Growth of the first kind never comes near the limit, so it never raises.
'probeForInsert' computes the limit from the number of entries and passes it down unchanged, so it bounds every growth of one insertion; the table that a rehash is filling holds only part of the entries and could not compute it.

An insertion that raises leaves the table it was given half-updated: 'probeForInsert' may already have written into its slot array in place before it called 'grow'.
Nothing observes that table afterwards, since no safe code can catch an exception and go on using a linearly owned table, or the 'Ref' that holds the table of the borrow-aware map.
An operation that one day catches exceptions inside 'BO' must not hand such a table back.
-}

-- | Move every entry of the old slot array into a fresh table, then insert.
rehashInto ::
  Int -> -- largest capacity the fresh table may grow to
  Int -> -- size of the old table
  Int -> -- physCapa of the old table
  Fingerprint -> -- fingerprint of the key to insert
  k -> -- key to insert
  v -> -- value to insert
  Int -> -- current index
  Int -> -- entries moved so far
  Slots k v %1 ->
  HashMap k v %1 ->
  HashMap k v
rehashInto !limit !oldSize !oldPhysCapa !fp !k !v !i !count !oldSlots !newMap
  | count == oldSize || i >= oldPhysCapa =
      oldSlots `lseq` insertFreshWithFingerprint limit fp k v newMap
  | otherwise =
      unsafeGetSlot i oldSlots & \case
        (Ur (Occupied fp' _ k' v'), oldSlots') ->
          rehashInto limit oldSize oldPhysCapa fp k v (i + 1) (count + 1) oldSlots' (insertFreshWithFingerprint limit fp' k' v' newMap)
        (Ur Empty, oldSlots') ->
          rehashInto limit oldSize oldPhysCapa fp k v (i + 1) count oldSlots' newMap

-- | \(O(1)\) amortized. The value stored at a key, if any.
lookup :: (Hashable k) => k -> HashMap k v %1 -> (Ur (Maybe v), HashMap k v)
lookup k hm =
  case probeKeyForAlter k hm of
    (# NotFound _, hm #) -> (Ur Nothing, hm)
    (# Found Location {val}, hm #) -> (Ur (Just val), hm)

-- | \(O(1)\) amortized. Whether a key is present.
member :: (Hashable k) => k -> HashMap k v %1 -> (Ur Bool, HashMap k v)
member k hm =
  case probeKeyForAlter k hm of
    (# NotFound {}, hm #) -> (Ur False, hm)
    (# Found {}, hm #) -> (Ur True, hm)

-- | \(O(1)\) amortized. Remove a key, returning the value it held.
delete :: (Hashable k) => k -> HashMap k v %1 -> (Ur (Maybe v), HashMap k v)
{-# INLINE delete #-}
delete k =
  unswapper
    . alterF (\old -> Swapper (Ur Nothing) old) k

-- | Where a successful probe stopped, and what it found there.
type Location :: Type -> UnliftedType
data Location v = Location
  { foundAt :: !Int
  , slotFp :: {-# UNPACK #-} !Fingerprint
  , slotDIB :: {-# UNPACK #-} !DIB
  , val :: !v
  }

-- | The outcome of a probe.
type LookupResult :: Type -> UnliftedType
data LookupResult v where
  Found :: !(Location v) -> LookupResult v
  NotFound :: {-# UNPACK #-} !ProbeSuspended -> LookupResult v

-- | Everything an unsuccessful probe learned, so an insertion can resume it.
type ProbeSuspended :: UnliftedType
data ProbeSuspended = ProbeSuspended
  { searchFp :: {-# UNPACK #-} !Fingerprint
  -- ^ Fingerprint of the key being searched for.
  , offset :: {-# UNPACK #-} !Int
  -- ^ Index the probe stopped at.
  , endType :: !EndType
  , dibAtMiss :: {-# UNPACK #-} !DIB
  -- ^ DIB the searched key would have at 'offset'.
  , cachedDIB :: !(Maybe DIB)
  -- ^ 'Nothing' for an empty slot, @'Just' dib@ for an occupied one.
  }

-- | Why a probe stopped: on a vacant slot, or on an occupied one.
newtype EndType = EndType (# (# #) | (# #) #)

pattern Vacant :: EndType
pattern Vacant = EndType (# (# #) | #)

pattern Paused :: EndType
pattern Paused = EndType (# | (# #) #)

{-# COMPLETE Paused, Vacant #-}

{- | The single probe shared by every lookup, insertion and deletion.

On a miss it returns enough state for an insertion to continue from where the
probe stopped, rather than starting the traversal again.
-}
probeKeyForAlter :: forall k v. (Hashable k) => k -> HashMap k v %1 -> (# LookupResult v, HashMap k v #)
{-# INLINE probeKeyForAlter #-}
probeKeyForAlter k (HashMap size capa maxDIB slots) =
  go start 0 slots
  where
    !searchFp = fingerprint k
    !start = fingerprintBucket searchFp capa
    !physCapa = capa + fromIntegral maxDibLimit
    go :: Int -> DIB -> Slots k v %1 -> (# LookupResult v, HashMap k v #)
    go !idx !dib !slots
      | idx == physCapa || dib NonLinear.== maxDibLimit + 1 =
          (#
            NotFound
              ProbeSuspended
                { searchFp
                , offset = idx
                , endType = Paused
                , dibAtMiss = dib
                , cachedDIB = Nothing -- dummy: always triggers a grow
                }
            , HashMap size capa maxDIB slots
          #)
      | dib NonLinear.> getMax maxDIB =
          -- Past the largest DIB in the table: no key can live any further out.
          unsafeGetSlot idx slots & \(Ur slot, slots) ->
            let endType = case slot of
                  Empty -> Vacant
                  _ -> Paused
             in (#
                  NotFound
                    ProbeSuspended
                      { searchFp
                      , offset = idx
                      , endType
                      , dibAtMiss = dib
                      , cachedDIB = slotDIB slot
                      }
                  , HashMap size capa maxDIB slots
                #)
      | otherwise =
          unsafeGetSlot idx slots & \case
            (Ur Empty, slots) ->
              (#
                NotFound
                  ProbeSuspended
                    { searchFp
                    , offset = idx
                    , endType = Vacant
                    , dibAtMiss = dib
                    , cachedDIB = Nothing
                    }
                , HashMap size capa maxDIB slots
              #)
            (Ur (Occupied slotFp existingDib k' val), slots) ->
              if
                | existingDib NonLinear.< dib ->
                    -- Robin Hood early exit: the key would have displaced this
                    -- entry, so it cannot be further along the run.
                    (#
                      NotFound
                        ProbeSuspended
                          { searchFp
                          , offset = idx
                          , endType = Paused
                          , dibAtMiss = dib
                          , cachedDIB = Just existingDib
                          }
                      , HashMap size capa maxDIB slots
                    #)
                | slotFp NonLinear./= searchFp -> go (idx + 1) (dib + 1) slots -- reject without touching the key
                | k NonLinear.== k' ->
                    (#
                      Found Location {foundAt = idx, slotFp, slotDIB = existingDib, val}
                      , HashMap size capa maxDIB slots
                    #)
                | otherwise -> go (idx + 1) (dib + 1) slots

{- | \(O(n)\). Consume the table into its entries.

Entry order is unspecified.
-}
toList :: HashMap k v %1 -> Ur [(k, v)]
{-# INLINE toList #-}
-- Through 'Endo', so that the list is built by one right-nested traversal rather than repeated concatenation.
toList =
  Ur.lift materialiseDiffList
    . foldMapWithKey (\ !k !v -> Ur (Endo ((k, v) :)))

-- | Run a difference list built by 'toList' into an ordinary list.
materialiseDiffList :: Endo [a] -> [a]
{-# INLINE materialiseDiffList #-}
materialiseDiffList diff = appEndo diff []

{- | \(O(n)\) amortized. Build a table from a list, later keys winning.

It raises an error on [too many colliding keys]("Data.HashMap.RobinHood.Mutable.Linear#collisions").
-}
fromList :: (Hashable k) => [(k, v)] -> Linearly %1 -> HashMap k v
fromList kvs = insertMany kvs . new (NonLinear.length kvs)
