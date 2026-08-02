{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
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
import Control.Monad.Borrow.Pure.Lifetime.Token (Linearly, withLinearly)
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Array.Mutable.Linear qualified as LA
import Data.Bits ((.&.))
import Data.Foldable qualified as NonLinear
import Data.Functor.Linear qualified as Data
import Data.Hashable (Hashable (..))
import Data.Semigroup (Max (..))
import Data.Unrestricted.Linear qualified as Ur
import Data.Word (Word8)
import GHC.Base (Type, UnliftedType)
import GHC.Exts qualified as GHC
import GHC.TypeError (ErrorMessage (..), Unsatisfiable, unsatisfiable)
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

-- | Cached hash value, for fast rejection and for rehashing without rehashing.
newtype Fingerprint = Fingerprint Int
  deriving newtype (NonLinear.Eq, NonLinear.Ord, Show)

-- | Compute the fingerprint of a key.
fingerprint :: (Hashable k) => k -> Fingerprint
{-# INLINE fingerprint #-}
fingerprint = Fingerprint NonLinear.. hash

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
complete duplication. See Note [Element ownership].
-}
instance Dupable (Slots k v) where
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

{- | A mutable hash table using Robin Hood hashing with backward-shift deletion.

The table linearly owns its backing store; its keys and values are GC-owned.
See Note [Element ownership].
-}
data HashMap k v where
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

instance Consumable (HashMap k v) where
  consume (HashMap _ _ _ slots) = consume slots
  {-# INLINE consume #-}

instance Dupable (HashMap k v) where
  dup2 (HashMap size capa maxDIB slots) =
    let %1 !(slots1, slots2) = dup slots
     in (HashMap size capa maxDIB slots1, HashMap size capa maxDIB slots2)
  {-# INLINE dup2 #-}

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
unsafeSetSlot i slot (Slots arr) = Slots (LA.unsafeSet i slot arr)

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
    (# Found loc, hm #) ->
      case f (Just loc.val) of
        Nothing -> deleteFrom loc hm
        (Just !v) ->
          -- Present: the slot keeps its fingerprint and DIB.
          hm & \(HashMap size capa maxDIB slots) -> DataFlow.do
            slots <- unsafeSetSlot loc.foundAt (Occupied loc.slotFp loc.slotDIB k v) slots
            HashMap size capa maxDIB slots

-- | \(O(1)\). The number of live entries.
size :: HashMap k v %1 -> (Ur Int, HashMap k v)
{-# INLINE size #-}
size (HashMap sz capa maxDIB slots) = (Ur sz, HashMap sz capa maxDIB slots)

-- | \(O(1)\). The number of buckets.
capacity :: HashMap k v %1 -> (Ur Int, HashMap k v)
{-# INLINE capacity #-}
capacity (HashMap sz capa maxDIB slots) = (Ur capa, HashMap sz capa maxDIB slots)

{- | \(O(n)\) amortized. Union of two tables.

The smaller table is inserted into the larger, so a key present in both takes
the value from the table that is inserted second.
-}
union :: (Hashable k) => HashMap k v %1 -> HashMap k v %1 -> HashMap k v
{-# INLINE union #-}
union hm1 hm2 = case (size hm1, size hm2) of
  ((Ur sz1, hm1), (Ur sz2, hm2)) -> DataFlow.do
    (parent, child) <- if sz1 >= sz2 then (hm1, hm2) else (hm2, hm1)
    appEndo
      (foldMapWithKey (\ !k !v -> Endo $ uncurry lseq . insert k v) child)
      parent

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
    (# Found loc, hm #) ->
      f (Just loc.val) Control.<&> \case
        Ur Nothing -> deleteFrom loc hm
        Ur (Just !v) ->
          -- Present: the slot keeps its fingerprint and DIB.
          hm & \(HashMap size capa maxDIB slots) -> DataFlow.do
            slots <- unsafeSetSlot loc.foundAt (Occupied loc.slotFp loc.slotDIB k v) slots
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
  (# Found loc, hm #) -> (Ur (Left loc.val), hm)
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

The table must be the same table, with no intervening mutation. Violating
this precondition can corrupt the Robin Hood invariants.
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
                  if dibAtMiss NonLinear.== maxDibLimit NonLinear.- 1
                    then grow size capa searchFp k v slots
                    else go size capa maxDIB searchFp (dibAtMiss + 1) k v (offset + 1) slots
  where
    physCapa :: Int
    physCapa = capa + fromIntegral maxDibLimit

    grow :: Int -> Int -> Fingerprint -> k -> v -> Slots k v %1 -> HashMap k v
    grow !size !capa !fp newK newV slots =
      withLinearly slots & \(lin, slots) ->
        rehashInto size physCapa fp newK newV 0 0 slots (new (capa * 2) lin)

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
                    if newDib NonLinear.== maxDibLimit NonLinear.- 1
                      then grow size capa newFp newK newV slots
                      else go size capa curMaxDIB newFp (newDib + 1) newK newV (i + 1) slots

{- | Insert a key known to be absent, reusing its cached fingerprint.

This is the rehashing path: it never has to hash a key again.
-}
insertFreshWithFingerprint :: Fingerprint -> k -> v -> HashMap k v %1 -> HashMap k v
{-# INLINE insertFreshWithFingerprint #-}
insertFreshWithFingerprint !fp !k !v (HashMap size capa maxDIB slots) =
  let !start = fingerprintBucket fp capa
      !physCapa = capa + fromIntegral maxDibLimit
   in goFreshF size capa physCapa maxDIB 0 fp k v start slots

-- | The probe loop of 'insertFreshWithFingerprint'.
goFreshF ::
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
goFreshF !size !capa !physCapa !curMaxDIB !dib !fp !k !v !i !slots
  | i == physCapa || dib NonLinear.> maxDibLimit =
      -- Should not arise during a rehash into a table twice the size, but
      -- growing again is the safe response.
      withLinearly slots & \(lin, slots) ->
        slots `lseq` insertFreshWithFingerprint fp k v (new (capa * 2) lin)
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
              goFreshF size capa physCapa (curMaxDIB NonLinear.<> Max dib) (existingDib + 1) existingFp k' v' (i + 1) slots'
            else
              goFreshF size capa physCapa curMaxDIB (dib + 1) fp k v (i + 1) slots'

-- | Move every entry of the old slot array into a fresh table, then insert.
rehashInto ::
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
rehashInto !oldSize !oldPhysCapa !fp !k !v !i !count !oldSlots !newMap
  | count == oldSize || i >= oldPhysCapa =
      oldSlots `lseq` insertFreshWithFingerprint fp k v newMap
  | otherwise =
      unsafeGetSlot i oldSlots & \case
        (Ur (Occupied fp' _ k' v'), oldSlots') ->
          rehashInto oldSize oldPhysCapa fp k v (i + 1) (count + 1) oldSlots' (insertFreshWithFingerprint fp' k' v' newMap)
        (Ur Empty, oldSlots') ->
          rehashInto oldSize oldPhysCapa fp k v (i + 1) count oldSlots' newMap

-- | \(O(1)\) amortized. The value stored at a key, if any.
lookup :: (Hashable k) => k -> HashMap k v %1 -> (Ur (Maybe v), HashMap k v)
lookup k hm =
  case probeKeyForAlter k hm of
    (# NotFound _, hm #) -> (Ur Nothing, hm)
    (# Found !loc, hm #) -> (Ur (Just loc.val), hm)

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
      | dib NonLinear.> maxDIB.getMax =
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

Entry order is unspecified. Accumulation goes through 'Endo' so that the list
is built by one right-nested traversal rather than repeated concatenation.
-}
toList :: HashMap k v %1 -> Ur [(k, v)]
{-# INLINE toList #-}
toList =
  Ur.lift materialiseDiffList
    . foldMapWithKey (\ !k !v -> Ur (Endo ((k, v) :)))

-- | Run a difference list built by 'toList' into an ordinary list.
materialiseDiffList :: Endo [a] -> [a]
{-# INLINE materialiseDiffList #-}
materialiseDiffList diff = appEndo diff []

-- | \(O(n)\) amortized. Build a table from a list, later keys winning.
fromList :: (Hashable k) => [(k, v)] -> Linearly %1 -> HashMap k v
fromList kvs = insertMany kvs . new (NonLinear.length kvs)
