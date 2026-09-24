{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | A pure value that performs an effect, stored unevaluated where a 'Share' reaches it and forced by both branches of a 'parBO', performs it once.

The value is either a call of one of the reference primitives, which guard their own effects (Note [Pure Ref primitives run their effects at most once] in "Data.Ref.Linear.Unlifted.Internal"), or a call that updates memory in place and that an owner stores, such as linear-base's @Data.Array.Mutable.Linear.map@, which the owner evaluates as it stores it (Note [Stored contents are evaluated after noDuplicate#] there).
The race is not deterministic, so each case repeats its run; a correct library passes every run.

The two branches of a run wait for each other before they force what they share, and no two cases of this module run at once, so that the branches of one case do not wait for a capability that another case holds.
The cases raise the capabilities to two when there are fewer, since one cannot race.
A stored call also counts the times it is entered, which must be once per run.
Two threads that enter one unguarded call do not always both finish it: the runtime suspends one of them at its next pause, once it sees the other evaluating the same thunk.
Against a library without the guard, both branches entered the call in 165 to 200 of 200 runs from the threads that tasty runs tests in, where the contents showed the double update in far fewer, in none for some cases.
-}
module Control.Monad.Borrow.Pure.SharedEffectSpec (
  module Control.Monad.Borrow.Pure.SharedEffectSpec,
) where

import Control.Concurrent (getNumCapabilities, setNumCapabilities, yield)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Functor.Linear qualified as Control
import Control.Monad (forM, when)
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Affine (aff, pop)
import Control.Monad.Borrow.Pure.BO (evaluateBO)
import Control.Monad.Borrow.Pure.BO.Unsafe (unsafeSystemIOToBO)
import Data.Array.Mutable.Linear (Array)
import Data.Array.Mutable.Linear qualified as Array
import Data.HashMap.RobinHood.Mutable.Linear.Borrow qualified as HashMap
import Data.Hashable (Hashable (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefB
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Linear.Borrow.Experimental.Multiplicity qualified as Multiplicity
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Growable
import Data.Vector.Mutable.Linear.Borrow qualified as Vector
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow qualified as UnboxedGrowable
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as Unboxed
import GHC.Conc (numCapabilities)
import GHC.Exts (Multiplicity (One))
import Prelude.Linear qualified as PL
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause, ignoreTestBecause)
import Test.Tasty.HUnit
import Unsafe.Linear qualified as Unsafe

-- | A pure loop that GHC cannot fold away.
busy :: Int -> Int
{-# NOINLINE busy #-}
busy n = go 0 (abs n)
  where
    go :: Int -> Int -> Int
    go !acc 0 = acc
    go !acc i = go ((acc + i * i) `rem` 1_000_003) (i - 1)

-- | Some pure work before handing a value on, which only widens the window of the race.
pause :: Int -> a %1 -> a
{-# NOINLINE pause #-}
pause work a = case busy work of !_ -> a

-- | Held by each case while it runs, so that no two run at once.
racing :: MVar ()
{-# NOINLINE racing #-}
racing = unsafePerformIO (newMVar ())

-- | Run the runs of a case while no other case of this module runs, on two capabilities at least.
race :: IO a -> IO a
race runs = withMVar racing \() -> do
  capabilities <- getNumCapabilities
  when (capabilities < 2) (setNumCapabilities 2)
  runs

-- | A count of the branches of one 'parBO' that have arrived at the barrier.
newArrivals :: BO α (Ur (IORef Int))
newArrivals = unsafeSystemIOToBO (Ur <$> newIORef 0)

-- | Wait until both branches have arrived, so that they force what they share at the same moment, however late either started.
arriveAndWait :: IORef Int -> BO β ()
arriveAndWait arrivals = unsafeSystemIOToBO do
  atomicModifyIORef' arrivals \n -> (n + 1, ())
  let waitForBoth = do
        n <- readIORef arrivals
        if n >= 2 then pure () else yield >> waitForBoth
  waitForBoth

-- * The reference primitives

bump :: Mut α (Ref Int) %1 -> BO α ()
bump m = Control.do
  bumped <- RefB.modify (PL.+ 1) m
  Control.pure (PL.consume bumped)

-- | Wait for the other branch, then read the reference that the first component of the pair holds, unevaluated.
branch :: IORef Int -> Share β (Ref Int, ()) -> BO β (Ur Int)
branch arrivals sh = case splitPair sh of
  (inner, _) -> Control.do
    arriveAndWait arrivals
    x <- RefB.copyRef inner
    Control.pure (PL.move x)

type Effect = Ref Int %1 -> Linearly %1 -> Ref Int

{- | Store @effect inner@ unevaluated as the first component of a pair, read it through a 'Share' in both branches of a 'parBO', and return what each branch read and what is left in the end.

A component of a pair, because the contents of a reference are evaluated as it is stored: 'Ref.new' would run the effect once, in the parent, whatever the primitive does.
Every component must be @seed + 1@.
-}
probe :: Effect -> Int -> Int -> (Int, Int, Int)
{-# NOINLINE probe #-}
probe effect work seed = linearly \lin -> case dup3 lin of
  (l1, l2, l3) -> case Ref.new seed l1 of
    !inner -> case modifyBO (effect (pause work inner) l2, ()) l3 body of
      (Ur (x, y), (outer, ())) -> case Ref.free outer of
        final -> (x, y, final)
  where
    body :: forall α. Mut α (Ref Int, ()) %1 -> BO α (Ur (Int, Int))
    body mut = Control.do
      Ur arrivals <- newArrivals
      (r, restored) <- sharing mut \sh -> Control.do
        (Ur x, Ur y) <- parBO (branch arrivals sh) (branch arrivals sh)
        Control.pure (Ur (x, y))
      Control.pure (restored `PL.lseq` r)

viaAtomicModify :: Effect
viaAtomicModify r l = l `PL.lseq` Ref.atomicModify_ (PL.+ 1) r

viaModifyBO :: Effect
viaModifyBO r l = modifyBO_ r l bump

-- | The offsets from the seed over a number of runs, which must all be one.
offsets :: Effect -> IO [(Int, Int, Int)]
offsets effect = race $ forM [1 .. 100] \i -> do
  (x, y, final) <- evaluate (probe effect 2_000 (i * 10))
  pure (x - i * 10, y - i * 10, final - i * 10)

test_sharedEffect :: TestTree
test_sharedEffect =
  testGroup
    "an effect stored behind a Share runs once when two branches force it"
    [ testCase "Ref.atomicModify_" do
        results <- offsets viaAtomicModify
        filter (/= (1, 1, 1)) results @?= []
    , testCase "modifyBO_" do
        results <- offsets viaModifyBO
        filter (/= (1, 1, 1)) results @?= []
    ]

-- * Calls that update memory in place, stored by an owner

-- | The number of elements of each array.
elements :: Int
elements = 2_000

-- | How many times the stored call of the current run has been entered; the cases run one at a time.
entries :: IORef Int
{-# NOINLINE entries #-}
entries = unsafePerformIO (newIORef 0)

-- | Count an entry into the call it wraps, in whichever thread makes it, and go on with the call.
entered :: a %1 -> a
{-# NOINLINE entered #-}
entered = Unsafe.toLinear \call ->
  unsafeDupablePerformIO (atomicModifyIORef' entries (\n -> (n + 1, ())) >> pure call)

-- | The call that each case stores: counted, some work, then one added to every element of the array, in place.
increment :: Array Int %1 -> Array Int
increment arr = entered (Array.map (+ 1) (pause 20_000 arr))

-- | What each branch does with the array it reaches.
data Access
  = -- | Evaluate it, and report nothing.
    Evaluates
  | -- | Clone it, and report what the clone holds.
    Clones

access :: Access -> Share β (Array Int) %1 -> BO β (Ur [[Int]])
access Evaluates element = Control.do
  forced <- evaluateBO element
  Control.pure (forced `PL.lseq` Ur [])
access Clones element = Control.do
  copied <- clone element
  Control.pure (contentsOf copied)

contentsOf :: Array Int %1 -> Ur [[Int]]
contentsOf arr = case Array.toList arr of
  Ur xs -> Ur [xs]

-- | Share the owner with the two branches of a 'parBO', which wait for each other before they run, and return what the two report.
inBranches ::
  (forall β. Share β a -> BO β (Ur [[Int]])) ->
  (forall β. Share β a -> BO β (Ur [[Int]])) ->
  Mut α a %1 ->
  BO α (Ur [[Int]])
inBranches left right mut = Control.do
  Ur arrivals <- newArrivals
  (seen, restored) <- sharing mut \sh -> Control.do
    (Ur x, Ur y) <- parBO (arriveAndWait arrivals Control.>> left sh) (arriveAndWait arrivals Control.>> right sh)
    Control.pure (Ur (x <> y))
  Control.pure (restored `PL.lseq` seen)

{- | Build an owner of @increment arr@ with @build@, reach the array through a 'Share' of it with @reach@ in both branches of a 'parBO', and then clone it in the parent, in a run of its own, and return every copy that was reported.

The parent reads the owner in a run of its own: read in the run that forks, after the 'parBO', the owner can be evaluated before the fork, and the case would test nothing.
-}
storedCase ::
  forall o.
  (PL.Consumable o) =>
  Access ->
  (Array Int %1 -> Linearly %1 -> o) ->
  (forall β. Share β o -> BO β (Share β (Array Int))) ->
  Array Int %1 ->
  Linearly %1 ->
  [[Int]]
storedCase inBranch build reach arr lin = case dup3 lin of
  (l1, l2, l3) -> case modifyBO (build arr l1) l2 (inBranches reachAndAccess reachAndAccess) of
    (Ur seen, owner) -> case modifyBO owner l3 inParent of
      (Ur final, owner') -> owner' `PL.lseq` (seen <> final)
  where
    reachAndAccess :: Share β o -> BO β (Ur [[Int]])
    reachAndAccess sh = reach sh Control.>>= access inBranch
    inParent :: Mut α o %1 -> BO α (Ur [[Int]])
    inParent mut = case share mut of
      Ur sh -> reach sh Control.>>= access Clones

{- | Run a case 200 times, each on an array holding @[seed .. seed + elements - 1]@ for a seed of its own, evaluated first.

Return, for each run that entered the stored call other than once or reported a copy that is not the array with one added to each element, the number of entries and how many elements of each such copy are not.
The array is evaluated first because linear-base's @fromList@ hands over an unevaluated chain of writes, which two branches forcing it at once could leave half done.
-}
runStored :: (Array Int %1 -> Linearly %1 -> [[Int]]) -> IO [(Int, [Int])]
runStored body =
  race $
    concat <$> forM [1 .. 200] \run -> do
      let seed = run * 10_000
          expected = [seed + 1 .. seed + elements]
      writeIORef entries 0
      copies <- evaluate (force (Array.fromList [seed .. seed + elements - 1] (evaluatedFirst body)))
      times <- readIORef entries
      let wrong = [length (filter id (zipWith (/=) reported expected)) | reported <- copies, reported /= expected]
      pure [(times, wrong) | times /= 1 || not (null wrong)]

evaluatedFirst :: (Array Int %1 -> Linearly %1 -> [[Int]]) -> Array Int %1 -> [[Int]]
evaluatedFirst body arr = case Array.size arr of
  (Ur _, evaluated) -> linearly (body evaluated)

assertRanOnce :: [(Int, [Int])] -> Assertion
assertRanOnce failures =
  assertBool
    ( show (length failures)
        <> " of 200 runs entered the stored call other than once or incremented some element other than once; the entries, and the wrong elements of each wrong copy, in the first of them: "
        <> show (take 5 failures)
    )
    (null failures)

-- | Reach the array through the first component of a pair.
inFirst :: (Share β o -> BO β (Share β (Array Int))) -> Share β (o, ()) -> BO β (Share β (Array Int))
inFirst reach sh = case splitPair sh of
  (first, _) -> reach first

inRef :: Array Int %1 -> Linearly %1 -> Ref (Array Int)
inRef arr = Ref.new (increment arr)

readRef :: Share β (Ref (Array Int)) -> BO β (Share β (Array Int))
readRef sh = Control.do
  Ur contents <- RefB.readShare sh
  Control.pure contents

-- | The reference is the first component of a pair, left unevaluated, so that both branches enter 'Ref.new' itself.
inPairedRef :: Array Int %1 -> Linearly %1 -> (Ref (Array Int), ())
inPairedRef arr lin = (Ref.new (increment arr) lin, ())

{- | A reference to another array, evaluated here, and the write of the call over it left unevaluated as the first component of a pair, so that both branches enter 'Ref.unsafeWriteRef' itself.

The reference is evaluated here so that the first guard the branches meet is the write's own.
The write drops the other array, as 'Ref.unsafeWriteRef' drops what it overwrites.
-}
inPairedWrite :: Array Int %1 -> Linearly %1 -> (Ref (Array Int), ())
inPairedWrite arr lin = case Array.allocBeside 1 0 arr of
  (other, arr') -> case Ref.new other lin of
    !ref -> (Ref.unsafeWriteRef ref (increment arr'), ())

inVector :: Array Int %1 -> Linearly %1 -> Vector.Vector (Array Int)
inVector arr = Vector.fromList [increment arr]

firstOfVector :: Share β (Vector.Vector (Array Int)) -> BO β (Share β (Array Int))
firstOfVector sh = Vector.get 0 sh

-- | The vector is the first component of a pair, left unevaluated, so that both branches enter 'Vector.fromList' itself.
inPairedVector :: Array Int %1 -> Linearly %1 -> (Vector.Vector (Array Int), ())
inPairedVector arr lin = (Vector.fromList [increment arr] lin, ())

-- | As 'inPairedVector', but the call runs as the list's first cell is produced, rather than as its element is evaluated.
inPairedVectorOfCell :: Array Int %1 -> Linearly %1 -> (Vector.Vector (Array Int), ())
inPairedVectorOfCell arr lin = (Vector.fromList (listAfter arr) lin, ())

-- | A list of the array, whose first cell exists only once the call has run.
listAfter :: Array Int %1 -> [Array Int]
{-# NOINLINE listAfter #-}
listAfter arr = case Array.size (increment arr) of
  (Ur _, incremented) -> [incremented]

type OwningVector = Multiplicity.Vector 'One V.Vector (Array Int)

inOwningVector :: Array Int %1 -> Linearly %1 -> OwningVector
inOwningVector arr = Multiplicity.fromList [increment arr]

inPairedOwningVector :: Array Int %1 -> Linearly %1 -> (OwningVector, ())
inPairedOwningVector arr lin = (Multiplicity.fromList [increment arr] lin, ())

-- | The vector is built around another array, over which the call is written through a 'Mut', in a run of its own.
writtenToOwningVector :: Array Int %1 -> Linearly %1 -> OwningVector
writtenToOwningVector arr lin = case dup lin of
  (l1, l2) -> case Array.allocBeside 1 0 arr of
    (placeholder, arr') -> modifyBO_ (Multiplicity.fromList [placeholder] l1) l2 \mut -> Control.do
      written <- Multiplicity.write 0 (increment arr') mut
      Control.pure (PL.consume written)

firstOfOwningVector :: Share β OwningVector -> BO β (Share β (Array Int))
firstOfOwningVector sh = Multiplicity.get 0 sh

-- | An array that an unboxed vector stores through its lazily boxed representation, as it can store any linearly owned value.
newtype Stored = Stored (Array Int)

instance PL.Consumable (U.DoNotUnboxLazy Stored) where
  consume (U.DoNotUnboxLazy (Stored arr)) = PL.consume arr

type LazyUnboxed = Unboxed.Vector (U.DoNotUnboxLazy Stored)

type LazyUnboxedGrowable = UnboxedGrowable.GrowableVector (U.DoNotUnboxLazy Stored)

inUnboxed :: Array Int %1 -> Linearly %1 -> LazyUnboxed
inUnboxed arr = Unboxed.fromList [U.DoNotUnboxLazy (Stored (increment arr))]

inPairedUnboxed :: Array Int %1 -> Linearly %1 -> (LazyUnboxed, ())
inPairedUnboxed arr lin = (Unboxed.fromList [U.DoNotUnboxLazy (Stored (increment arr))] lin, ())

-- | As 'inPairedUnboxed', but the call runs as the list's first cell is produced.
inPairedUnboxedOfCell :: Array Int %1 -> Linearly %1 -> (LazyUnboxed, ())
inPairedUnboxedOfCell arr lin = (Unboxed.fromList (storedAfter arr) lin, ())

-- | A list of the array, whose first cell exists only once the call has run.
storedAfter :: Array Int %1 -> [U.DoNotUnboxLazy Stored]
{-# NOINLINE storedAfter #-}
storedAfter arr = case Array.size (increment arr) of
  (Ur _, incremented) -> [U.DoNotUnboxLazy (Stored incremented)]

firstOfUnboxed :: Share β LazyUnboxed -> BO β (Share β (Array Int))
firstOfUnboxed sh = upcast Control.<$> Unboxed.get 0 sh

inUnboxedGrowable :: Array Int %1 -> Linearly %1 -> LazyUnboxedGrowable
inUnboxedGrowable arr = UnboxedGrowable.fromList [U.DoNotUnboxLazy (Stored (increment arr))]

inPairedUnboxedGrowable :: Array Int %1 -> Linearly %1 -> (LazyUnboxedGrowable, ())
inPairedUnboxedGrowable arr lin = (UnboxedGrowable.fromList [U.DoNotUnboxLazy (Stored (increment arr))] lin, ())

firstOfUnboxedGrowable :: Share β LazyUnboxedGrowable -> BO β (Share β (Array Int))
firstOfUnboxedGrowable sh = upcast Control.<$> UnboxedGrowable.get 0 sh

-- * Writes through a Mut, of a call passed to a function that makes the write

{-
Each function below builds an owner around another array, and writes the value it is given over that array through a 'Mut', in a run of its own; a case leaves a call of the function unevaluated in a pair.
A write that let GHC see its demand on the value would make the function strict in the value, and the pair's thunk would then evaluate the call before the function's run, in both branches, outside the run's guard.
-}

setToVector :: Array Int %1 -> Array Int %1 -> Linearly %1 -> Vector.Vector (Array Int)
{-# NOINLINE setToVector #-}
setToVector other value lin = case dup lin of
  (l1, l2) -> modifyBO_ (Vector.fromList [other] l1) l2 \mut -> Control.do
    (old, mut') <- Vector.set 0 value mut
    Control.pure (PL.consume old `PL.lseq` PL.consume mut')

updateInVector :: Array Int %1 -> Array Int %1 -> Linearly %1 -> Vector.Vector (Array Int)
{-# NOINLINE updateInVector #-}
updateInVector other value lin = case dup lin of
  (l1, l2) -> modifyBO_ (Vector.fromList [other] l1) l2 \mut -> Control.do
    (old, mut') <- Vector.update 0 (\old -> Control.pure (old, value)) mut
    Control.pure (PL.consume old `PL.lseq` PL.consume mut')

setToUnboxed :: Array Int %1 -> Array Int %1 -> Linearly %1 -> LazyUnboxed
{-# NOINLINE setToUnboxed #-}
setToUnboxed other value lin = case dup lin of
  (l1, l2) -> modifyBO_ (Unboxed.fromList [U.DoNotUnboxLazy (Stored other)] l1) l2 \mut -> Control.do
    (old, mut') <- Unboxed.set 0 (U.DoNotUnboxLazy (Stored value)) mut
    Control.pure (PL.consume old `PL.lseq` PL.consume mut')

pushToGrowable :: Array Int %1 -> Array Int %1 -> Linearly %1 -> Growable.GrowableVector (Array Int)
{-# NOINLINE pushToGrowable #-}
pushToGrowable other value lin = case dup lin of
  (l1, l2) ->
    other `PL.lseq` modifyBO_ (Growable.empty l1) l2 \mut -> Control.do
      pushed <- Growable.push value mut
      Control.pure (PL.consume pushed)

pushToUnboxedGrowable :: Array Int %1 -> Array Int %1 -> Linearly %1 -> LazyUnboxedGrowable
{-# NOINLINE pushToUnboxedGrowable #-}
pushToUnboxedGrowable other value lin = case dup lin of
  (l1, l2) ->
    other `PL.lseq` modifyBO_ (UnboxedGrowable.empty l1) l2 \mut -> Control.do
      pushed <- UnboxedGrowable.push (U.DoNotUnboxLazy (Stored value)) mut
      Control.pure (PL.consume pushed)

writeToOwningVector :: Array Int %1 -> Array Int %1 -> Linearly %1 -> OwningVector
{-# NOINLINE writeToOwningVector #-}
writeToOwningVector other value lin = case dup lin of
  (l1, l2) -> modifyBO_ (Multiplicity.fromList [other] l1) l2 \mut -> Control.do
    written <- Multiplicity.write 0 value mut
    Control.pure (PL.consume written)

modifyRef :: Array Int %1 -> Array Int %1 -> Linearly %1 -> Ref (Array Int)
{-# NOINLINE modifyRef #-}
modifyRef other value lin = case dup lin of
  (l1, l2) -> modifyBO_ (Ref.new other l1) l2 \mut -> Control.do
    modified <- RefB.modify (\old -> old `PL.lseq` value) mut
    Control.pure (PL.consume modified)

-- | A call of @write other (increment arr)@, left unevaluated as the first component of a pair.
inPairedWriteBy :: (Array Int %1 -> Array Int %1 -> Linearly %1 -> o) -> Array Int %1 -> Linearly %1 -> (o, ())
inPairedWriteBy write arr lin = case Array.allocBeside 1 0 arr of
  (other, arr') -> (write other (increment arr') lin, ())

firstOfGrowable :: Share β (Growable.GrowableVector (Array Int)) -> BO β (Share β (Array Int))
firstOfGrowable sh = Growable.get 0 sh

{- | Leave the call unevaluated as the first component of a pair, and let one branch store a 'Share' of it in a reference while the other clones it.

Storing the 'Share' evaluates it, which evaluates the call, and nothing guards the call itself, so both branches can run it.
-}
storedShareCase :: Array Int %1 -> Linearly %1 -> [[Int]]
storedShareCase arr lin = case dup lin of
  (l1, l2) -> case modifyBO (increment arr, ()) l1 (inBranches storeShare cloneField) of
    (Ur seen, owner) -> case modifyBO owner l2 inParent of
      (Ur final, owner') -> owner' `PL.lseq` (seen <> final)
  where
    storeShare :: Share β (Array Int, ()) -> BO β (Ur [[Int]])
    storeShare sh = case splitPair sh of
      (field, _) -> Control.do
        lin' <- askLinearly
        ref <- Control.pure PL.$! Ref.new field lin'
        Control.pure (pop (aff ref) `PL.lseq` Ur [])
    cloneField :: Share β (Array Int, ()) -> BO β (Ur [[Int]])
    cloneField sh = case splitPair sh of
      (field, _) -> access Clones field
    inParent :: Mut α (Array Int, ()) %1 -> BO α (Ur [[Int]])
    inParent mut = case share mut of
      Ur sh -> cloneField sh

-- | A key whose hash takes some work, which widens the window of the race inside a union.
newtype Key = Key Int
  deriving (Eq)

instance Hashable Key where
  hashWithSalt salt (Key k) = case busy (2_000 + k) of
    !_ -> hashWithSalt salt k

type Table = HashMap.HashMap Key Int

-- | The entries from key @from@ to key @to@, each mapped to @seed@ more than itself.
tableEntries :: Int -> Int -> Int -> [(Key, Int)]
tableEntries seed from to = [(Key k, seed + k) | k <- [from .. to]]

-- | Evaluate a table in this thread.
evaluatedTable :: Table %1 -> Linearly %1 -> Table
evaluatedTable table lin = case modifyBO table lin size of
  (Ur _, table') -> table'
  where
    size :: Mut α Table %1 -> BO α (Ur Int)
    size mut = Control.do
      (Ur n, mut') <- HashMap.size mut
      Control.pure (mut' `PL.lseq` Ur n)

lookUp :: IORef Int -> Key -> Share β Table -> BO β (Ur (Maybe Int))
lookUp arrivals key sh = Control.do
  arriveAndWait arrivals
  (Ur found, sh') <- HashMap.lookup key sh
  Control.pure (sh' `PL.lseq` Ur found)

-- | Look two keys of the smaller table up in the two branches of a 'parBO', in the table that @table@ reaches.
lookUpInBothBranches :: (forall β. Share β o -> Share β Table) -> Mut α o %1 -> BO α (Ur (Maybe Int, Maybe Int))
lookUpInBothBranches table mut = Control.do
  Ur arrivals <- newArrivals
  (found, restored) <- sharing mut \sh -> Control.do
    (Ur x, Ur y) <- parBO (lookUp arrivals (Key 13) (table sh)) (lookUp arrivals (Key 14) (table sh))
    Control.pure (Ur (x, y))
  Control.pure (restored `PL.lseq` found)

sizeAndEntries :: Mut α Table %1 -> BO α (Ur (Int, [(Int, Int)]))
sizeAndEntries mut = Control.do
  (Ur size, mut') <- HashMap.size mut
  (Ur xs, mut'') <- HashMap.toList mut'
  Control.pure (mut'' `PL.lseq` Ur (size, sort [(k, v) | (Key k, v) <- xs]))

-- | The table that the first component of a pair holds.
firstTable :: Share β (Table, ()) -> Share β Table
firstTable sh = case splitPair sh of
  (table, _) -> table

{- | Unite a table of 12 entries with one of 4, both evaluated first, look two of the second table's keys up in the two branches of a 'parBO', and return what they found, then the size and the entries of the union, read in a run of its own.

The union inserts the smaller table into the larger in place, so two runs of it corrupt the larger table.
When @paired@, the union is left unevaluated as the first component of a pair, so that both branches enter 'HashMap.union' itself.
-}
unionRun :: Bool -> Int -> ((Maybe Int, Maybe Int), (Int, [(Int, Int)]))
{-# NOINLINE unionRun #-}
unionRun paired seed = PL.unur PL.$ linearly \lin -> case dup3 lin of
  (l1, l2, rest) -> case dup3 rest of
    (l3, l4, rest') -> case dup rest' of
      -- Evaluated here, not left to the union: a table evaluated inside it would run 'runBO', whose 'GHC.Exts.noDuplicate#' would keep a second thread out of the union too.
      (l5, l6) -> case evaluatedTable (HashMap.fromList (tableEntries seed 0 11) l1) l3 of
        !larger -> case evaluatedTable (HashMap.fromList (tableEntries seed 12 15) l2) l4 of
          !smaller
            | paired -> case modifyBO (HashMap.union larger smaller, ()) l5 (lookUpInBothBranches firstTable) of
                (Ur found, (united, ())) -> finish found united l6
            | otherwise -> case modifyBO (HashMap.union larger smaller) l5 (lookUpInBothBranches PL.id) of
                (Ur found, united) -> finish found united l6
  where
    finish :: (Maybe Int, Maybe Int) -> Table %1 -> Linearly %1 -> Ur ((Maybe Int, Maybe Int), (Int, [(Int, Int)]))
    finish found united lin = case modifyBO united lin sizeAndEntries of
      (Ur final, united') -> united' `PL.lseq` Ur (found, final)

unionCase :: Bool -> Assertion
unionCase paired = do
  wrong <-
    race $
      concat <$> forM [1 .. 200] \run -> do
        let seed = run * 1_000
            expected = ((Just (seed + 13), Just (seed + 14)), (16, [(k, seed + k) | k <- [0 .. 15]]))
        result <- evaluate (force (unionRun paired seed))
        pure [result | result /= expected]
  assertBool
    (show (length wrong) <> " unions went wrong, the first ones: " <> show (take 3 wrong))
    (null wrong)

{- | A race that the library does not prevent, whose case must fail.

One capability cannot race, and the case would pass there, which an expected failure reports as an error, so it is ignored when the program starts on one.
-}
knownRace :: String -> TestTree -> TestTree
knownRace why = (if numCapabilities < 2 then ignoreTestBecause "one capability cannot race" else id) . expectFailBecause why

test_storedCall :: TestTree
test_storedCall =
  testGroup
    "a call that updates memory in place, stored by an owner, runs once when two branches force it"
    [ testCase "Ref.new, read in both branches" do
        runStored (storedCase Evaluates inRef readRef) >>= assertRanOnce
    , testCase "Ref.new, cloned in both branches" do
        runStored (storedCase Clones inRef readRef) >>= assertRanOnce
    , testCase "Ref.new left unevaluated in a pair, read in both branches" do
        runStored (storedCase Evaluates inPairedRef (inFirst readRef)) >>= assertRanOnce
    , testCase "Ref.new left unevaluated in a pair, cloned in both branches" do
        runStored (storedCase Clones inPairedRef (inFirst readRef)) >>= assertRanOnce
    , testCase "Ref.unsafeWriteRef left unevaluated in a pair" do
        runStored (storedCase Clones inPairedWrite (inFirst readRef)) >>= assertRanOnce
    , testCase "the boxed Vector.fromList, its element cloned in both branches" do
        runStored (storedCase Clones inVector firstOfVector) >>= assertRanOnce
    , testCase "the boxed Vector.fromList left unevaluated in a pair" do
        runStored (storedCase Clones inPairedVector (inFirst firstOfVector)) >>= assertRanOnce
    , testCase "the boxed Vector.fromList left unevaluated in a pair, of a list whose first cell makes the call" do
        runStored (storedCase Clones inPairedVectorOfCell (inFirst firstOfVector)) >>= assertRanOnce
    , testCase "the multiplicity vector's fromList at One" do
        runStored (storedCase Clones inOwningVector firstOfOwningVector) >>= assertRanOnce
    , testCase "the multiplicity vector's fromList at One left unevaluated in a pair" do
        runStored (storedCase Clones inPairedOwningVector (inFirst firstOfOwningVector)) >>= assertRanOnce
    , testCase "the multiplicity vector's write at One" do
        runStored (storedCase Clones writtenToOwningVector firstOfOwningVector) >>= assertRanOnce
    , testCase "the unboxed Vector.fromList of DoNotUnboxLazy" do
        runStored (storedCase Clones inUnboxed firstOfUnboxed) >>= assertRanOnce
    , testCase "the unboxed Vector.fromList of DoNotUnboxLazy left unevaluated in a pair" do
        runStored (storedCase Clones inPairedUnboxed (inFirst firstOfUnboxed)) >>= assertRanOnce
    , testCase "the unboxed Vector.fromList of DoNotUnboxLazy left unevaluated in a pair, of a list whose first cell makes the call" do
        runStored (storedCase Clones inPairedUnboxedOfCell (inFirst firstOfUnboxed)) >>= assertRanOnce
    , testCase "the unboxed GrowableVector.fromList of DoNotUnboxLazy" do
        runStored (storedCase Clones inUnboxedGrowable firstOfUnboxedGrowable) >>= assertRanOnce
    , testCase "the unboxed GrowableVector.fromList of DoNotUnboxLazy left unevaluated in a pair" do
        runStored (storedCase Clones inPairedUnboxedGrowable (inFirst firstOfUnboxedGrowable)) >>= assertRanOnce
    , testCase "the boxed Vector.set, of a call passed to the function that writes, left unevaluated in a pair" do
        runStored (storedCase Clones (inPairedWriteBy setToVector) (inFirst firstOfVector)) >>= assertRanOnce
    , testCase "the boxed Vector.update, of a call passed to the function that writes, left unevaluated in a pair" do
        runStored (storedCase Clones (inPairedWriteBy updateInVector) (inFirst firstOfVector)) >>= assertRanOnce
    , testCase "the unboxed Vector.set, of a call passed to the function that writes, left unevaluated in a pair" do
        runStored (storedCase Clones (inPairedWriteBy setToUnboxed) (inFirst firstOfUnboxed)) >>= assertRanOnce
    , testCase "the boxed GrowableVector.push, of a call passed to the function that writes, left unevaluated in a pair" do
        runStored (storedCase Clones (inPairedWriteBy pushToGrowable) (inFirst firstOfGrowable)) >>= assertRanOnce
    , testCase "the unboxed GrowableVector.push, of a call passed to the function that writes, left unevaluated in a pair" do
        runStored (storedCase Clones (inPairedWriteBy pushToUnboxedGrowable) (inFirst firstOfUnboxedGrowable)) >>= assertRanOnce
    , testCase "the multiplicity vector's write at One, of a call passed to the function that writes, left unevaluated in a pair" do
        runStored (storedCase Clones (inPairedWriteBy writeToOwningVector) (inFirst firstOfOwningVector)) >>= assertRanOnce
    , testCase "Data.Ref.Linear.Borrow.modify, of a call passed to the function that writes, left unevaluated in a pair" do
        runStored (storedCase Clones (inPairedWriteBy modifyRef) (inFirst readRef)) >>= assertRanOnce
    , testCase "HashMap.union, looked up in both branches" do
        unionCase False
    , testCase "HashMap.union left unevaluated in a pair, looked up in both branches" do
        unionCase True
    , knownRace "storing a Share evaluates the lazy field that it points to, where nothing guards the call: the known issue in the CHANGELOG" $
        testCase "a Share of a lazy field, stored in a Ref by one branch while the other clones the field" do
          runStored storedShareCase >>= assertRanOnce
    ]

-- * Ordinary unboxed values computed from linear owners

countedIntArray :: Array Int %1 -> Array Int
{-# NOINLINE countedIntArray #-}
countedIntArray = Unsafe.toLinear \arr ->
  unsafeDupablePerformIO do
    ordinal <- atomicModifyIORef' entries \n -> (n + 1, n)
    _ <- evaluate (busy (20_000 + ordinal))
    pure arr

-- The stored type is ordinary Int, but evaluating it consumes a linear owner.
sumAfterLinearMap :: Array Int %1 -> Int
{-# NOINLINE sumAfterLinearMap #-}
sumAfterLinearMap arr = case Array.toList (Array.map (+ 1) (countedIntArray arr)) of
  Ur xs -> sum xs

setIntVector :: Int %1 -> Linearly %1 -> Unboxed.Vector Int
{-# NOINLINE setIntVector #-}
setIntVector value lin = case dup lin of
  (l1, l2) -> modifyBO_ (Unboxed.fromList [0] l1) l2 \mut -> Control.do
    (old, mut') <- Unboxed.set 0 value mut
    Control.pure (PL.consume old `PL.lseq` PL.consume mut')

updateIntVector :: Int %1 -> Linearly %1 -> Unboxed.Vector Int
{-# NOINLINE updateIntVector #-}
updateIntVector value lin = case dup lin of
  (l1, l2) -> modifyBO_ (Unboxed.fromList [0] l1) l2 \mut -> Control.do
    (old, mut') <- Unboxed.update 0 (\old -> Control.pure (old, value)) mut
    Control.pure (PL.consume old `PL.lseq` PL.consume mut')

pushIntGrowable :: Int %1 -> Linearly %1 -> UnboxedGrowable.GrowableVector Int
{-# NOINLINE pushIntGrowable #-}
pushIntGrowable value lin = case dup lin of
  (l1, l2) -> modifyBO_ (UnboxedGrowable.empty l1) l2 \mut -> Control.do
    mut' <- UnboxedGrowable.push value mut
    Control.pure (PL.consume mut')

setIntGrowable :: Int %1 -> Linearly %1 -> UnboxedGrowable.GrowableVector Int
{-# NOINLINE setIntGrowable #-}
setIntGrowable value lin = case dup lin of
  (l1, l2) -> modifyBO_ (UnboxedGrowable.fromList [0] l1) l2 \mut -> Control.do
    (old, mut') <- UnboxedGrowable.set 0 value mut
    Control.pure (PL.consume old `PL.lseq` PL.consume mut')

updateIntGrowable :: Int %1 -> Linearly %1 -> UnboxedGrowable.GrowableVector Int
{-# NOINLINE updateIntGrowable #-}
updateIntGrowable value lin = case dup lin of
  (l1, l2) -> modifyBO_ (UnboxedGrowable.fromList [0] l1) l2 \mut -> Control.do
    (old, mut') <- UnboxedGrowable.update 0 (\old -> Control.pure (old, value)) mut
    Control.pure (PL.consume old `PL.lseq` PL.consume mut')

type ReadIntOwner o = forall β. Share β o -> BO β (Ur Int)

readIntVector :: ReadIntOwner (Unboxed.Vector Int)
readIntVector = Unboxed.copyAt 0

readIntGrowable :: ReadIntOwner (UnboxedGrowable.GrowableVector Int)
readIntGrowable = UnboxedGrowable.copyAt 0

intBranch :: IORef Int -> ReadIntOwner o -> Share β (o, ()) -> BO β (Ur Int)
intBranch arrivals readOwner sh = Control.do
  arriveAndWait arrivals
  case splitPair sh of
    (owner, _) -> readOwner owner

intInBoth :: ReadIntOwner o -> Mut α (o, ()) %1 -> BO α (Ur (Int, Int))
intInBoth readOwner mut = Control.do
  Ur arrivals <- newArrivals
  (r, restored) <- sharing mut \sh -> Control.do
    (Ur x, Ur y) <- parBO (intBranch arrivals readOwner sh) (intBranch arrivals readOwner sh)
    Control.pure (Ur (x, y))
  Control.pure (restored `PL.lseq` r)

intInParent :: ReadIntOwner o -> Mut α (o, ()) %1 -> BO α (Ur Int)
intInParent readOwner mut = case share mut of
  Ur sh -> case splitPair sh of
    (owner, _) -> readOwner owner

storedIntCase ::
  (PL.Consumable o) =>
  (Int %1 -> Linearly %1 -> o) ->
  ReadIntOwner o ->
  Array Int %1 ->
  Linearly %1 ->
  Ur (Int, Int, Int)
{-# NOINLINE storedIntCase #-}
storedIntCase writeOwner readOwner arr0 lin = case Array.size arr0 of
  (Ur _, arr) -> case dup3 lin of
    (l1, l2, l3) -> case modifyBO (writeOwner (sumAfterLinearMap arr) l1, ()) l2 (intInBoth readOwner) of
      (Ur (x, y), owner) -> case modifyBO owner l3 (intInParent readOwner) of
        (Ur z, owner') -> owner' `PL.lseq` Ur (x, y, z)

runStoredInt ::
  (PL.Consumable o) =>
  (Int %1 -> Linearly %1 -> o) ->
  ReadIntOwner o ->
  Assertion
runStoredInt writeOwner readOwner = race do
  results <- forM [1 .. 200] \n -> do
    writeIORef entries 0
    let seed = n * 10_000
        expected = sum [seed + 1 .. seed + elements]
    result <- evaluate (force (PL.unur (Array.fromList [seed .. seed + elements - 1] \arr -> linearly (storedIntCase writeOwner readOwner arr))))
    times <- readIORef entries
    pure (times, result, expected)
  [(times, result, expected) | (times, result, expected) <- results, times /= 1 || result /= (expected, expected, expected)] @?= []

test_storedInt :: TestTree
test_storedInt =
  testGroup
    "an ordinary Int computed by consuming an owner is evaluated inside the run guard"
    [ testCase "unboxed set" (runStoredInt setIntVector readIntVector)
    , testCase "unboxed update" (runStoredInt updateIntVector readIntVector)
    , testCase "unboxed growable push" (runStoredInt pushIntGrowable readIntGrowable)
    , testCase "unboxed growable set" (runStoredInt setIntGrowable readIntGrowable)
    , testCase "unboxed growable update" (runStoredInt updateIntGrowable readIntGrowable)
    ]
