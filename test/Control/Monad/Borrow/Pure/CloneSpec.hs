{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Borrow.Pure.CloneSpec (
  module Control.Monad.Borrow.Pure.CloneSpec,
) where

import Control.Exception (TypeError, displayException, evaluate, try)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias))
import Control.Monad.Borrow.Pure.Clone.TypingCases
import Control.Monad.Borrow.Pure.CloneSpec.ArrayLoops qualified as ArrayLoops
import Data.Array.Mutable.Linear (Array)
import Data.Array.Mutable.Linear qualified as LA
import Data.Complex (Complex)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.HashMap.Mutable.Linear qualified as LH
import Data.HashMap.RobinHood.Mutable.Linear qualified as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Monoid qualified as Monoid
import Data.Ord (Down)
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import Data.Semigroup qualified as Sem
import Data.Set.Mutable.Linear qualified as LS
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as VG
import Data.Vector.Mutable.Linear qualified as LV
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow qualified as UG
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as UV
import Prelude.Linear
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

-- These must keep compiling: each is the instance a record with such a field needs to derive 'Clone' generically.

cloneUr :: Share α (Ur Int) %1 -> BO α (Ur Int)
cloneUr = clone

cloneSum :: Share α (Sum Int) %1 -> BO α (Sum Int)
cloneSum = clone

cloneProduct :: Share α (Product Int) %1 -> BO α (Product Int)
cloneProduct = clone

cloneMin :: Share α (Sem.Min Int) %1 -> BO α (Sem.Min Int)
cloneMin = clone

cloneMax :: Share α (Sem.Max Int) %1 -> BO α (Sem.Max Int)
cloneMax = clone

cloneArg :: Share α (Sem.Arg Int Int) %1 -> BO α (Sem.Arg Int Int)
cloneArg = clone

cloneComplex :: Share α (Complex Double) %1 -> BO α (Complex Double)
cloneComplex = clone

-- These must keep compiling too: the array's instance reaches a container of arrays, and a newtype over an array derives its own.

cloneVectorOfArrays :: Share α (VL.Vector (Array Int)) %1 -> BO α (VL.Vector (Array Int))
cloneVectorOfArrays = clone

-- | A user's type built on an array.
newtype Buffer = Buffer (Array Int)
  deriving newtype (Clone)

cloneBuffer :: Share α Buffer %1 -> BO α Buffer
cloneBuffer = clone

-- | An array of values with no instance of any class: an array's clone requires nothing of its elements.
cloneFunctions :: Share α (Array (Int -> Int)) %1 -> BO α (Array (Int -> Int))
cloneFunctions = clone

{- | Write 7 at index 0 of the clone, and only then read the original.

The write must be forced first: a read of the original made before it would see @[1, 2]@ even through a clone that shared the original's storage.
-}
writeThenReadBoth :: Array Int %1 -> Array Int %1 -> ([Int], [Int])
writeThenReadBoth original cloned = case LA.toList (LA.set 0 7 cloned) of
  Ur inClone -> case LA.toList original of
    Ur inOriginal -> (inOriginal, inClone)

{- | Clone a shared array of @[1, 2]@, write to the clone, and read both arrays.

Expected @([1, 2], [7, 2])@: a clone that shared the original's storage would give @([7, 2], [7, 2])@.
The write happens inside the lifetime, while the original is still borrowed, and the bind forces it before the original is read.
-}
writeToArrayClone :: ([Int], [Int])
{-# NOINLINE writeToArrayClone #-}
writeToArrayClone = unur do
  LA.fromList [1, 2 :: Int] \arr -> move do
    linearly \lin -> runBO lin Control.do
      (borrowed, lend) <- borrowM arr
      Ur shared <- Control.pure (share borrowed)
      cloned <- clone shared
      Ur inClone <- Control.pure (LA.toList (LA.set 0 7 cloned))
      pureAfter case LA.toList (reclaim lend) of
        Ur inOriginal -> (inOriginal, inClone)

{- | Clone a shared array of @[1, 2]@, write 9 into the original once it is reclaimed, and only then read the clone.

Expected @([9, 2], [1, 2])@: the copy must be complete inside 'clone', before the lifetime ends.
A clone left as an unevaluated copy would copy the original only when read, after the write, and give @[9, 2]@.
The clone is read lazily, in the second component, so that forcing the pair does not read it before the write.
-}
cloneBeforeLaterWrite :: ([Int], [Int])
{-# NOINLINE cloneBeforeLaterWrite #-}
cloneBeforeLaterWrite = unur do
  LA.fromList [1, 2 :: Int] \arr -> move do
    linearly \lin -> runBO lin Control.do
      (borrowed, lend) <- borrowM arr
      Ur shared <- Control.pure (share borrowed)
      cloned <- clone shared
      pureAfter case LA.toList (LA.set 0 9 (reclaim lend)) of
        Ur inOriginal -> (inOriginal, unur (LA.toList cloned))

-- | 'cloneBeforeLaterWrite' through a reference to the array, whose 'Clone' clones its contents with the array's.
refCloneBeforeLaterWrite :: ([Int], [Int])
{-# NOINLINE refCloneBeforeLaterWrite #-}
refCloneBeforeLaterWrite = unur do
  LA.fromList [1, 2 :: Int] \arr -> move do
    linearly \lin -> runBO lin Control.do
      ref <- asksLinearly (Ref.new arr)
      (borrowed, lend) <- borrowM ref
      Ur shared <- Control.pure (share borrowed)
      cloned <- clone shared
      pureAfter case LA.toList (LA.set 0 9 (Ref.free (reclaim lend))) of
        Ur inOriginal -> (inOriginal, unur (LA.toList (Ref.free cloned)))

{- | Clone one evaluated array of @[0, 0]@ in both branches of a 'parBO', write each branch's number into its clone, and read the clones and the original.

Expected @([1, 0], [2, 0], [0, 0])@: the clones are arrays of their own, and the original is only read.
-}
parallelArrayClones :: ([Int], [Int], [Int])
{-# NOINLINE parallelArrayClones #-}
parallelArrayClones = unur do
  LA.alloc 2 (0 :: Int) \arr -> move do
    linearly \lin -> runBO lin Control.do
      (borrowed, lend) <- borrowM arr
      Ur shared <- Control.pure (share borrowed)
      (Ur first, Ur second) <- parBO (cloneAndWrite 1 shared) (cloneAndWrite 2 shared)
      pureAfter case LA.toList (reclaim lend) of
        Ur original -> (first, second, original)
  where
    cloneAndWrite :: Int -> Share α (Array Int) -> BO α (Ur [Int])
    cloneAndWrite k shared = Control.do
      cloned <- clone shared
      Control.pure (LA.toList (LA.set 0 k cloned))

{- | Clone a reference to an array of @[1, 2]@ through a shared borrow, then write to the clone's array and read both.

Expected @([1, 2], [7, 2])@: 'Clone' of a 'Ref' clones its contents, here through the array's instance.
-}
cloneRefOfArray :: ([Int], [Int])
{-# NOINLINE cloneRefOfArray #-}
cloneRefOfArray = unur do
  LA.fromList [1, 2 :: Int] \arr -> move do
    linearly \lin -> runBO lin Control.do
      ref <- asksLinearly (Ref.new arr)
      (borrowed, lend) <- borrowM ref
      Ur shared <- Control.pure (share borrowed)
      cloned <- clone shared
      pureAfter (writeThenReadBoth (Ref.free (reclaim lend)) (Ref.free cloned))

{- | Clone a shared empty array, and read the sizes of the clone and the original.

Expected @(0, 0)@: the instance checks that its copy is not the original, and a runtime that shared one empty array among all would fail that check here.
-}
cloneEmptyArray :: (Int, Int)
{-# NOINLINE cloneEmptyArray #-}
cloneEmptyArray = unur do
  LA.alloc 0 (0 :: Int) \arr -> move do
    linearly \lin -> runBO lin Control.do
      (borrowed, lend) <- borrowM arr
      Ur shared <- Control.pure (share borrowed)
      cloned <- clone shared
      pureAfter case (LA.size cloned, LA.size (reclaim lend)) of
        ((Ur inClone, cloned'), (Ur inOriginal, original')) -> cloned' `lseq` original' `lseq` (inClone, inOriginal)

clonedUr :: (Int, Int)
{-# NOINLINE clonedUr #-}
clonedUr = linearly \lin -> runBO lin Control.do
  (borrowed, lend) <- borrowM (Ur (41 :: Int))
  let !(Ur shared) = share borrowed
  Ur copied <- cloneUr shared
  pureAfter (case reclaim lend of Ur original -> (original, copied))

{- | A value that logs its identity when it is consumed, and whose clone is a fresh value.

Cloning a container of them must leave the originals to their owner, so that every identity is consumed exactly once.
-}
data Tracked = Tracked (IORef [Int]) (IORef Int) Int

logConsumed :: IORef [Int] -> Int -> ()
{-# NOINLINE logConsumed #-}
logConsumed consumed identity = unsafePerformIO (atomicModifyIORef' consumed \ids -> (identity : ids, ()))

-- | A fresh value; the tag keeps two calls with the same references from being merged.
freshTracked :: Int -> IORef [Int] -> IORef Int -> Tracked
{-# NOINLINE freshTracked #-}
freshTracked tag consumed counter =
  unsafePerformIO do
    identity <- atomicModifyIORef' counter \next -> (next + 1, next)
    NonLinear.pure (Tracked consumed counter (identity NonLinear.+ 0 NonLinear.* tag))

instance Consumable Tracked where
  consume = Unsafe.toLinear \(Tracked consumed _ identity) -> logConsumed consumed identity
  {-# NOINLINE consume #-}

instance Clone Tracked where
  clone = Unsafe.toLinear \(UnsafeAlias (Tracked consumed counter _)) ->
    Control.pure $! freshTracked 1 consumed counter
  {-# NOINLINE clone #-}

-- | Clone a container of 'Tracked' values through a shared borrow, consume the original and the clone, and return the sorted identities created and consumed.
cloneAccount ::
  (Consumable (f Tracked), Clone (f Tracked)) =>
  (IORef [Int] -> IORef Int -> Linearly %1 -> f Tracked) ->
  ([Int], [Int])
{-# NOINLINE cloneAccount #-}
cloneAccount build = unsafePerformIO do
  consumed <- newIORef []
  counter <- newIORef 0
  () <- evaluate $ linearly \lin -> runBO lin Control.do
    container <- asksLinearly (build consumed counter)
    (borrowed, lend) <- borrowM container
    let !(Ur shared) = share borrowed
    cloned <- clone shared
    pureAfter (consume (reclaim lend) `lseq` consume cloned)
  created <- readIORef counter
  ids <- readIORef consumed
  NonLinear.pure ([0 .. created NonLinear.- 1], List.sort ids)

refAccount, vectorAccount :: ([Int], [Int])
{-# NOINLINE refAccount #-}
{-# NOINLINE vectorAccount #-}
refAccount = cloneAccount \consumed counter -> Ref.new (freshTracked 0 consumed counter)
vectorAccount = cloneAccount \consumed counter ->
  VL.fromList [freshTracked 0 consumed counter, freshTracked 3 consumed counter]

{- | Clone a reference to a reference through a shared borrow, overwrite the inner reference of the clone, and read the inner reference of the original through a shared borrow of it taken before the clone.

Returns what that read saw and what was written.
-}
cloneThenOverwrite :: (Int, Int)
{-# NOINLINE cloneThenOverwrite #-}
cloneThenOverwrite = linearly \lin -> runBO lin Control.do
  outer <- asksLinearly \l ->
    dup2 l & \(l1, l2) -> Ref.new (Ref.new (0 :: Int) l1) l2
  (borrowed, lend) <- borrowM outer
  let !(Ur shared) = share borrowed
  Ur innerShared <- RefBorrow.readShare shared
  cloned <- clone shared
  let !written = Ref.free (Ref.atomicModify_ (\old -> old `lseq` 42) (Ref.free cloned))
  seen <- RefBorrow.copyRef innerShared
  pureAfter (consume (reclaim lend) `lseq` (seen, written))

{- | Clone a vector of references through a shared borrow, bump the first reference of the original afterwards, and read the first reference of each.

A clone is a snapshot, so this returns @(2, 1)@.
-}
cloneSnapshot :: (Int, Int)
{-# NOINLINE cloneSnapshot #-}
cloneSnapshot = linearly \lin -> runBO lin Control.do
  source <- asksLinearly \l ->
    dup2 l & \(l1, l2) -> VL.fromList [Ref.new (1 :: Int) l1] l2
  (borrowed, lend) <- borrowM source
  (cloned, borrowed') <- sharing borrowed \shared -> clone shared
  element <- VL.get 0 borrowed'
  bumped <- RefBorrow.modify (+ 1) element
  inOriginal <- RefBorrow.copyRef bumped
  (clonedBorrow, clonedLend) <- borrowM cloned
  clonedElement <- VL.get 0 clonedBorrow
  inClone <- RefBorrow.copyRef clonedElement
  pureAfter (consume (reclaim lend) `lseq` consume (reclaim clonedLend) `lseq` (inOriginal, inClone))

-- | An owned hash map holding @1 ↦ 0@.
mapOfOne :: Linearly %1 -> HM.HashMap Int Int
mapOfOne l = case HM.insert 1 0 (HM.new 64 l) of
  (Ur _, table) -> table

-- | Look key 2 up in a clone, then insert @k@ at it.
readThenWrite :: Int -> HM.HashMap Int Int %1 -> Ur (Maybe Int)
readThenWrite k table = case HM.lookup 2 table of
  (Ur found, looked) -> case HM.insert 2 k looked of
    (Ur _, inserted) -> consume inserted `lseq` Ur found

{- | Clone one shared hash map in each iteration of a loop that closes over the shared borrow, and in each clone read key 2 and then write it.

Every read must find nothing.
While the copy of a table was a pure function of the table alone, GHC shared one copy among the clones of the loop, and each iteration read what the previous one had written.
-}
loopClones :: Int -> [Maybe Int]
{-# NOINLINE loopClones #-}
loopClones n = linearly \lin -> runBO lin Control.do
  table <- asksLinearly mapOfOne
  (borrowed, lend) <- borrowM table
  Ur shared <- Control.pure (share borrowed)
  let go k
        | k > n = Control.pure []
        | otherwise = Control.do
            cloned <- clone shared
            Ur found <- Control.pure (readThenWrite k cloned)
            rest <- go (k + 1)
            Control.pure (found : rest)
  founds <- go 1
  pureAfter (consume (reclaim lend) `lseq` founds)

-- | Insert 999 at key 1 of the first table, then look key 1 up in both.
writeThenRead :: HM.HashMap Int Int %1 -> HM.HashMap Int Int %1 -> (Maybe Int, Maybe Int)
writeThenRead first second = case HM.insert 1 999 first of
  (Ur _, written) -> case HM.lookup 1 written of
    (Ur r1, first') -> case HM.lookup 1 second of
      (Ur r2, second') -> consume first' `lseq` consume second' `lseq` (r1, r2)

{- | Collect two clones from a loop over one shared hash map, write through the first and read through the second.

Expected @(Just 999, Just 0)@: two owned tables with one slot array would give @(Just 999, Just 999)@.
-}
twoLiveFromLoop :: (Maybe Int, Maybe Int)
{-# NOINLINE twoLiveFromLoop #-}
twoLiveFromLoop = linearly \lin -> runBO lin Control.do
  table <- asksLinearly mapOfOne
  (borrowed, lend) <- borrowM table
  Ur shared <- Control.pure (share borrowed)
  let go k
        | k > (2 :: Int) = Control.pure []
        | otherwise = Control.do
            cloned <- clone shared
            rest <- go (k + 1)
            Control.pure (cloned : rest)
  clones <- go 1
  pureAfter (consume (reclaim lend) `lseq` firstTwo clones)
  where
    firstTwo :: [HM.HashMap Int Int] %1 -> (Maybe Int, Maybe Int)
    firstTwo [first, second] = writeThenRead first second
    firstTwo others = consume others `lseq` (Nothing, Nothing)

test_clone :: TestTree
test_clone =
  testGroup
    "Clone instances"
    [ testCase "cloning an Ur shares its GC-owned payload" do
        clonedUr @?= (41, 41)
    , testCase "cloning a Ref leaves the original to its owner" do
        let (created, consumed) = refAccount
        consumed @?= created
    , testCase "cloning a boxed Vector leaves the original to its owner" do
        let (created, consumed) = vectorAccount
        consumed @?= created
    , testCase "a clone of a Ref shares no reference with the original" do
        cloneThenOverwrite @?= (0, 42)
    , testCase "a clone of a vector of references does not see later writes to the original" do
        cloneSnapshot @?= (2, 1)
    , testCase "clones of one hash map taken in a loop are tables of their own" do
        loopClones 5 @?= [Nothing, Nothing, Nothing, Nothing, Nothing]
    , testCase "two clones of one hash map collected from a loop share no slot array" do
        twoLiveFromLoop @?= (Just 999, Just 0)
    , testCase "writing to a clone of an Array leaves the original unchanged" do
        writeToArrayClone @?= ([1, 2], [7, 2])
    , testCase "both branches of a parBO clone one Array into arrays of their own" do
        parallelArrayClones @?= ([1, 0], [2, 0], [0, 0])
    , testCase "a clone of an Array is complete before a later write to the original" do
        cloneBeforeLaterWrite @?= ([9, 2], [1, 2])
    , testCase "a clone of a Ref of an Array is complete before a later write to the original" do
        refCloneBeforeLaterWrite @?= ([9, 2], [1, 2])
    , testCase "copy of a shared Array is rejected, and the message points to clone" do
        assertDeferredTypeError "clone a shared borrow of it inside BO" copyOfSharedArray
    , testCase "copy of a shared linear-base Vector is rejected" do
        assertDeferredTypeError "clone a shared borrow of it inside BO" copyOfSharedVector
    , testCase "a clone of a Ref of an Array holds an array of its own" do
        cloneRefOfArray @?= ([1, 2], [7, 2])
    , testCase "an empty Array can be cloned" do
        cloneEmptyArray @?= (0, 0)
    , testCase "clones of one Array taken in a loop are arrays of their own" do
        ArrayLoops.loopArrayClones 5 @?= [0, 0, 0, 0, 0]
    , testCase "two clones of one Array collected from a loop share no array" do
        ArrayLoops.twoArrayClonesFromLoop @?= (999, 0)
    , testCase "two clones of one Ref of an Array collected from a loop share no array" do
        ArrayLoops.twoRefOfArrayClonesFromLoop @?= (999, 0)
    , testCase "a Ref cannot be cloned when its contents are Dupable but not Clone" do
        assertDeferredTypeError "Clone DupableOnly" refOfDupableOnly
    , testCase "a boxed Vector cannot be cloned when its elements are Dupable but not Clone" do
        assertDeferredTypeError "Clone DupableOnly" vectorOfDupableOnly
    ]

{- | The example of a hand-written instance in the header of "Control.Monad.Borrow.Pure.Clone" is the library's instance for arrays, line for line, so that the example compiles and does what the tests above check.

It reads the source, relative to the package directory, where @cabal test@ runs the suite.
-}
test_cloneExample :: TestTree
test_cloneExample =
  testCase "the hand-written example in the Clone documentation is the library's instance" do
    source <- NonLinear.lines NonLinear.<$> NonLinear.readFile "src/Control/Monad/Borrow/Pure/Clone.hs"
    let example =
          NonLinear.map untrack
            NonLinear.. NonLinear.takeWhile (List.isPrefixOf ">")
            NonLinear.$ NonLinear.dropWhile (NonLinear./= "> instance Clone (Array a) where") source
        code =
          NonLinear.take (NonLinear.length example) NonLinear.$
            NonLinear.dropWhile (NonLinear./= "instance Clone (Array a) where") source
    assertBool "the example is missing" (NonLinear.length example NonLinear.> 10)
    example @?= code
  where
    untrack :: NonLinear.String -> NonLinear.String
    untrack ('>' : ' ' : rest) = rest
    untrack ">" = ""
    untrack other = other

-- | Force a value that must be a deferred type error, catching only a 'TypeError', and check its message.
assertDeferredTypeError :: NonLinear.String -> a -> Assertion
assertDeferredTypeError expectedFragment value = do
  result <- try @TypeError (evaluate value)
  case result of
    Left exception ->
      assertBool
        ("unexpected deferred error: " <> displayException exception)
        (expectedFragment `List.isInfixOf` displayException exception)
    Right _ ->
      assertFailure ("expected a deferred type error containing " <> expectedFragment)

-- | Two parallel copies and a snapshot must own storage independent of the original and each other.
cloneFamily ::
  forall o.
  (Clone o) =>
  (Linearly %1 -> o) ->
  (Int -> o %1 -> Linearly %1 -> o) ->
  (o %1 -> Ur [Int]) ->
  ([Int], [Int], [Int], [Int])
{-# NOINLINE cloneFamily #-}
cloneFamily build writeOwner contents = linearly \lin -> runBO lin Control.do
  original <- asksLinearly build
  (mut, lend) <- borrowM original
  Ur shared <- Control.pure (share mut)
  (Ur first, Ur second) <- parBO (branch 7 shared) (branch 8 shared)
  snapshot <- clone shared
  token <- askLinearly
  pureAfter case contents (writeOwner 9 (reclaim lend) token) of
    Ur changed -> (first, second, changed, unur (contents snapshot))
  where
    branch :: Int -> Share α o -> BO α (Ur [Int])
    branch k shared = Control.do
      copied <- clone shared
      token <- askLinearly
      Control.pure $! contents (writeOwner k copied token)

boxedGrowableWrite :: Int -> VG.GrowableVector Int %1 -> Linearly %1 -> VG.GrowableVector Int
boxedGrowableWrite value owner token = modifyBO_ owner token \mut -> Control.do
  (old, next) <- VG.set 0 value mut
  Control.pure (consume old `lseq` consume next)

boxedGrowableClones :: ([Int], [Int], [Int], [Int])
{-# NOINLINE boxedGrowableClones #-}
boxedGrowableClones = cloneFamily (VG.fromList [1, 2]) boxedGrowableWrite VG.toList

unboxedFixedWrite :: Int -> UV.Vector Int %1 -> Linearly %1 -> UV.Vector Int
unboxedFixedWrite value owner token = modifyBO_ owner token \mut -> Control.do
  (old, next) <- UV.set 0 value mut
  Control.pure (consume old `lseq` consume next)

unboxedFixedClones :: ([Int], [Int], [Int], [Int])
{-# NOINLINE unboxedFixedClones #-}
unboxedFixedClones = cloneFamily (UV.fromList [1, 2]) unboxedFixedWrite UV.toList

unboxedGrowableWrite :: Int -> UG.GrowableVector Int %1 -> Linearly %1 -> UG.GrowableVector Int
unboxedGrowableWrite value owner token = modifyBO_ owner token \mut -> Control.do
  (old, next) <- UG.set 0 value mut
  Control.pure (consume old `lseq` consume next)

unboxedGrowableClones :: ([Int], [Int], [Int], [Int])
{-# NOINLINE unboxedGrowableClones #-}
unboxedGrowableClones = cloneFamily (UG.fromList [1, 2]) unboxedGrowableWrite UG.toList

-- A boxed Unbox representation can own a Ref, and must clone that Ref deeply.
newtype DeepRef = DeepRef (Ref.Ref Int)
  deriving newtype (Clone, Consumable)

instance Clone (U.DoNotUnboxStrict DeepRef) where
  clone = Unsafe.toLinear \(UnsafeAlias (U.DoNotUnboxStrict value)) ->
    U.DoNotUnboxStrict Control.<$> clone (UnsafeAlias value)

instance Consumable (U.DoNotUnboxStrict DeepRef) where
  consume (U.DoNotUnboxStrict value) = consume value
  {-# NOINLINE consume #-}

unwrapDeepRef :: Mut α (U.DoNotUnboxStrict DeepRef) %1 -> Mut α (Ref.Ref Int)
unwrapDeepRef = Unsafe.toLinear \(UnsafeAlias (U.DoNotUnboxStrict (DeepRef ref))) -> UnsafeAlias ref

deepCloneFamily ::
  (Clone o, Consumable o) =>
  (Ref.Ref Int %1 -> Linearly %1 -> o) ->
  (forall α. Mut α o %1 -> BO α (Mut α (Ref.Ref Int))) ->
  (Int, Int)
{-# NOINLINE deepCloneFamily #-}
deepCloneFamily build element = linearly \lin -> runBO lin Control.do
  ref <- asksLinearly (Ref.new 1)
  original <- asksLinearly (build ref)
  (mut, lend) <- borrowM original
  (copied, restored) <- sharing mut (\shared -> clone shared)
  originalRef <- element restored
  bumped <- RefBorrow.modify (+ 1) originalRef
  before <- RefBorrow.copyRef bumped
  (copiedMut, copiedLend) <- borrowM copied
  copiedRef <- element copiedMut
  after <- RefBorrow.copyRef copiedRef
  pureAfter (consume (reclaim lend) `lseq` consume (reclaim copiedLend) `lseq` (before, after))

buildBoxedDeep :: Ref.Ref Int %1 -> Linearly %1 -> VG.GrowableVector (Ref.Ref Int)
buildBoxedDeep ref token = case dup2 token of
  (allocate, run) -> modifyBO_ (VG.empty allocate) run \mut ->
    consume Control.<$> VG.push ref mut

boxedGrowableDeep :: (Int, Int)
boxedGrowableDeep = deepCloneFamily buildBoxedDeep (VG.get 0)

unboxedFixedDeep :: (Int, Int)
unboxedFixedDeep =
  deepCloneFamily
    (\ref -> UV.fromList [U.DoNotUnboxStrict (DeepRef ref)])
    (\mut -> unwrapDeepRef Control.<$> UV.get 0 mut)

unboxedGrowableDeep :: (Int, Int)
unboxedGrowableDeep =
  deepCloneFamily
    (\ref -> UG.fromList [U.DoNotUnboxStrict (DeepRef ref)])
    (\mut -> unwrapDeepRef Control.<$> UG.get 0 mut)

boxedGrowableCapacity :: (Int, Int, Int)
boxedGrowableCapacity = linearly \lin -> runBO lin Control.do
  owner <- asksLinearly (VG.fromList [1, 2 :: Int])
  (mut, lend) <- borrowM owner
  reserved <- VG.reserve 32 mut
  Ur shared <- Control.pure (share reserved)
  (Ur originalCapacity, originalBorrow) <- VG.capacity shared
  copied <- clone originalBorrow
  (copiedMut, copiedLend) <- borrowM copied
  (Ur copiedCapacity, copiedBorrow) <- VG.capacity copiedMut
  (Ur copiedSize, sizedBorrow) <- VG.size copiedBorrow
  let !() = consume sizedBorrow
  pureAfter (consume (reclaim lend) `lseq` consume (reclaim copiedLend) `lseq` (originalCapacity, copiedCapacity, copiedSize))

unboxedGrowableCapacity :: (Int, Int, Int)
unboxedGrowableCapacity = linearly \lin -> runBO lin Control.do
  owner <- asksLinearly (UG.fromList [1, 2 :: Int])
  (mut, lend) <- borrowM owner
  reserved <- UG.reserve 32 mut
  Ur shared <- Control.pure (share reserved)
  (Ur originalCapacity, originalBorrow) <- UG.capacity shared
  copied <- clone originalBorrow
  (copiedMut, copiedLend) <- borrowM copied
  (Ur copiedCapacity, copiedBorrow) <- UG.capacity copiedMut
  (Ur copiedSize, sizedBorrow) <- UG.size copiedBorrow
  let !() = consume sizedBorrow
  pureAfter (consume (reclaim lend) `lseq` consume (reclaim copiedLend) `lseq` (originalCapacity, copiedCapacity, copiedSize))

-- These types pin linear-base's GC-owned element boundary and require no Clone for the contents.
linearVectorGet :: LV.Vector a %1 -> (Ur a, LV.Vector a)
linearVectorGet = LV.get 0

linearVectorSet :: a -> LV.Vector a %1 -> LV.Vector a
linearVectorSet = LV.set 0

linearMapInsert :: Int -> a -> LH.HashMap Int a %1 -> LH.HashMap Int a
linearMapInsert = LH.insert

linearMapLookup :: Int -> LH.HashMap Int a %1 -> (Ur (Maybe a), LH.HashMap Int a)
linearMapLookup = LH.lookup

linearSetInsert :: Int -> LS.Set Int %1 -> LS.Set Int
linearSetInsert = LS.insert

linearSetContents :: LS.Set Int %1 -> Ur [Int]
linearSetContents = LS.toList

cloneLinearFunctions :: Share α (LV.Vector (Int -> Int)) %1 -> BO α (LV.Vector (Int -> Int))
cloneLinearFunctions = clone

cloneMapFunctions :: Share α (LH.HashMap Int (Int -> Int)) %1 -> BO α (LH.HashMap Int (Int -> Int))
cloneMapFunctions = clone

cloneSetFunctions :: Share α (LS.Set (Int -> Int)) %1 -> BO α (LS.Set (Int -> Int))
cloneSetFunctions = clone

-- Exercise storage growth and shrinking in linear-base after cloning.
linearVectorClones :: ([Int], [Int], [Int])
linearVectorClones = unur $ LV.fromList [1, 2 :: Int] \owner -> move $
  linearly \lin -> runBO lin Control.do
    (mut, lend) <- borrowM owner
    Ur shared <- Control.pure (share mut)
    (copied, second) <- parBO (clone shared) (clone shared)
    Ur changed <- Control.pure (LV.toList (LV.shrinkToFit (LV.push 4 (LV.push 3 (LV.set 0 7 copied)))))
    pureAfter (changed, unur (LV.toList second), unur (LV.toList (reclaim lend)))

linearMapClones :: ([(Int, Int)], [(Int, Int)], [(Int, Int)])
linearMapClones = unur $ LH.fromList [(1, 2 :: Int)] \owner -> move $
  linearly \lin -> runBO lin Control.do
    (mut, lend) <- borrowM owner
    Ur shared <- Control.pure (share mut)
    (copied, second) <- parBO (clone shared) (clone shared)
    Ur changed <- Control.pure (LH.toList (LH.shrinkToFit (insertMany 40 copied)))
    pureAfter (List.sort changed, unur (LH.toList second), unur (LH.toList (reclaim lend)))
  where
    insertMany :: Int -> LH.HashMap Int Int %1 -> LH.HashMap Int Int
    insertMany 0 table = table
    insertMany n table = insertMany (n - 1) (LH.insert n n table)

linearSetClones :: ([Int], [Int], [Int])
linearSetClones = unur $ LS.fromList [1 :: Int] \owner -> move $
  linearly \lin -> runBO lin Control.do
    (mut, lend) <- borrowM owner
    Ur shared <- Control.pure (share mut)
    (copied, second) <- parBO (clone shared) (clone shared)
    Ur changed <- Control.pure (LS.toList (insertMany 40 copied))
    pureAfter (List.sort changed, unur (LS.toList second), unur (LS.toList (reclaim lend)))
  where
    insertMany :: Int -> LS.Set Int %1 -> LS.Set Int
    insertMany 0 table = table
    insertMany n table = insertMany (n - 1) (LS.insert n table)

cloneWrapper0 :: Share α (Identity (Ref.Ref Int)) %1 -> BO α (Identity (Ref.Ref Int))
cloneWrapper0 = clone

copyWrapper0 :: Share α (Identity (Int)) %1 -> Identity (Int)
copyWrapper0 = copy

cloneWrapper1 :: Share α (Down (Ref.Ref Int)) %1 -> BO α (Down (Ref.Ref Int))
cloneWrapper1 = clone

copyWrapper1 :: Share α (Down (Int)) %1 -> Down (Int)
copyWrapper1 = copy

cloneWrapper2 :: Share α (Const (Ref.Ref Int) Bool) %1 -> BO α (Const (Ref.Ref Int) Bool)
cloneWrapper2 = clone

copyWrapper2 :: Share α (Const (Int) Bool) %1 -> Const (Int) Bool
copyWrapper2 = copy

cloneWrapper3 :: Share α (Sem.Dual (Ref.Ref Int)) %1 -> BO α (Sem.Dual (Ref.Ref Int))
cloneWrapper3 = clone

copyWrapper3 :: Share α (Sem.Dual (Int)) %1 -> Sem.Dual (Int)
copyWrapper3 = copy

cloneWrapper4 :: Share α (Sem.First (Ref.Ref Int)) %1 -> BO α (Sem.First (Ref.Ref Int))
cloneWrapper4 = clone

copyWrapper4 :: Share α (Sem.First (Int)) %1 -> Sem.First (Int)
copyWrapper4 = copy

cloneWrapper5 :: Share α (Sem.Last (Ref.Ref Int)) %1 -> BO α (Sem.Last (Ref.Ref Int))
cloneWrapper5 = clone

copyWrapper5 :: Share α (Sem.Last (Int)) %1 -> Sem.Last (Int)
copyWrapper5 = copy

cloneWrapper6 :: Share α (Sem.WrappedMonoid (Ref.Ref Int)) %1 -> BO α (Sem.WrappedMonoid (Ref.Ref Int))
cloneWrapper6 = clone

copyWrapper6 :: Share α (Sem.WrappedMonoid (Int)) %1 -> Sem.WrappedMonoid (Int)
copyWrapper6 = copy

cloneWrapper7 :: Share α (Monoid.Alt Identity (Ref.Ref Int)) %1 -> BO α (Monoid.Alt Identity (Ref.Ref Int))
cloneWrapper7 = clone

copyWrapper7 :: Share α (Monoid.Alt Identity (Int)) %1 -> Monoid.Alt Identity (Int)
copyWrapper7 = copy

cloneTuple6 :: Share α (Int, Int, Int, Int, Int, Ref.Ref Int) %1 -> BO α (Int, Int, Int, Int, Int, Ref.Ref Int)
cloneTuple6 = clone

copyTuple5 :: Share α (Int, Int, Int, Int, Int) %1 -> (Int, Int, Int, Int, Int)
copyTuple5 = copy

copyTuple6 :: Share α (Int, Int, Int, Int, Int, Int) %1 -> (Int, Int, Int, Int, Int, Int)
copyTuple6 = copy

cloneBools :: Share α (Monoid.Any, Monoid.All) %1 -> BO α (Monoid.Any, Monoid.All)
cloneBools = clone

sortedUr :: (NonLinear.Ord a) => Ur [a] %1 -> [a]
sortedUr (Ur values) = List.sort values

linearVectorSnapshot :: ([Int], [Int])
{-# NOINLINE linearVectorSnapshot #-}
linearVectorSnapshot = unur $ LV.fromList [1, 2 :: Int] \owner -> move $
  linearly \lin -> runBO lin Control.do
    (mut, lend) <- borrowM owner
    Ur shared <- Control.pure (share mut)
    snapshot <- clone shared
    pureAfter case LV.toList (LV.set 0 9 (reclaim lend)) of
      Ur changed -> (changed, unur (LV.toList snapshot))

linearMapSnapshot :: ([(Int, Int)], [(Int, Int)])
{-# NOINLINE linearMapSnapshot #-}
linearMapSnapshot = unur $ LH.fromList [(1, 2 :: Int)] \owner -> move $
  linearly \lin -> runBO lin Control.do
    (mut, lend) <- borrowM owner
    Ur shared <- Control.pure (share mut)
    snapshot <- clone shared
    pureAfter case LH.toList (LH.insert 1 9 (reclaim lend)) of
      Ur changed -> (List.sort changed, sortedUr (LH.toList snapshot))

linearSetSnapshot :: ([Int], [Int])
{-# NOINLINE linearSetSnapshot #-}
linearSetSnapshot = unur $ LS.fromList [1 :: Int] \owner -> move $
  linearly \lin -> runBO lin Control.do
    (mut, lend) <- borrowM owner
    Ur shared <- Control.pure (share mut)
    snapshot <- clone shared
    pureAfter case LS.toList (LS.insert 9 (reclaim lend)) of
      Ur changed -> (List.sort changed, sortedUr (LS.toList snapshot))

test_additionalClones :: TestTree
test_additionalClones =
  testGroup
    "owning and GC-owned container clones"
    [ testCase "boxed growable parallel copies and later-write snapshot" (boxedGrowableClones @?= expected)
    , testCase "unboxed fixed parallel copies and later-write snapshot" (unboxedFixedClones @?= expected)
    , testCase "unboxed growable parallel copies and later-write snapshot" (unboxedGrowableClones @?= expected)
    , testCase "boxed growable clones Ref elements deeply" (boxedGrowableDeep @?= (2, 1))
    , testCase "unboxed fixed clones boxed Ref elements deeply" (unboxedFixedDeep @?= (2, 1))
    , testCase "unboxed growable clones boxed Ref elements deeply" (unboxedGrowableDeep @?= (2, 1))
    , testCase "boxed growable preserves capacity" (boxedGrowableCapacity @?= (32, 32, 2))
    , testCase "unboxed growable preserves capacity" (unboxedGrowableCapacity @?= (32, 32, 2))
    , testCase "linear-base Vector copies can grow independently" (linearVectorClones @?= ([7, 2, 3, 4], [1, 2], [1, 2]))
    , testCase "linear-base HashMap copies can resize independently" (linearMapClones @?= ([(n, n) | n <- [1 .. 40]], [(1, 2)], [(1, 2)]))
    , testCase "linear-base Set copies can resize independently" (linearSetClones @?= ([1 .. 40], [1], [1]))
    , testCase "linear-base Vector snapshot precedes later write" (linearVectorSnapshot @?= ([9, 2], [1, 2]))
    , testCase "linear-base HashMap snapshot precedes later write" (linearMapSnapshot @?= ([(1, 9)], [(1, 2)]))
    , testCase "linear-base Set snapshot precedes later write" (linearSetSnapshot @?= ([1, 9], [1]))
    , testCase "copy of linear-base HashMap points to clone" (assertDeferredTypeError "clone a shared borrow of it inside BO" copyOfSharedHashMap)
    , testCase "copy of linear-base Set points to clone" (assertDeferredTypeError "clone a shared borrow of it inside BO" copyOfSharedSet)
    , testCase "boxed growable requires Clone elements" (assertDeferredTypeError "Clone DupableOnly" growableOfDupableOnly)
    , testCase "unboxed fixed requires Clone elements even for boxed storage" (assertDeferredTypeError "Clone (U.DoNotUnboxLazy" unboxedRefsWithoutClone)
    , testCase "unboxed growable requires Clone elements even for boxed storage" (assertDeferredTypeError "Clone (U.DoNotUnboxLazy" unboxedGrowableRefsWithoutClone)
    ]
  where
    expected = ([7, 2], [8, 2], [9, 2], [1, 2])
