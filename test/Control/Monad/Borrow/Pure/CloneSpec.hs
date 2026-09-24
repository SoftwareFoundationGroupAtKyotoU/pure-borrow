{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Borrow.Pure.CloneSpec (
  module Control.Monad.Borrow.Pure.CloneSpec,
) where

import Control.Exception (TypeError, displayException, evaluate, try)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias))
import Control.Monad.Borrow.Pure.Clone.TypingCases (copyOfSharedArray, copyOfSharedVector, refOfDupableOnly, vectorOfDupableOnly)
import Control.Monad.Borrow.Pure.CloneSpec.ArrayLoops qualified as ArrayLoops
import Data.Array.Mutable.Linear (Array)
import Data.Array.Mutable.Linear qualified as LA
import Data.Complex (Complex)
import Data.HashMap.RobinHood.Mutable.Linear qualified as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import Data.Semigroup qualified as Sem
import Data.Vector.Mutable.Linear.Borrow qualified as VL
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
{-# INLINE cloneAccount #-}
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
        assertDeferredTypeError "has no Clone instance either" copyOfSharedVector
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
