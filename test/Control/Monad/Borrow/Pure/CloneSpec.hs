{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Borrow.Pure.CloneSpec (
  module Control.Monad.Borrow.Pure.CloneSpec,
) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias))
import Control.Monad.Borrow.Pure.Clone.TypingCases (refOfDupableOnly, vectorOfDupableOnly)
import Control.Monad.Borrow.Pure.CloneSpec.Recipe qualified as Recipe
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
    , testCase "the documented recipe for a hand-written instance keeps the clones of a loop apart" do
        Recipe.loopArrayClones 5 @?= [0, 0, 0, 0, 0]
    , testCase "two clones collected from a loop through the documented recipe share no array" do
        Recipe.twoArrayClonesFromLoop @?= (999, 0)
    , testCase "a Ref cannot be cloned when its contents are Dupable but not Clone" do
        assertDeferredTypeError "Clone DupableOnly" refOfDupableOnly
    , testCase "a boxed Vector cannot be cloned when its elements are Dupable but not Clone" do
        assertDeferredTypeError "Clone DupableOnly" vectorOfDupableOnly
    ]

assertDeferredTypeError :: NonLinear.String -> a -> Assertion
assertDeferredTypeError expectedFragment value = do
  result <- try @SomeException (evaluate value)
  case result of
    Left exception ->
      assertBool
        ("unexpected deferred error: " <> displayException exception)
        (expectedFragment `List.isInfixOf` displayException exception)
    Right _ ->
      assertFailure ("expected a deferred type error containing " <> expectedFragment)
