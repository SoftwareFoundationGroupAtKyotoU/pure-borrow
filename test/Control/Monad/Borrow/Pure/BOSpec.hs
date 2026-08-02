{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.Borrow.Pure.BOSpec (
  module Control.Monad.Borrow.Pure.BOSpec,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO qualified as BO
import Control.Monad.Borrow.Pure.Experimental.Borrows qualified as Borrows
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Functor.Linear qualified as Data
import Data.HashMap.RobinHood.Mutable.Linear.Borrow qualified as HashMap
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import Data.Type.Equality ((:~:))
import Data.Vector qualified as V
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Growable
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow qualified as Unboxed
import Prelude.Linear (Ur (..), consume, dup, lseq, unur, ($), (&))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Unsafe.Linear qualified as Unsafe
import Prelude (Int, Maybe (..), otherwise, show, (+), (-), (<>), (>=))
import Prelude qualified as NonLinear

assocBorrowEqTypingCase ::
  forall (bk :: BO.BorrowKind) α β γ a.
  BO.Borrow bk ((α /\ β) /\ γ) a :~: BO.Borrow bk (α /\ (β /\ γ)) a
assocBorrowEqTypingCase = BO.assocBorrowEq @bk @α @β @γ @a

shortenShare :: (α >= β) => Share α a -> Share β a
shortenShare = subShare

addLinear :: Int %1 -> Int %1 -> Int
addLinear = Unsafe.toLinear2 (+)

test_instanceMethods :: TestTree
test_instanceMethods =
  testGroup
    "BO instance methods"
    [ testCase "linear liftA2" do
        linearly (\lin -> runBO_ lin (Control.liftA2 addLinear (Control.pure 20) (Control.pure 22))) @?= (42 :: Int)
    , testCase "non-linear liftA2" do
        linearly (\lin -> runBO_ lin (Data.liftA2 addLinear (Data.pure 20) (Data.pure 22))) @?= (42 :: Int)
    , testCase "linear sequencing" do
        linearly (\lin -> runBO_ lin (Control.pure () Control.>> Control.pure 42)) @?= (42 :: Int)
    ]

{-
Note [Observing a borrow scope's writes through the borrow it restores]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The kernels below share one shape: read the length through a borrow, hand that borrow to a scope whose body grows the vector, then read the length again through the borrow the scope handed back.
The second read must observe the growth.

They live here rather than with the growable vector's own tests because the growable vector is the instrument, not the subject.
What is under test is the delimiter -- see Note [Restoring a borrow must break its Core identity] in "Control.Monad.Borrow.Pure.BO.Internal" -- and a growable vector is simply the resource this package ships whose length and buffer are read outside the state token, so a delimiter that hands back the caller's own binder lets common-subexpression elimination serve the second read from the first.

Two details are load-bearing, and both were established by measuring variants against the pre-fix delimiter rather than by reasoning.

Both reads must end up inlined into one Core body: each kernel is therefore a @NOINLINE@ top-level binding with no helper standing between the borrow and either read.
Moving the pre-scope read behind its own @NOINLINE@ helper, or reaching the post-scope read through a recursive worker, makes the kernel pass on the broken delimiter -- vacuous, not merely weaker.
The scope must write the header; which projection each side reads does not matter, and @capacity@ before against @size@ after merges just as readily.

Two details that look load-bearing are not.
The reallocation matters only for the stale-/buffer/ symptom that 'elementAcrossReborrowing' and 'writeAcrossReborrowing' observe; the stale-/length/ symptom reproduces on a push that fits in the existing capacity.
And the @NOINLINE@ on each kernel is insurance rather than a requirement -- a plain CAF in the style of the other spec helpers reproduces too.

Most of these fail as wrong data rather than as a crash: at -O2 before the fix the post-scope read is deleted outright and the pre-scope length is returned in its place.
'writeAcrossReborrowing' is the exception, and is the one that reaches the shape which corrupted the heap downstream: it writes at an index the fresh length admits, through a header that still names the old, shorter buffer.
It fails here as a bounds error only because 'Growable.modify' is the checked operation; the unchecked 'Growable.unsafeSet' at the same index would run off the end of the allocation, which is what the downstream SIGSEGV was.

There is deliberately no 'sharing' or 'sharing_' kernel.
Those delimiters hand their callback a 'Share', nothing in the API writes a growable header through a 'Share', and the 'Mut' that would be needed was consumed by the delimiter -- so such a kernel would pass on the broken build and would be evidence of nothing.
They are fixed all the same, because the obligation belongs to the delimiter rather than to the set of writes that happen to be reachable today, but that half of the fix is prophylactic and is pinned by the Core obligations in @pure-borrow-inspection@ instead.
-}

seeded :: [Int]
seeded = [10, 20, 30]

appended :: Int -> [Int]
appended count = NonLinear.map (100 +) (NonLinear.enumFromTo 0 (count - 1))

-- | The model every kernel below is compared against: the length before, the length after, and the contents.
grown :: Int -> (Int, Int, [Int])
grown count = (3, 3 + count, seeded <> appended count)

pushRange ::
  (α >= β) =>
  Int ->
  Int ->
  Mut α (Growable.GrowableVector Int) %1 ->
  BO β (Mut α (Growable.GrowableVector Int))
{-# INLINE pushRange #-}
pushRange index count vector
  | index >= count = Control.pure vector
  | otherwise = Control.do
      vector <- Growable.push (100 + index) vector
      pushRange (index + 1) count vector

pushRangeUnboxed ::
  (α >= β) =>
  Int ->
  Int ->
  Mut α (Unboxed.GrowableVector Int) %1 ->
  BO β (Mut α (Unboxed.GrowableVector Int))
{-# INLINE pushRangeUnboxed #-}
pushRangeUnboxed index count vector
  | index >= count = Control.pure vector
  | otherwise = Control.do
      vector <- Unboxed.push (100 + index) vector
      pushRangeUnboxed (index + 1) count vector

-- | 'reborrowing': the scope returns a value alongside the restored borrow.
lengthAcrossReborrowing :: Int -> (Int, Int, [Int])
{-# NOINLINE lengthAcrossReborrowing #-}
lengthAcrossReborrowing count =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList seeded) ownerLinear)
      Growable.size vector & \(Ur before, vector) -> Control.do
        ((), vector) <-
          reborrowing vector \short ->
            consume Data.<$> pushRange 0 count short
        Growable.size vector & \(Ur after, vector) ->
          vector `lseq` pureAfter (report before after (reclaim lend))

-- | 'reborrowing'': the scope returns its value @After@ the sublifetime.
lengthAcrossReborrowingAfter :: Int -> (Int, Int, [Int])
{-# NOINLINE lengthAcrossReborrowingAfter #-}
lengthAcrossReborrowingAfter count =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList seeded) ownerLinear)
      Growable.size vector & \(Ur before, vector) -> Control.do
        ((), vector) <-
          reborrowing' vector \short ->
            (\short -> After (consume short)) Data.<$> pushRange 0 count short
        Growable.size vector & \(Ur after, vector) ->
          vector `lseq` pureAfter (report before after (reclaim lend))

-- | 'reborrowing_': the scope discards its value.
lengthAcrossReborrowingDiscarding :: Int -> (Int, Int, [Int])
{-# NOINLINE lengthAcrossReborrowingDiscarding #-}
lengthAcrossReborrowingDiscarding count =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList seeded) ownerLinear)
      Growable.size vector & \(Ur before, vector) -> Control.do
        vector <-
          reborrowing_ vector \short ->
            consume Data.<$> pushRange 0 count short
        Growable.size vector & \(Ur after, vector) ->
          vector `lseq` pureAfter (report before after (reclaim lend))

-- | The same as 'lengthAcrossReborrowing', on the unboxed growable vector.
lengthAcrossReborrowingUnboxed :: Int -> (Int, Int, [Int])
{-# NOINLINE lengthAcrossReborrowingUnboxed #-}
lengthAcrossReborrowingUnboxed count =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Unboxed.fromVector (U.fromList seeded) ownerLinear)
      Unboxed.size vector & \(Ur before, vector) -> Control.do
        ((), vector) <-
          reborrowing vector \short ->
            consume Data.<$> pushRangeUnboxed 0 count short
        Unboxed.size vector & \(Ur after, vector) ->
          vector `lseq` pureAfter (reportUnboxed before after (reclaim lend))

{- | 'Growable.withContent', which delimits a fixed view of the growable vector the same way.

Growth happens after the scope rather than inside it, because the fixed view the callback receives deliberately cannot grow.
Unlike its neighbours this kernel passes on the broken delimiter and cannot be made to fail: the callback receives a bare slice with no header to write, so nothing the scope does can make a merged header read stale.
It is kept as a forward-looking guard on a delimiter that shares the defective shape, not as regression coverage — 'Growable.withContent' restores through the barrier for the same prophylactic reason 'sharing' does.
-}
lengthAcrossContentScope :: Int -> (Int, Int, [Int])
{-# NOINLINE lengthAcrossContentScope #-}
lengthAcrossContentScope count =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList seeded) ownerLinear)
      Growable.size vector & \(Ur before, vector) -> Control.do
        ((), vector) <-
          Growable.withContent vector \contents ->
            Control.pure (consume contents)
        vector <- pushRange 0 count vector
        Growable.size vector & \(Ur after, vector) ->
          vector `lseq` pureAfter (report before after (reclaim lend))

{- | The same read-grow-read shape, repeated inside a recursive 'BO' loop.

The straight-line kernels above already reproduce, but a loop is what supplies the inlining depth and the loop-invariant read the downstream report describes, so this covers a read floated out of the loop as well as one merged with its neighbour.
Each round appends one element and records the length it observes afterwards, so a stale read is caught on the round that made it stale rather than only at the end.
-}
lengthsAcrossReborrowingLoop :: Int -> ([Int], [Int])
{-# NOINLINE lengthsAcrossReborrowingLoop #-}
lengthsAcrossReborrowingLoop rounds =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList seeded) ownerLinear)
      (Ur observed, vector) <- go 0 vector
      vector `lseq` pureAfter do
        Growable.toVector (reclaim lend) & \(Ur contents) ->
          Ur (observed, V.toList contents)
  where
    go ::
      forall α.
      Int ->
      Mut α (Growable.GrowableVector Int) %1 ->
      BO α (Ur [Int], Mut α (Growable.GrowableVector Int))
    {-# INLINE go #-}
    go index vector
      | index >= rounds = Control.pure (Ur [], vector)
      | otherwise = Control.do
          ((), vector) <-
            reborrowing vector \short ->
              consume Data.<$> Growable.push (100 + index) short
          Growable.size vector & \(Ur seen, vector) -> Control.do
            (Ur rest, vector) <- go (index + 1) vector
            Control.pure (Ur (seen : rest), vector)

{- | An element read through the restored borrow, rather than a length.

The kernels above all materialize their contents from @'reclaim' lend@, which is the owner and never the expression the delimiter resurrects, so their contents component is correct even on the broken build and only their lengths discriminate.
This one reads back through the borrow the scope handed over, so it is the kernel that actually witnesses a stale /buffer/: the push reallocates, the following 'Growable.modify' lands in the new allocation only, and a restored borrow still naming the old header reads the value from before the bump.
-}
elementAcrossReborrowing :: Int -> Int
{-# NOINLINE elementAcrossReborrowing #-}
elementAcrossReborrowing bumpBy =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList seeded) ownerLinear)
      ((), vector) <-
        reborrowing vector \short -> Control.do
          short <- Growable.push 100 short
          short <- Growable.modify 1 (addLinear bumpBy) short
          Control.pure (consume short)
      (Ur seen, vector) <- Growable.copyAtMut 1 vector
      vector `lseq` pureAfter (consume (reclaim lend) `lseq` Ur seen)

{- | A write through the restored borrow, at an index only the post-scope length admits.

This is the shape that corrupted the heap downstream.
'Growable.modify' is bounds-checked, so on the broken build it raises rather than scribbling past the allocation; the unchecked 'Growable.unsafeSet' at the same index is what the downstream solver was doing.
-}
writeAcrossReborrowing :: Int -> [Int]
{-# NOINLINE writeAcrossReborrowing #-}
writeAcrossReborrowing count =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList seeded) ownerLinear)
      -- The read before the scope is what the bounds check inside 'Growable.modify'
      -- gets merged with; without it there is nothing for the stale read to be
      -- served from, and this kernel passes even on the broken delimiter.
      Growable.size vector & \(Ur before, vector) -> Control.do
        vector <-
          reborrowing_ vector \short ->
            consume Data.<$> pushRange 0 count short
        vector <- Growable.modify (before + count - 1) (addLinear 1) vector
        vector `lseq` pureAfter do
          Growable.toVector (reclaim lend) & \(Ur contents) ->
            Ur (V.toList contents)

{- | The same shape on a plain 'Ref.Ref', whose reads bypass the state token in exactly the same way.

'RefBorrow.copyRef' bottoms out in 'Data.Ref.Linear.unsafeReadRef', so this is the smallest instrument that exhibits the defect at all — no vector involved.
The reads go through their own 'reborrowing' because 'RefBorrow.copyRef' consumes the borrow it reads.
-}
valueAcrossReborrowingRef :: Int -> (Int, Int)
{-# NOINLINE valueAcrossReborrowingRef #-}
valueAcrossReborrowingRef start =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (ref, lend) <- borrowM (Ref.new start ownerLinear)
      (before, ref) <- reborrowing ref RefBorrow.copyRef
      ((), ref) <-
        reborrowing ref \short ->
          consume Data.<$> RefBorrow.modify (addLinear 1) short
      (after, ref) <- reborrowing ref RefBorrow.copyRef
      ref `lseq` pureAfter (consume (Ref.free (reclaim lend)) `lseq` (before, after))

{- | The same shape on the Robin Hood table, which replaces its backing array on growth.

A stale read here is a stale /array/, not a stale count, so the lookup afterwards is the interesting assertion: on the broken delimiter it reports the key as absent after eight successful inserts.
-}
entriesAcrossReborrowingHashMap :: Int -> (Int, Int, Maybe Int)
{-# NOINLINE entriesAcrossReborrowingHashMap #-}
entriesAcrossReborrowingHashMap count =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (table, lend) <- borrowM (HashMap.empty @Int @Int 0 ownerLinear)
      (Ur before, table) <- HashMap.size table
      table <-
        reborrowing_ table \short ->
          consume Data.<$> insertRange 0 count short
      (Ur after, table) <- HashMap.size table
      (Ur found, table) <- HashMap.lookup 0 table
      table `lseq` pureAfter (consume (reclaim lend) `lseq` Ur (before, after, found))

{- | The plural delimiter, which restores a whole 'Borrows.Muts' bundle.

Like the 'Growable.withContent' kernel this one passes on the broken build, because 'Borrows.reborrowings'' restores through 'reclaim'' inside an @After@ and so picks up 'withEnd'\'s @nospec@ barrier by accident.
It is here as a guard on that accident: erasing that delimiter the way 'reborrowing'' was erased would remove it, and this kernel is what would then go red.
-}
lengthAcrossPluralReborrowing :: Int -> (Int, Int, [Int])
{-# NOINLINE lengthAcrossPluralReborrowing #-}
lengthAcrossPluralReborrowing count =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList seeded) ownerLinear)
      Growable.size vector & \(Ur before, vector) -> Control.do
        bundle <-
          Borrows.reborrowings_
            (vector Borrows.:- Borrows.BNil)
            \(short Borrows.:- Borrows.BNil) ->
              consume Data.<$> pushRange 0 count short
        case bundle of
          vector Borrows.:- Borrows.BNil ->
            Growable.size vector & \(Ur after, vector) ->
              vector `lseq` pureAfter (report before after (reclaim lend))

insertRange ::
  Int ->
  Int ->
  Mut α (HashMap.HashMap Int Int) %1 ->
  BO α (Mut α (HashMap.HashMap Int Int))
{-# INLINE insertRange #-}
insertRange index count table
  | index >= count = Control.pure table
  | otherwise = Control.do
      (Ur _, table) <- HashMap.insert index (100 + index) table
      insertRange (index + 1) count table

report ::
  Int ->
  Int ->
  Growable.GrowableVector Int %1 ->
  Ur (Int, Int, [Int])
{-# INLINE report #-}
report before after vector =
  Growable.toVector vector & \(Ur contents) ->
    Ur (before, after, V.toList contents)

reportUnboxed ::
  Int ->
  Int ->
  Unboxed.GrowableVector Int %1 ->
  Ur (Int, Int, [Int])
{-# INLINE reportUnboxed #-}
reportUnboxed before after vector =
  Unboxed.toVector vector & \(Ur contents) ->
    Ur (before, after, U.toList contents)

{- | Regression coverage for the erased borrow scopes.

See Note [Observing a borrow scope's writes through the borrow it restores].
-}
test_scopeRestoresAUsableBorrow :: TestTree
test_scopeRestoresAUsableBorrow =
  testGroup
    "a borrow scope's writes are visible through the borrow it restores"
    [ testGroup "reborrowing" (cases lengthAcrossReborrowing)
    , testGroup "reborrowing'" (cases lengthAcrossReborrowingAfter)
    , testGroup "reborrowing_" (cases lengthAcrossReborrowingDiscarding)
    , testGroup "reborrowing, unboxed" (cases lengthAcrossReborrowingUnboxed)
    , testGroup "withContent" (cases lengthAcrossContentScope)
    , testCase "reborrowing in a loop" do
        lengthsAcrossReborrowingLoop 8
          @?= (NonLinear.map (4 +) (NonLinear.enumFromTo 0 7), seeded <> appended 8)
    , testGroup
        "an element read back through the restored borrow"
        [ testCase (show bumpBy <> " added") do
            elementAcrossReborrowing bumpBy @?= 20 + bumpBy
        | bumpBy <- [1, 5, 41]
        ]
    , testGroup
        "a write through the restored borrow lands in the grown buffer"
        [ testCase (show count <> " appended") do
            writeAcrossReborrowing count
              @?= seeded <> NonLinear.init (appended count) <> [100 + count]
        | count <- [1, 2, 5, 17]
        ]
    , testGroup
        "Ref"
        [ testCase (show start) do
            valueAcrossReborrowingRef start @?= (start, start + 1)
        | start <- [0, 1, 41]
        ]
    , testGroup "reborrowings (plural)" (cases lengthAcrossPluralReborrowing)
    , testGroup
        "RobinHood HashMap"
        [ testCase (show count <> " inserted") do
            entriesAcrossReborrowingHashMap count @?= (0, count, Just 100)
        | count <- [1, 8, 33]
        ]
    ]
  where
    cases :: (Int -> (Int, Int, [Int])) -> [TestTree]
    cases kernel =
      [ testCase (show count <> " appended") (kernel count @?= grown count)
      | count <- counts
      ]
    counts :: [Int]
    counts = [1, 2, 3, 5, 17, 33, 1025]
