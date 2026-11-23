{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Specs of the borrow-aware Robin Hood hash table.
module Data.HashMap.RobinHood.Mutable.Linear.BorrowSpec (
  module Data.HashMap.RobinHood.Mutable.Linear.BorrowSpec,
) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.HashMap.RobinHood.Mutable.Linear.Borrow qualified as HM
import Data.HashMap.RobinHood.Mutable.Linear.Borrow.Unsafe qualified as HMU
import Data.HashMap.RobinHood.Mutable.LinearSpec.Steered (Steered (..))
import Data.HashMap.Strict qualified as HMS
import Data.Hashable (Hashable (..))
import Data.List qualified as NonLinear
import Data.List.NonEmpty (NonEmpty (..))
import Prelude.Linear
import Test.Falsify.Generator qualified as G
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as G
import Test.Tasty (TestTree)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit
import Prelude qualified as NonLinear

type Table = HM.HashMap String Int

{- | Run an action against a freshly borrowed table.

The action owns the mutable borrow and must consume it; the table itself is
reclaimed and released once the lifetime ends.
-}
withTable ::
  forall r.
  Int ->
  (forall α. Mut α Table %1 -> BO α (Ur r)) ->
  Ur r
withTable capacity action =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (table, lend) <- borrowM (HM.empty capacity ownerLinear)
      Ur result <- action table
      pureAfter (consume (reclaim lend) `lseq` Ur result)

-- | Materialize an owned table's entries, then release it.
drainTable :: HM.HashMap k v %1 -> Ur [(k, v)]
drainTable table =
  linearly \linear ->
    runBO linear Control.do
      (borrowed, lend) <- borrowM table
      (Ur entries, borrowed) <- HM.toList borrowed
      consume borrowed `lseq` pureAfter (consume (reclaim lend) `lseq` Ur entries)

insertAll :: [(String, Int)] -> Mut α Table %1 -> BO α (Mut α Table)
insertAll [] table = Control.pure table
insertAll ((k, v) : rest) table = Control.do
  (Ur _, table) <- HM.insert k v table
  insertAll rest table

sorted :: [(String, Int)] -> [(String, Int)]
sorted = NonLinear.sortOn NonLinear.fst

test_insertLookupDelete :: TestTree
test_insertLookupDelete = testCase "insert, lookup and delete through a mutable borrow" do
  let Ur (displaced, one, missing, present, absent, count, entries) =
        withTable 16 \table -> Control.do
          (Ur displacedOne, table) <- HM.insert "one" 1 table
          (Ur _, table) <- HM.insert "two" 2 table
          (Ur displacedTwo, table) <- HM.insert "two" 22 table
          (Ur one, table) <- HM.lookup "one" table
          (Ur missing, table) <- HM.lookup "three" table
          (Ur present, table) <- HM.member "two" table
          (Ur deleted, table) <- HM.delete "one" table
          (Ur absent, table) <- HM.member "one" table
          (Ur count, table) <- HM.size table
          (Ur entries, table) <- HM.toList table
          consume table `lseq`
            Control.pure
              (Ur ((displacedOne, displacedTwo, deleted), one, missing, present, absent, count, entries))
  displaced @?= (Nothing, Just 2, Just 1)
  one @?= Just 1
  missing @?= Nothing
  present @?= True
  absent @?= False
  count @?= 1
  entries @?= [("two", 22)]

test_growthIsVisibleThroughTheBorrow :: TestTree
test_growthIsVisibleThroughTheBorrow =
  testCase "a growth taken while borrowed is written back to the reference" do
    -- The underlying owned table replaces its backing array when it grows, so
    -- this is the regression test for the 'Ref' indirection: were the grown
    -- table not written back, every entry inserted after the first growth
    -- would be lost.
    let pairs = [(NonLinear.show i, i) | i <- [1 .. 512 :: Int]]
        Ur (count, entries) = withTable 4 \table -> Control.do
          table <- insertAll pairs table
          (Ur count, table) <- HM.size table
          (Ur entries, table) <- HM.toList table
          consume table `lseq` Control.pure (Ur (count, entries))
    count @?= NonLinear.length pairs
    sorted entries @?= sorted pairs

test_preparedInsertion :: TestTree
test_preparedInsertion = testCase "suspended lookups resume as insertions" do
  let expected = [(NonLinear.show i, i) | i <- [1 .. 256 :: Int]]
      Ur (found, entries) = withTable 4 \table -> Control.do
        table <- go 1 table
        (Ur found, table) <- HM.lookup "128" table
        (Ur entries, table) <- HM.toList table
        consume table `lseq` Control.pure (Ur (found, entries))
  found @?= Just 128
  sorted entries @?= sorted expected
  where
    go :: Int -> Mut α Table %1 -> BO α (Mut α Table)
    go i table
      | i NonLinear.> 256 = Control.pure table
      | otherwise = Control.do
          (Ur plan, table) <- HMU.lookupForInsert (NonLinear.show i) table
          table <- case plan of
            Left _ -> Control.pure table
            Right plan -> HMU.unsafeInsertPrepared plan i table
          go (i + 1) table

test_alter :: TestTree
test_alter = testCase "alter inserts, updates and deletes" do
  let Ur (inserted, updated, deleted) = withTable 16 \table -> Control.do
        table <- HM.alter (\_ -> Just 1) "key" table
        (Ur inserted, table) <- HM.lookup "key" table
        table <- HM.alter (NonLinear.fmap (NonLinear.+ 41)) "key" table
        (Ur updated, table) <- HM.lookup "key" table
        table <- HM.alter (\_ -> Nothing) "key" table
        (Ur deleted, table) <- HM.lookup "key" table
        consume table `lseq` Control.pure (Ur (inserted, updated, deleted))
  inserted @?= Just 1
  updated @?= Just 42
  deleted @?= Nothing

test_alterF :: TestTree
test_alterF = testCase "alterF may inspect the table it is altering" do
  let Ur (observed, final) = withTable 16 \table -> Control.do
        table <- HM.alter (\_ -> Just 7) "key" table
        table <-
          HM.alterF
            (\seen -> Control.pure (Ur (NonLinear.fmap (NonLinear.* 2) seen)))
            "key"
            table
        (Ur final, table) <- HM.lookup "key" table
        (Ur observed, table) <- HM.size table
        consume table `lseq` Control.pure (Ur (observed, final))
  observed @?= 1
  final @?= Just 14

test_takeLeavesAnEmptyTable :: TestTree
test_takeLeavesAnEmptyTable = testCase "take empties the borrowed table" do
  let pairs = [("a", 1), ("b", 2), ("c", 3 :: Int)]
      Ur (taken, remaining) = withTable 16 \table -> Control.do
        table <- insertAll pairs table
        (old, table) <- HM.take table
        (Ur remaining, table) <- HM.toList table
        drainTable old & \(Ur taken) ->
          consume table `lseq` Control.pure (Ur (taken, remaining))
  sorted taken @?= sorted pairs
  remaining @?= []

test_swap :: TestTree
test_swap = testCase "swap exchanges the borrowed table with an owned one" do
  let Ur (displaced, remaining) = withTable 16 \table -> Control.do
        table <- insertAll [("a", 1), ("b", 2)] table
        replacement <- asksLinearlyM \lin -> Control.pure (HM.fromList [("z", 26)] lin)
        (old, table) <- HM.swap replacement table
        (Ur remaining, table) <- HM.toList table
        drainTable old & \(Ur displaced) ->
          consume table `lseq` Control.pure (Ur (displaced, remaining))
  sorted displaced @?= [("a", 1), ("b", 2)]
  remaining @?= [("z", 26)]

test_extend :: TestTree
test_extend = testCase "extend merges an owned table into a borrowed one" do
  let Ur entries = withTable 16 \table -> Control.do
        table <- insertAll [("a", 1), ("b", 2)] table
        donor <- asksLinearlyM \lin -> Control.pure (HM.fromList [("b", 20), ("c", 3)] lin)
        table <- HM.extend donor table
        (Ur entries, table) <- HM.toList table
        consume table `lseq` Control.pure (Ur entries)
  sorted entries @?= [("a", 1), ("b", 20), ("c", 3)]

-- | Extend a table holding @a@ and @b@ with a donor of the given entries.
extendWith :: [(String, Int)] -> [(String, Int)]
extendWith donorPairs =
  let Ur entries = withTable 16 \table -> Control.do
        table <- insertAll [("a", 1), ("b", 2)] table
        donor <- asksLinearlyM \lin -> Control.pure (HM.fromList donorPairs lin)
        table <- HM.extend donor table
        (Ur entries, table) <- HM.toList table
        consume table `lseq` Control.pure (Ur entries)
   in sorted entries

test_extendKeepsTheInsertedValue :: TestTree
test_extendKeepsTheInsertedValue = testCase "extend keeps the inserted value, whichever table is larger" do
  extendWith [("b", 20)] @?= [("a", 1), ("b", 20)]
  extendWith [("b", 20), ("c", 3), ("d", 4)] @?= [("a", 1), ("b", 20), ("c", 3), ("d", 4)]

-- | An 'Int' hashes to itself, so multiples of 1024 have hashes that agree in their ten low bits, which a table must not pick its buckets by.
test_insertKeepsKeysSharingLowBits :: TestTree
test_insertKeepsKeysSharingLowBits = testCase "insert keeps keys whose hashes share their low bits" do
  let keys = [i NonLinear.* 1024 | i <- [1 .. 200 :: Int]]
      Ur entries = linearly \linear -> DataFlow.do
        (ownerLinear, runLinear) <- dup linear
        runBO runLinear Control.do
          (table, lend) <- borrowM (HM.empty 16 ownerLinear)
          table <- insertKeys keys table
          (Ur entries, table) <- HM.toList table
          consume table `lseq` pureAfter (consume (reclaim lend) `lseq` Ur entries)
  NonLinear.sortOn NonLinear.fst entries @?= [(k, k) | k <- keys]
  where
    insertKeys :: [Int] -> Mut α (HM.HashMap Int Int) %1 -> BO α (Mut α (HM.HashMap Int Int))
    insertKeys [] table = Control.pure table
    insertKeys (k : rest) table = Control.do
      (Ur _, table) <- HM.insert k k table
      insertKeys rest table

-- | A key whose every value has the same hash.
newtype Colliding = Colliding Int
  deriving (NonLinear.Show, NonLinear.Eq)

instance Hashable Colliding where
  hashWithSalt _ _ = 0

test_insertRejectsTooManyKeysOfOneHash :: TestTree
test_insertRejectsTooManyKeysOfOneHash = testCase "insert raises on 129 keys of one hash" do
  let Ur count = linearly \linear -> DataFlow.do
        (ownerLinear, runLinear) <- dup linear
        runBO runLinear Control.do
          (table, lend) <- borrowM (HM.empty 16 ownerLinear)
          table <- insertColliding [1 .. 129] table
          (Ur count, table) <- HM.size table
          consume table `lseq` pureAfter (consume (reclaim lend) `lseq` Ur count)
  result <- try @ErrorCall (evaluate count)
  case result of
    Left (ErrorCall message) -> assertBool message ("too many keys collide" `NonLinear.isInfixOf` message)
    Right n -> assertFailure ("insert left " NonLinear.<> NonLinear.show n NonLinear.<> " entries")
  where
    insertColliding :: [Int] -> Mut α (HM.HashMap Colliding Int) %1 -> BO α (Mut α (HM.HashMap Colliding Int))
    insertColliding [] table = Control.pure table
    insertColliding (i : rest) table = Control.do
      (Ur _, table) <- HM.insert (Colliding i) i table
      insertColliding rest table

test_insertKeepsAnEntryDisplacedAtTheDibLimit :: TestTree
test_insertKeepsAnEntryDisplacedAtTheDibLimit =
  testCase "an entry displaced at the DIB limit stays reachable and is not stored twice" do
    -- 127 keys of bucket 0 and two of bucket 1 behind them, then a 128th key of bucket 0, which displaces the first of those two.
    let fps = [256 NonLinear.* i | i <- [0 .. 126]] NonLinear.<> [257, 769, 256 NonLinear.* 127]
        Ur outcome = linearly \linear -> DataFlow.do
          (ownerLinear, runLinear) <- dup linear
          runBO runLinear Control.do
            (table, lend) <- borrowM (HM.empty 256 ownerLinear)
            table <- insertSteered fps table
            (Ur before, table) <- HM.lookup (Steered 257) table
            (Ur displaced, table) <- HM.insert (Steered 257) 99 table
            (Ur after, table) <- HM.lookup (Steered 257) table
            (Ur count, table) <- HM.size table
            (Ur entries, table) <- HM.toList table
            consume table `lseq`
              pureAfter
                ( consume (reclaim lend) `lseq`
                    Ur (before, displaced, after, count, NonLinear.length [() | (Steered 257, _) <- entries])
                )
    outcome @?= (Just 257, Just 257, Just 99, 130, 1)
  where
    insertSteered :: [Int] -> Mut α (HM.HashMap Steered Int) %1 -> BO α (Mut α (HM.HashMap Steered Int))
    insertSteered [] table = Control.pure table
    insertSteered (fp : rest) table = Control.do
      (Ur _, table) <- HM.insert (Steered fp) fp table
      insertSteered rest table

test_unionOfCollidingKeys :: TestTree
test_unionOfCollidingKeys = testCase "union keeps the colliding keys of both tables, and raises on more than a table holds" do
  -- 128 distinct keys of one hash fit, whichever table is the larger.
  unionColliding [1 .. 80] [61 .. 128]
    @?= ([(k, 1) | k <- [1 .. 60]] NonLinear.<> [(k, 2) | k <- [61 .. 128]])
  unionColliding [1 .. 40] [21 .. 128]
    @?= ([(k, 1) | k <- [1 .. 20]] NonLinear.<> [(k, 2) | k <- [21 .. 128]])
  result <- try @ErrorCall (evaluate (NonLinear.length (unionColliding [1 .. 80] [61 .. 129])))
  case result of
    Left (ErrorCall message) -> assertBool message ("too many keys collide" `NonLinear.isInfixOf` message)
    Right n -> assertFailure ("union left " NonLinear.<> NonLinear.show n NonLinear.<> " entries")
  where
    -- The union of a table holding 1 at the first keys and one holding 2 at the second keys.
    unionColliding :: [Int] -> [Int] -> [(Int, Int)]
    unionColliding left right =
      let Ur entries = linearly \linear -> DataFlow.do
            (linLeft, linRight) <- dup linear
            drainTable
              ( HM.union
                  (HM.fromList [(Colliding k, 1 :: Int) | k <- left] linLeft)
                  (HM.fromList [(Colliding k, 2) | k <- right] linRight)
              )
       in NonLinear.sortOn NonLinear.fst [(k, v) | (Colliding k, v) <- entries]

test_union :: TestTree
test_union = testCase "union of two owned tables" do
  let Ur entries = linearly \linear -> DataFlow.do
        (linLeft, linRight) <- dup linear
        drainTable
          ( HM.union
              (HM.fromList [("a", 1), ("b", 2)] linLeft)
              (HM.fromList [("b", 20), ("c", 3)] linRight)
          )
  sorted entries @?= [("a", 1), ("b", 20), ("c", 3)]

test_unionKeepsTheSecondTablesValue :: TestTree
test_unionKeepsTheSecondTablesValue = testCase "union keeps the second table's value, whichever table is larger" do
  unionOf [("b", 2)] [("b", 20), ("c", 3)] @?= [("b", 20), ("c", 3)]
  unionOf [("a", 1), ("b", 2)] [("b", 20)] @?= [("a", 1), ("b", 20)]
  where
    unionOf :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
    unionOf left right =
      let Ur entries = linearly \linear -> DataFlow.do
            (linLeft, linRight) <- dup linear
            drainTable (HM.union (HM.fromList left linLeft) (HM.fromList right linRight))
       in sorted entries

-- | Mutations through a borrow agree with @unordered-containers@.
test_randomMutations :: TestTree
test_randomMutations = testProperty "random mutations agree with the oracle" do
  program <-
    F.gen $
      G.list (G.between (1, 256)) do
        key <- G.list (G.between (1, 4)) (G.elem ('a' :| "bcd"))
        value <- G.int (G.between (-10, 10))
        deleting <- G.bool NonLinear.False
        NonLinear.pure (if deleting then Left key else Right (key, value))
  let expected = NonLinear.foldl' step HMS.empty program
      Ur entries = withTable 8 \table -> Control.do
        table <- apply program table
        (Ur entries, table) <- HM.toList table
        consume table `lseq` Control.pure (Ur entries)
  F.assert $ P.expect expected .$ ("final table", HMS.fromList entries)
  where
    step oracle = \case
      Left key -> HMS.delete key oracle
      Right (key, value) -> HMS.insert key value oracle

    apply ::
      [Either String (String, Int)] ->
      Mut α Table %1 ->
      BO α (Mut α Table)
    apply [] table = Control.pure table
    apply (instruction : rest) table = Control.do
      table <- case instruction of
        Left key -> Control.fmap (\(Ur _, table) -> table) (HM.delete key table)
        Right (key, value) -> Control.fmap (\(Ur _, table) -> table) (HM.insert key value table)
      apply rest table
