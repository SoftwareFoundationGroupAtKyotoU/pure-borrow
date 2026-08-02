{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | Benchmarks of the Robin Hood hash table.

Each group measures the same workload three ways: this package's table (owned,
and through its borrow-aware wrapper), @linear-base@'s own linear hash map, and
the unrestricted @unordered-containers@ map. The expensive-key groups exist
because the table caches a fingerprint per slot: a key whose hash is cheap but
whose equality is not is exactly where that cache pays off, and the colliding
group is the extreme of that, where every key lands in one bucket and only the
fingerprint can reject a candidate without a full comparison.
-}
module PureBorrow.Bench.HashMap (
  test_hashMap,
  ExpensiveKey (..),
  benchInsertRobinHood,
  benchInsertRobinHoodBorrow,
  benchInsertLinearBase,
  benchInsertUnordered,
  benchFromListRobinHood,
  benchFromListLinearBase,
  benchFromListUnordered,
  benchLookupRobinHood,
  benchLookupRobinHoodBorrow,
  benchLookupLinearBase,
  benchLookupUnordered,
  testData,
  expensiveTestData,
  collidingExpensiveTestData,
) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.HashMap.Mutable.Linear qualified as LB
import Data.HashMap.RobinHood.Mutable.Linear qualified as RH
import Data.HashMap.RobinHood.Mutable.Linear.Borrow qualified as RHB
import Data.HashMap.Strict qualified as UCHM
import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)
import Prelude.Linear (lseq, unur, (&))
import Prelude.Linear qualified as PL
import Test.Tasty.Bench

{- | A key with a cheap hash and an expensive equality.

The hash reads only the prefix, so two keys sharing a prefix collide and force
the payloads to be compared.
-}
data ExpensiveKey = ExpensiveKey
  { ekPrefix :: {-# UNPACK #-} !Int
  , ekPayload :: ![Int]
  }
  deriving (Show, Generic, NFData)

instance Eq ExpensiveKey where
  ExpensiveKey p1 payload1 == ExpensiveKey p2 payload2 =
    p1 == p2 && payload1 == payload2
  {-# INLINE (==) #-}

instance Hashable ExpensiveKey where
  hashWithSalt s (ExpensiveKey p _) = hashWithSalt s p
  {-# INLINE hashWithSalt #-}

-- * Insertion into an empty table

benchInsertRobinHood :: (Hashable k) => [(k, v)] -> [(k, v)]
benchInsertRobinHood kvs = unur PL.$ linearly \lin ->
  let %1 !hm = RH.new (length kvs) lin
   in RH.toList (RH.insertMany kvs hm)

benchInsertRobinHoodBorrow :: (Hashable k) => [(k, v)] -> [(k, v)]
benchInsertRobinHoodBorrow kvs = unur PL.$ linearly \lin -> DataFlow.do
  (ownerLinear, runLinear) <- PL.dup lin
  runBO runLinear Control.do
    (table, lend) <- borrowM (RHB.empty (length kvs) ownerLinear)
    table <- insertAllBorrow kvs table
    (Ur entries, table) <- RHB.toList table
    PL.consume table `lseq`
      pureAfter (PL.consume (reclaim lend) `lseq` Ur entries)

insertAllBorrow ::
  (Hashable k) =>
  [(k, v)] ->
  Mut α (RHB.HashMap k v) %1 ->
  BO α (Mut α (RHB.HashMap k v))
insertAllBorrow [] table = Control.pure table
insertAllBorrow ((k, v) : rest) table = Control.do
  (Ur _, table) <- RHB.insert k v table
  insertAllBorrow rest table

benchInsertLinearBase :: (LB.Keyed k) => [(k, v)] -> [(k, v)]
benchInsertLinearBase kvs = unur PL.$ LB.empty (length kvs) \hm ->
  LB.toList (insertManyLinearBase kvs hm)

insertManyLinearBase :: (LB.Keyed k) => [(k, v)] -> LB.HashMap k v %1 -> LB.HashMap k v
insertManyLinearBase [] hm = hm
insertManyLinearBase ((k, v) : rest) hm = insertManyLinearBase rest (LB.insert k v hm)

benchInsertUnordered :: (Hashable k) => [(k, v)] -> [(k, v)]
benchInsertUnordered kvs = UCHM.toList (foldr (uncurry UCHM.insert) UCHM.empty kvs)

-- * Bulk construction

benchFromListRobinHood :: (Hashable k) => [(k, v)] -> [(k, v)]
benchFromListRobinHood kvs = unur PL.$ linearly \lin ->
  RH.toList (RH.fromList kvs lin)

benchFromListLinearBase :: (LB.Keyed k) => [(k, v)] -> [(k, v)]
benchFromListLinearBase kvs = unur PL.$ LB.fromList kvs LB.toList

benchFromListUnordered :: (Hashable k) => [(k, v)] -> [(k, v)]
benchFromListUnordered kvs = UCHM.toList (UCHM.fromList kvs)

-- * Lookup after a bulk insertion

benchLookupRobinHood :: forall k v. (Hashable k) => [(k, v)] -> [k] -> Int
benchLookupRobinHood kvs keys = unur PL.$ linearly \lin ->
  let %1 !hm = RH.insertMany kvs (RH.new (length kvs) lin)
   in go 0 keys hm
  where
    go :: Int -> [k] -> RH.HashMap k v %1 -> Ur Int
    go !acc [] hm = hm `lseq` Ur acc
    go !acc (k : ks) hm =
      RH.lookup k hm & \(Ur mv, hm') ->
        go (acc + maybe 0 (const 1) mv) ks hm'

benchLookupRobinHoodBorrow :: forall k v. (Hashable k) => [(k, v)] -> [k] -> Int
benchLookupRobinHoodBorrow kvs keys = unur PL.$ linearly \lin -> DataFlow.do
  (ownerLinear, runLinear) <- PL.dup lin
  runBO runLinear Control.do
    (table, lend) <- borrowM (RHB.fromList kvs ownerLinear)
    (Ur found, table) <- go 0 keys table
    PL.consume table `lseq`
      pureAfter (PL.consume (reclaim lend) `lseq` Ur found)
  where
    go ::
      Int ->
      [k] ->
      Mut α (RHB.HashMap k v) %1 ->
      BO α (Ur Int, Mut α (RHB.HashMap k v))
    go !acc [] table = Control.pure (Ur acc, table)
    go !acc (k : ks) table = Control.do
      (Ur mv, table) <- RHB.lookup k table
      go (acc + maybe 0 (const 1) mv) ks table

benchLookupLinearBase :: forall k v. (LB.Keyed k) => [(k, v)] -> [k] -> Int
benchLookupLinearBase kvs keys = unur PL.$ LB.empty (length kvs) \hm ->
  let %1 !hm' = insertManyLinearBase kvs hm
   in go 0 keys hm'
  where
    go :: Int -> [k] -> LB.HashMap k v %1 -> Ur Int
    go !acc [] hm = hm `lseq` Ur acc
    go !acc (k : ks) hm =
      LB.lookup k hm & \(Ur mv, hm') ->
        go (acc + maybe 0 (const 1) mv) ks hm'

benchLookupUnordered :: forall k v. (Hashable k) => [(k, v)] -> [k] -> Int
benchLookupUnordered kvs keys = go 0 keys (foldr (uncurry UCHM.insert) UCHM.empty kvs)
  where
    go !acc [] _ = acc
    go !acc (k : ks) hm =
      go (acc + maybe 0 (const 1) (UCHM.lookup k hm)) ks hm

-- * Inputs

-- | Distinct integer keys.
testData :: Int -> [(Int, Int)]
testData n = [(i, i) | i <- [1 .. n]]

-- | Distinct expensive keys: unique prefixes, hundred-element payloads.
expensiveTestData :: Int -> [(ExpensiveKey, Int)]
expensiveTestData n =
  [ (ExpensiveKey i [i .. i + 99], i)
  | i <- [1 .. n]
  ]

-- | Expensive keys that all hash to one bucket.
collidingExpensiveTestData :: Int -> [(ExpensiveKey, Int)]
collidingExpensiveTestData n =
  [ (ExpensiveKey 42 [i .. i + 99], i)
  | i <- [1 .. n]
  ]

test_hashMap :: [Benchmark]
test_hashMap =
  [ bgroup
      "hashmap/insert"
      [ env (evaluate (force (testData n))) \kvs ->
          bgroup
            (show n)
            [ bench "robin-hood" $ nf benchInsertRobinHood kvs
            , bench "robin-hood/borrow" $ nf benchInsertRobinHoodBorrow kvs
            , bench "linear-base" $ nf benchInsertLinearBase kvs
            , bench "unordered-containers" $ nf benchInsertUnordered kvs
            ]
      | n <- sizes
      ]
  , bgroup
      "hashmap/from-list"
      [ env (evaluate (force (testData n))) \kvs ->
          bgroup
            (show n)
            [ bench "robin-hood" $ nf benchFromListRobinHood kvs
            , bench "linear-base" $ nf benchFromListLinearBase kvs
            , bench "unordered-containers" $ nf benchFromListUnordered kvs
            ]
      | n <- sizes
      ]
  , bgroup
      "hashmap/lookup"
      [ env (evaluate (force (testData n))) \kvs ->
          bgroup
            (show n)
            [ bench "robin-hood" $ nf (benchLookupRobinHood kvs) (map fst kvs)
            , bench "robin-hood/borrow" $ nf (benchLookupRobinHoodBorrow kvs) (map fst kvs)
            , bench "linear-base" $ nf (benchLookupLinearBase kvs) (map fst kvs)
            , bench "unordered-containers" $ nf (benchLookupUnordered kvs) (map fst kvs)
            ]
      | n <- sizes
      ]
  , bgroup
      "hashmap/expensive-key-insert"
      [ env (evaluate (force (expensiveTestData n))) \kvs ->
          bgroup
            (show n)
            [ bench "robin-hood" $ nf benchInsertRobinHood kvs
            , bench "linear-base" $ nf benchInsertLinearBase kvs
            , bench "unordered-containers" $ nf benchInsertUnordered kvs
            ]
      | n <- sizes
      ]
  , bgroup
      "hashmap/expensive-key-lookup"
      [ env (evaluate (force (expensiveTestData n))) \kvs ->
          bgroup
            (show n)
            [ bench "robin-hood" $ nf (benchLookupRobinHood kvs) (map fst kvs)
            , bench "linear-base" $ nf (benchLookupLinearBase kvs) (map fst kvs)
            , bench "unordered-containers" $ nf (benchLookupUnordered kvs) (map fst kvs)
            ]
      | n <- sizes
      ]
  , bgroup
      "hashmap/colliding-expensive-key-lookup"
      [ env (evaluate (force (collidingExpensiveTestData n))) \kvs ->
          bgroup
            (show n)
            [ bench "robin-hood" $ nf (benchLookupRobinHood kvs) (map fst kvs)
            , bench "linear-base" $ nf (benchLookupLinearBase kvs) (map fst kvs)
            , bench "unordered-containers" $ nf (benchLookupUnordered kvs) (map fst kvs)
            ]
      | -- Every key lands in one bucket, so the workload is quadratic; the
      -- large size would dominate the whole suite's wall clock.
      n <- collidingSizes
      ]
  ]

sizes :: [Int]
sizes = [100, 1000, 10000]

collidingSizes :: [Int]
collidingSizes = [100, 1000]
