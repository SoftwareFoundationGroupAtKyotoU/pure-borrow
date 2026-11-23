{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Specs of the owned Robin Hood hash table.
module Data.HashMap.RobinHood.Mutable.LinearSpec (
  module Data.HashMap.RobinHood.Mutable.LinearSpec,
) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Control.Functor.Linear qualified as Lin
import Control.Monad (forM)
import Control.Monad.Borrow.Pure (Linearly, linearly)
import Data.Bifunctor.Linear qualified as BiL
import Data.Foldable (forM_)
import Data.HashMap.RobinHood.Mutable.Linear.Internal qualified as LHM
import Data.HashMap.RobinHood.Mutable.LinearSpec.Cases
import Data.HashMap.RobinHood.Mutable.LinearSpec.Steered
import Data.HashMap.Strict qualified as HMS
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable (..))
import Data.List (isInfixOf, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Unrestricted.Linear (UrT (..), runUrT)
import Data.Unrestricted.Linear qualified as Ur
import GHC.Generics (Generic)
import Prelude.Linear (Ur (..), unur, (&))
import Prelude.Linear qualified as PL
import Test.Falsify.Generator qualified as F
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as F
import Test.Tasty
import Test.Tasty.Falsify (Property, testProperty)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit

test_case1 :: TestTree
test_case1 = testCase "HashMap case 1" do
  let Ur Case1Result {..} = withNewEmptyHashMap case1
  initOneResident @?= Nothing
  newOneResident @?= Just 1
  initTwoResident @?= Nothing
  deletedOneResident @?= Just 1
  finalResult @?= [("Two", 2)]

test_case2 :: TestTree
test_case2 = testCase "HashMap case 2" do
  let Ur finalResult = withNewEmptyHashMap case2
  let resl = Map.fromList finalResult
      expected = Map.fromList [(show i, i) | i <- [1 .. 15] <> [129 .. 256]]
  Map.size resl @?= Map.size expected
  resl @?= expected

test_case3 :: TestTree
test_case3 = testCase "HashMap case 3" do
  let Ur Case3Result {..} = withNewEmptyHashMap case3
  iniOneReside @?= iniOneResideExpected
  oneBeforeBulkInsert @?= oneBeforeBulkInsertExpected
  oneAfterBulkInsert @?= oneAfterBulkInsertExpected
  sixteenAfterBulkInsert @?= sixteenAfterBulkInsertExpected
  sixteenAfterBulkDelete @?= sixteenAfterBulkDeleteExpected
  poppedSixteen @?= poppedSixteenExpected
  finalSixteen @?= finalSixteenExpected
  Map.size finalResult @?= Map.size expectedResult
  finalResult @?= expectedResult

test_case4 :: TestTree
test_case4 = testCase "HashMap case 4" do
  let Ur (ccVal, lst) = withNewEmptyHashMap \hm ->
        LHM.insert "aa" (-10 :: Int) hm & \(Ur _, hm) ->
          LHM.insert "cc" (-10) hm & \(Ur _, hm) ->
            LHM.lookup "cc" hm & \(Ur valCc, hm) ->
              LHM.toList hm & \(Ur lst) ->
                Ur (valCc, lst)
  sortOn fst lst @?= sortOn fst [("aa", -10), ("cc", -10)]
  ccVal @?= Just (-10)

test_case5 :: TestTree
test_case5 = testCase "HashMap case 5" do
  let input = [("abcba", 1 :: Int), ("bacba", 2), ("aaba", 3), ("baa", 4)]
  let Ur (abcbaVal, lst) = withNewEmptyHashMap \hm ->
        LHM.insertMany input hm & \hm ->
          LHM.lookup "abcba" hm & \(Ur valCc, hm) ->
            LHM.toList hm & \(Ur lst) ->
              Ur (valCc, lst)
  sortOn fst lst @?= sortOn fst input
  abcbaVal @?= lookup "abcba" input

test_preparedInsertion :: TestTree
test_preparedInsertion = testCase "prepared insertion preserves lookup and growth" do
  let expected = Map.fromList [(show i, i) | i <- [1 .. 256 :: Int]]
      Ur (found, actual) = withNewEmptyHashMap (go 1)
  found @?= Just 128
  Map.fromList actual @?= expected
  where
    go :: Int -> LHM.HashMap String Int %1 -> Ur (Maybe Int, [(String, Int)])
    go i hm
      | i <= 256 = case LHM.lookupForInsert (show i) hm of
          (Ur (Left old), hm) -> go (i + 1) (LHM.alter (const (Just old)) (show i) hm)
          (Ur (Right plan), hm) -> go (i + 1) (LHM.unsafeInsertPrepared plan i hm)
      | otherwise = case LHM.lookupForInsert "128" hm of
          (Ur (Left value), hm) -> case LHM.toList hm of
            Ur entries -> Ur (Just value, entries)
          (Ur (Right plan), hm) -> case LHM.toList (LHM.unsafeInsertPrepared plan 0 hm) of
            Ur entries -> Ur (Nothing, entries)

-- | A key whose every value lands in the same bucket.
newtype Colliding = Colliding Int
  deriving (Show, Eq, Ord)

instance Hashable Colliding where
  hashWithSalt _ _ = 0

test_preparedInsertionCollisions :: TestTree
test_preparedInsertionCollisions = testCase "prepared insertion preserves Robin Hood collisions" do
  let expected = Map.fromList [(Colliding i, i) | i <- [1 .. 64]]
      Ur actual = linearly \lin -> go 1 (LHM.new 1 lin)
  Map.fromList actual @?= expected
  where
    go :: Int -> LHM.HashMap Colliding Int %1 -> Ur [(Colliding, Int)]
    go i hm
      | i <= 64 = case LHM.lookupForInsert (Colliding i) hm of
          (Ur (Left old), hm) -> go (i + 1) (LHM.alter (const (Just old)) (Colliding i) hm)
          (Ur (Right plan), hm) -> go (i + 1) (LHM.unsafeInsertPrepared plan i hm)
      | otherwise = LHM.toList hm

-- | An 'Int' hashes to itself, so multiples of 1024 have hashes that agree in their ten low bits, which a table must not pick its buckets by.
test_fromListKeepsKeysSharingLowBits :: TestTree
test_fromListKeepsKeysSharingLowBits = testCase "fromList keeps keys whose hashes share their low bits" do
  let input = [(i * 1024, i) | i <- [1 .. 200 :: Int]]
      Ur actual = linearly \lin -> LHM.toList (LHM.fromList input lin)
  Map.fromList actual @?= Map.fromList input

test_fromListKeepsKeysOfOneHash :: TestTree
test_fromListKeepsKeysOfOneHash = testCase "fromList keeps 128 keys of one hash" do
  let input = [(Colliding i, i) | i <- [1 .. 128]]
      Ur actual = linearly \lin -> LHM.toList (LHM.fromList input lin)
  Map.fromList actual @?= Map.fromList input

test_fromListRejectsTooManyKeysOfOneHash :: TestTree
test_fromListRejectsTooManyKeysOfOneHash = testCase "fromList raises on 129 keys of one hash" do
  assertFromListRaises [(Colliding i, i) | i <- [1 .. 129]]

-- | Whole-number 'Double's hash to their bit patterns, whose low bits are all zero.
test_fromListKeepsWholeNumberDoubles :: TestTree
test_fromListKeepsWholeNumberDoubles = testCase "fromList keeps whole-number Double keys" do
  let input = [(fromIntegral i :: Double, i) | i <- [1 .. 1000 :: Int]]
      Ur actual = linearly \lin -> LHM.toList (LHM.fromList input lin)
  Map.fromList actual @?= Map.fromList input

test_mixHashIsABijection :: TestTree
test_mixHashIsABijection = testCase "the hash mix and its inverse undo each other at both word sizes" do
  let samples64 = [0, 1, 2, 1024, maxBound, 0x0123456789abcdef] <> [i * 0x9e3779b97f4a7c15 | i <- [1 .. 1000]]
      samples32 = [0, 1, 2, 1024, maxBound, 0x01234567] <> [i * 0x9e3779b9 | i <- [1 .. 1000]]
  map (unmixWord64 . LHM.mixWord64) samples64 @?= samples64
  map (LHM.mixWord64 . unmixWord64) samples64 @?= samples64
  map (unmixWord32 . LHM.mixWord32) samples32 @?= samples32
  map (LHM.mixWord32 . unmixWord32) samples32 @?= samples32

test_fromListKeepsDisplacedEntries :: TestTree
test_fromListKeepsDisplacedEntries =
  testCase "fromList keeps an entry that a rehash displaces past the DIB limit" do
    -- At 512 buckets the run of bucket 255 spills into that of bucket 256 and pushes one of its entries past the limit.
    assertKeepsSteered [fp | t <- [0 .. 99], fp <- [255 + 512 * t, 256 + 512 * t]]

test_fromListKeepsEntriesAcrossANestedGrowth :: TestTree
test_fromListKeepsEntriesAcrossANestedGrowth =
  testCase "fromList keeps every entry when a rehash grows the table it is filling" do
    -- At 512 buckets the runs of buckets 255 and 256 merge and overflow halfway through the rehash, which goes on into a table of 1024.
    assertKeepsSteered
      ([768 + 1024 * j | j <- [0 .. 89]] <> [255 + 1024 * j | j <- [0 .. 89]] <> [101 + 1024 * j | j <- [0 .. 11]])

test_rehashGrowsByAllTheEntries :: TestTree
test_rehashGrowsByAllTheEntries =
  testCase "a rehash may grow the table it is filling as far as all the entries allow" do
    -- A table of 4096 buckets grows to 8192, where the runs of buckets 4095 and 4096 merge and overflow before the rehash is over, and only 16384 buckets separate them.
    -- That is within 64 buckets for each of the 260 entries, but beyond 64 for each of the 240 that the rehash has placed by then.
    assertBuildKeepsSteered
      (\kvs lin -> LHM.insertMany kvs (LHM.new 3000 lin))
      ([100 .. 210] <> [12288 + 16384 * j | j <- [0 .. 19]] <> [4095 + 8192 * j | j <- [0 .. 128]])

test_fromListKeepsKeysALargerTableSeparates :: TestTree
test_fromListKeepsKeysALargerTableSeparates =
  testCase "fromList keeps keys that only a table of eight times the buckets separates" do
    -- Every key shares bucket 0 up to 1024 buckets, and they split in two at 2048.
    assertKeepsSteered [i * 1024 | i <- [1 .. 200]]

test_fromListKeepsKeysAtTheDibLimit :: TestTree
test_fromListKeepsKeysAtTheDibLimit =
  testCase "fromList keeps keys that neighbours push to the DIB limit" do
    -- A run of one bucket's keys, a few keys of the next bucket behind it, then more keys of the first bucket, which displace those.
    forM_ [(a, b, c) | a <- [120 .. 128], b <- [0 .. 3], c <- [0 .. 3]] \(a, b, c) ->
      assertKeepsSteered
        ( [10 + 256 * t | t <- [0 .. a - 1]]
            <> [11 + 256 * t | t <- [0 .. b - 1]]
            <> [10 + 256 * (a + t) | t <- [0 .. c - 1]]
        )
    -- The displaced key of bucket 11 has to pass keys of buckets 11 and 12 that already sit at the limit.
    assertKeepsSteered ([10 + 256 * t | t <- [0 .. 126]] <> [11, 12, 268, 10 + 256 * 127])

test_fromListRejectsKeysNoGrowthSeparates :: TestTree
test_fromListRejectsKeysNoGrowthSeparates =
  testCase "fromList raises on keys built to collide in every table it may grow to" do
    -- The fingerprints agree in their twenty low bits.
    assertFromListRaises [(Steered (i * 2 ^ (20 :: Int)), i) | i <- [1 .. 129]]

test_fromListRaisesAfterDisplacing :: TestTree
test_fromListRaisesAfterDisplacing =
  testCase "fromList raises on keys built to collide when it meets them while displacing an entry" do
    -- 127 keys of bucket 0 and two of bucket 1, alike in their twenty low bits; a 128th key of bucket 0 then displaces one of bucket 1, which has nowhere to go.
    assertFromListRaises
      [ (Steered fp, fp)
      | fp <- [2 ^ (20 :: Int) * i | i <- [0 .. 126]] <> [1, 1 + 2 ^ (20 :: Int), 2 ^ (20 :: Int) * 127]
      ]

-- | Building a table from the list raises the error of too many colliding keys.
assertFromListRaises :: (Hashable k) => [(k, Int)] -> Assertion
assertFromListRaises input = do
  let Ur actual = linearly \lin -> LHM.toList (LHM.fromList input lin)
  result <- try @ErrorCall (evaluate (length actual))
  case result of
    Left (ErrorCall message) -> assertBool message ("too many keys collide" `isInfixOf` message)
    Right count -> assertFailure ("fromList returned " <> show count <> " entries")

-- | Build a table of steered keys with 'LHM.fromList' and check that it holds every one of them.
assertKeepsSteered :: [Int] -> Assertion
assertKeepsSteered = assertBuildKeepsSteered LHM.fromList

-- | Build a table of steered keys, given by their fingerprints, and check its size, a lookup of every key, and its entries.
assertBuildKeepsSteered ::
  ([(Steered, Int)] -> Linearly %1 -> LHM.HashMap Steered Int) -> [Int] -> Assertion
assertBuildKeepsSteered build fps = do
  map (LHM.fingerprint . Steered) fps @?= map LHM.Fingerprint fps
  let input = [(Steered fp, fp) | fp <- fps]
      Ur (count, found, entries) = linearly \lin ->
        build input lin & \hm ->
          LHM.size hm & \(Ur count, hm) ->
            lookupAll (map fst input) hm & \(Ur found, hm) ->
              LHM.toList hm & \(Ur entries) -> Ur (count, found, entries)
  count @?= length fps
  found @?= map (Just . snd) input
  Map.fromList entries @?= Map.fromList input

lookupAll :: (Hashable k) => [k] -> LHM.HashMap k v %1 -> (Ur [Maybe v], LHM.HashMap k v)
lookupAll [] hm = (Ur [], hm)
lookupAll (k : ks) hm = case LHM.lookup k hm of
  (Ur found, hm) -> case lookupAll ks hm of
    (Ur rest, hm) -> (Ur (found : rest), hm)

data Instruction
  = Inserts (NonEmpty (String, Int))
  | Insert String Int
  | Delete String
  deriving (Show, Eq, Ord, Generic)

instructionG :: F.Gen Instruction
instructionG =
  F.oneof $
    NE.fromList
      [ Insert
          <$> readableStringG
          <*> valG
      , Inserts . NE.fromList
          <$> F.list (F.between (1, 128)) ((,) <$> readableStringG <*> valG)
      ]

readableStringG :: F.Gen String
readableStringG = F.list (F.between (1, 5)) (F.elem $ 'a' :| "bc")

valG :: F.Gen Int
valG = F.int (F.between (-10, 10))

type Semantics = HMS.HashMap String Int

test_randomInstructions :: TestTree
test_randomInstructions = testProperty "random instructions" do
  program <- F.gen $ F.list (F.between (1, 256)) instructionG
  testInstructions program

-- | Run a random program against @unordered-containers@ as the oracle.
testInstructions :: [Instruction] -> Property ()
testInstructions instrs = do
  unur PL.$ linearly \lin ->
    go HMS.empty (LHM.new 16 lin) (pure ()) instrs
  where
    go ::
      Semantics ->
      LHM.HashMap String Int %1 ->
      Property () ->
      [Instruction] ->
      Ur (Property ())
    go sem !hm !act [] = case LHM.toList hm of
      Ur lst ->
        Ur $
          act
            *> F.assert (P.expect sem .$ ("Final dictionary", HMS.fromList lst))
    go sem !hm !act (instr : rest) = case instr of
      Insert k v ->
        let expectOld = HMS.lookup k sem
            sem' = HMS.insert k v sem
         in LHM.insert k v hm & \(Ur oldVal, hm) ->
              LHM.lookup k hm & \(Ur newVal, hm) ->
                let checks = do
                      F.collect "colliding insertion" [isJust expectOld]
                      F.assert $
                        P.expect expectOld
                          .$ ("before insert " <> show (k, v), oldVal)
                      F.assert $
                        P.expect (Just v)
                          .$ ("after insert " <> show (k, v), newVal)
                 in go sem' hm (act *> checks) rest
      Inserts kvs ->
        let sem' = foldl' (flip $ uncurry HMS.insert) sem kvs
            overlaps = map (\(k, _) -> HMS.member k sem) $ NE.toList kvs
            targKeys = HashSet.toList $ HashSet.fromList $ map fst $ NE.toList kvs
         in LHM.insertMany (NE.toList kvs) hm & \hm ->
              Lin.runState
                ( runUrT PL.$ forM targKeys \k -> UrT $ Lin.state \hm ->
                    BiL.first (Ur.lift (k,)) (LHM.lookup k hm)
                )
                hm
                & \(Ur lookups, hm) ->
                  let checks = do
                        F.label "bulk insertion size" [showSize $ NE.length kvs]
                        F.collect "colliding insertion" overlaps
                        forM_ lookups \(k, newVals) -> do
                          F.assert $
                            P.expect (HMS.lookup k sem')
                              .$ ("after bulk insert for " <> show k, newVals)
                   in go sem' hm (act *> checks) rest
      Delete k ->
        let expectOld = HMS.lookup k sem
            sem' = HMS.delete k sem
         in LHM.delete k hm & \(Ur oldVal, hm) ->
              LHM.lookup k hm & \(Ur newVal, hm) ->
                let checks = do
                      F.collect "vacuous deletion" [isNothing expectOld]
                      F.assert $
                        P.expect expectOld
                          .$ ("value before deletion " <> show k, oldVal)
                      F.assert $
                        P.expect Nothing
                          .$ ("value after deletion " <> show k, newVal)
                 in go sem' hm (act *> checks) rest

showSize :: Int -> String
showSize 0 = "0"
showSize i =
  let lb = floor @_ @Int (fromIntegral @_ @Double i / 10) * 10
      ub = lb + 10
   in "[" <> show lb <> ", " <> show ub <> ")"
