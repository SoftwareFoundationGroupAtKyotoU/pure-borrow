{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Specs of the owned Robin Hood hash table.
module Data.HashMap.RobinHood.Mutable.LinearSpec (
  module Data.HashMap.RobinHood.Mutable.LinearSpec,
) where

import Control.Functor.Linear qualified as Lin
import Control.Monad (forM)
import Control.Monad.Borrow.Pure (linearly)
import Data.Bifunctor.Linear qualified as BiL
import Data.Foldable (forM_)
import Data.HashMap.RobinHood.Mutable.Linear qualified as LHM
import Data.HashMap.RobinHood.Mutable.LinearSpec.Cases
import Data.HashMap.Strict qualified as HMS
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable (..))
import Data.List (sortOn)
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
