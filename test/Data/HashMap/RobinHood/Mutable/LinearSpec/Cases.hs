{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Scripted mutation sequences shared by the Robin Hood hash table specs.
module Data.HashMap.RobinHood.Mutable.LinearSpec.Cases (
  module Data.HashMap.RobinHood.Mutable.LinearSpec.Cases,
) where

import Control.Monad.Borrow.Pure (linearly)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.HashMap.RobinHood.Mutable.Linear as HM
import Data.Map.Strict qualified as Map
import Data.Unrestricted.Linear qualified as Ur
import Prelude.Linear hiding (lookup)
import Prelude qualified as NonLinear

withNewEmptyHashMap :: (HashMap k v %1 -> Ur r) %1 -> Ur r
withNewEmptyHashMap f = linearly $ f . new 16

data Case1Result = Case1Result
  { initOneResident :: Maybe Int
  , newOneResident :: Maybe Int
  , initTwoResident :: Maybe Int
  , deletedOneResident :: Maybe Int
  , finalResult :: [(String, Int)]
  }
  deriving (Show, NonLinear.Eq, NonLinear.Ord)

-- | Insert, look up, insert again and delete, then materialize.
case1 :: HashMap String Int %1 -> Ur Case1Result
case1 hm =
  HM.insert "One" 1 hm & \(Ur initOneResident, hm) ->
    HM.lookup "One" hm & \(Ur newOneResident, hm) ->
      HM.insert "Two" 2 hm & \(Ur initTwoResident, hm) ->
        HM.delete "One" hm & \(Ur deletedOneResident, hm) ->
          HM.toList hm & \(Ur finalResult) ->
            Ur Case1Result {..}

-- | Bulk insert, bulk delete across a growth, then bulk insert again.
case2 :: HashMap String Int %1 -> Ur [(String, Int)]
case2 hm = DataFlow.do
  hm <- HM.insertMany [(show i, i) | i <- [1 .. 128]] hm
  hm <-
    foldl'
      (\hm i -> move i & \(Ur i) -> uncurry lseq (HM.delete (show i) hm))
      hm
      [16 .. 256 :: Int]
  hm <- HM.insertMany [(show i, i) | i <- [129 .. 256]] hm
  HM.toList hm

data Case3Result = Case3Result
  { iniOneReside :: Maybe Int
  , iniOneResideExpected :: Maybe Int
  , oneBeforeBulkInsert :: Maybe Int
  , oneBeforeBulkInsertExpected :: Maybe Int
  , oneAfterBulkInsert :: Maybe Int
  , oneAfterBulkInsertExpected :: Maybe Int
  , sixteenAfterBulkInsert :: Maybe Int
  , sixteenAfterBulkInsertExpected :: Maybe Int
  , sixteenAfterBulkDelete :: Maybe Int
  , sixteenAfterBulkDeleteExpected :: Maybe Int
  , poppedSixteen :: Maybe Int
  , poppedSixteenExpected :: Maybe Int
  , finalSixteen :: Maybe Int
  , finalSixteenExpected :: Maybe Int
  , finalResult :: Map.Map String Int
  , expectedResult :: Map.Map String Int
  }
  deriving (Show, NonLinear.Eq, NonLinear.Ord)

-- | Interleave lookups with the bulk mutations of 'case2'.
case3 :: HashMap String Int %1 -> Ur Case3Result
case3 hm = DataFlow.do
  (Ur iniOneReside, hm) <- HM.insert "1" 919 hm
  let iniOneResideExpected = Nothing
  (Ur oneBeforeBulkInsert, hm) <- HM.lookup "1" hm
  let oneBeforeBulkInsertExpected = Just 919
  hm <- HM.insertMany [(show i, i) | i <- [1 .. 128]] hm
  (Ur oneAfterBulkInsert, hm) <- HM.lookup "1" hm
  let oneAfterBulkInsertExpected = Just 1
  (Ur sixteenAfterBulkInsert, hm) <- HM.lookup "16" hm
  let sixteenAfterBulkInsertExpected = Just 16
  hm <-
    foldl'
      (\hm i -> move i & \(Ur i) -> uncurry lseq (HM.delete (show i) hm))
      hm
      [16 .. 256 :: Int]
  (Ur sixteenAfterBulkDelete, hm) <- HM.lookup "16" hm
  let sixteenAfterBulkDeleteExpected = Nothing
  hm <- HM.insertMany [(show i, i) | i <- [129 .. 256]] hm
  (Ur poppedSixteen, hm) <- HM.insert "16" 9181 hm
  let poppedSixteenExpected = Nothing
  (Ur finalSixteen, hm) <- HM.lookup "16" hm
  let finalSixteenExpected = Just 9181
  Ur finalResult <- Map.fromList `Ur.lift` HM.toList hm
  let expectedResult = Map.fromList $ [(show i, i) | i <- [2 .. 15] <> [129 .. 256]] <> [("1", 1), ("16", 9181)]
  Ur Case3Result {..}
