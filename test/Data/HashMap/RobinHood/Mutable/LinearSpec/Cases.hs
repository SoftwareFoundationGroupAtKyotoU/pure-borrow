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

{- NOTE: written with @case@ rather than as @DataFlow.do@, unlike 'case2'.

Every step here binds through a pattern that mentions 'Ur', and 'Ur' is
declared in GADT syntax, so GHC 9.10 rates such a pattern refutable in a @do@
statement and demands a `fail` that "Control.Syntax.DataFlow" does not
provide. GHC 9.12 accepts the same code. A @case@ alternative carries no such
rule, so this compiles on every supported compiler; 'case2' can keep its @do@
because it only ever binds plain variables.
-}
case3 :: HashMap String Int %1 -> Ur Case3Result
case3 hm = case HM.insert "1" 919 hm of
  (Ur iniOneReside, hm) -> case HM.lookup "1" hm of
    (Ur oneBeforeBulkInsert, hm) -> case HM.insertMany [(show i, i) | i <- [1 .. 128]] hm of
      hm -> case HM.lookup "1" hm of
        (Ur oneAfterBulkInsert, hm) -> case HM.lookup "16" hm of
          (Ur sixteenAfterBulkInsert, hm) ->
            case foldl'
              (\hm i -> move i & \(Ur i) -> uncurry lseq (HM.delete (show i) hm))
              hm
              [16 .. 256 :: Int] of
              hm -> case HM.lookup "16" hm of
                (Ur sixteenAfterBulkDelete, hm) -> case HM.insertMany [(show i, i) | i <- [129 .. 256]] hm of
                  hm -> case HM.insert "16" 9181 hm of
                    (Ur poppedSixteen, hm) -> case HM.lookup "16" hm of
                      (Ur finalSixteen, hm) -> case Map.fromList `Ur.lift` HM.toList hm of
                        Ur finalResult ->
                          let iniOneResideExpected = Nothing
                              oneBeforeBulkInsertExpected = Just 919
                              oneAfterBulkInsertExpected = Just 1
                              sixteenAfterBulkInsertExpected = Just 16
                              sixteenAfterBulkDeleteExpected = Nothing
                              poppedSixteenExpected = Nothing
                              finalSixteenExpected = Just 9181
                              expectedResult = Map.fromList $ [(show i, i) | i <- [2 .. 15] <> [129 .. 256]] <> [("1", 1), ("16", 9181)]
                           in Ur Case3Result {..}
