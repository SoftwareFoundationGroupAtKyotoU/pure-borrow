{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 #-}

module PureBorrow.Inspection.RecordSplit (
  test_recordSplit,
  Rec8 (..),
  splitDotSharp,
  refDotSharp,
  splitBang,
  pmDotSharp,
  refPMDotSharp,
  pmTuple,
  refPMTuple,
  firstClassLabel,
) where

import Control.Monad.Borrow.Pure.BO (Mut)
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias), unsafeMapAlias)
import Data.Record.Linear.Borrow.Experimental.PatternMatch qualified as PM
import Data.Record.Linear.Borrow.Experimental.Split qualified as Split
import GHC.Records (getField)
import Generics.Linear.TH (deriveGeneric)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection
import Unsafe.Linear qualified as Unsafe

-- The obligations cover both the Generic instances derived here and the Split and PatternMatch projection Core.
data Rec8 = Rec8
  { f1 :: Int
  , f2 :: Int
  , f3 :: Int
  , f4 :: Int
  , f5 :: Int
  , f6 :: Int
  , f7 :: Int
  , f8 :: Int
  }

deriveGeneric ''Rec8

deriving anyclass instance Split.SplittableRecord Rec8

splitDotSharp :: Mut α Rec8 %1 -> Mut α Int
{-# NOINLINE splitDotSharp #-}
splitDotSharp m = m Split..# #f6

refDotSharp :: Mut α Rec8 %1 -> Mut α Int
{-# NOINLINE refDotSharp #-}
refDotSharp (UnsafeAlias !r) = UnsafeAlias $! Unsafe.toLinear (getField @"f6") r

splitBang :: Mut α Rec8 %1 -> Mut α Int
{-# NOINLINE splitBang #-}
splitBang m = Split.splitRecord m Split.!# #f6

pmDotSharp :: Mut α Rec8 %1 -> Mut α Int
{-# NOINLINE pmDotSharp #-}
pmDotSharp m = m PM..# #f6

refPMDotSharp :: Mut α Rec8 %1 -> Mut α Int
{-# NOINLINE refPMDotSharp #-}
refPMDotSharp = unsafeMapAlias (Unsafe.toLinear (getField @"f6"))

pmTuple :: Mut α Rec8 %1 -> (Mut α Int, Mut α Int)
{-# NOINLINE pmTuple #-}
pmTuple m = m PM..@ (#f2, #f6)

refPMTuple :: Mut α Rec8 %1 -> (Mut α Int, Mut α Int)
{-# NOINLINE refPMTuple #-}
refPMTuple = Unsafe.toLinear \r ->
  ( unsafeMapAlias (Unsafe.toLinear (getField @"f2")) r
  , unsafeMapAlias (Unsafe.toLinear (getField @"f6")) r
  )

firstClassLabel :: Split.RecordLabel Rec8 "f6" Int
{-# NOINLINE firstClassLabel #-}
firstClassLabel = #f6

test_recordSplit :: TestTree
test_recordSplit =
  testGroup
    "record label projections compile away Generic representations"
    [ $(inspectTest ((hasNoGenerics 'splitDotSharp) {testName = Just "Split (.#) leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'splitDotSharp) {testName = Just "Split (.#) leaves no type-class dictionaries"}))
    , $(inspectTest (('splitDotSharp ==- 'refDotSharp) {testName = Just "Split (.#) matches HasField.getField"}))
    , $(inspectTest ((hasNoGenerics 'splitBang) {testName = Just "Split (!#) leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'splitBang) {testName = Just "Split (!#) leaves no type-class dictionaries"}))
    , $(inspectTest (('splitBang ==- 'refDotSharp) {testName = Just "Split (!#) matches HasField.getField"}))
    , $(inspectTest ((hasNoGenerics 'pmDotSharp) {testName = Just "PatternMatch (.#) leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'pmDotSharp) {testName = Just "PatternMatch (.#) leaves no type-class dictionaries"}))
    , $(inspectTest (('pmDotSharp ==- 'refPMDotSharp) {testName = Just "PatternMatch (.#) matches HasField.getField"}))
    , $(inspectTest ((hasNoGenerics 'pmTuple) {testName = Just "PatternMatch (.@) tuple leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'pmTuple) {testName = Just "PatternMatch (.@) tuple leaves no type-class dictionaries"}))
    , $(inspectTest (('pmTuple ==- 'refPMTuple) {testName = Just "PatternMatch (.@) tuple matches HasField.getField"}))
    , $(inspectTest ((hasNoGenerics 'firstClassLabel) {testName = Just "first-class RecordLabel leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'firstClassLabel) {testName = Just "first-class RecordLabel leaves no type-class dictionaries"}))
    ]
