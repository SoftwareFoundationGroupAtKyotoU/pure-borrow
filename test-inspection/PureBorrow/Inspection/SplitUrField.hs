{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 #-}

module PureBorrow.Inspection.SplitUrField (
  test_splitUrField,
  Boxed (..),
  splitBoxed,
  refBoxed,
  Labelled (..),
  splitLabelled,
  refLabelled,
  Entry (..),
  splitEntry,
  refEntry,
  Wide (..),
  splitWide,
  refWide,
  Single (..),
  splitSingle,
) where

import Control.Monad.Borrow.Pure.BO (DistributesAlias, Mut, split)
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias))
import Data.Kind (Type)
import Generics.Linear qualified as GL
import Generics.Linear.TH (deriveGenericAnd1)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

-- The obligations are about the instances derived here, so the types share this module.
-- Each reference does by hand what split does: evaluate every field of a product, and rebuild the constructor around the same 'Ur'.

-- A lazy field of type 'Ur' beside an element.
data Boxed a = Boxed (Ur String) a

deriveGenericAnd1 ''Boxed

deriving anyclass instance DistributesAlias Boxed

splitBoxed :: Mut α (Boxed a) %1 -> Boxed (Mut α a)
{-# NOINLINE splitBoxed #-}
splitBoxed = split

refBoxed :: Mut α (Boxed a) %1 -> Boxed (Mut α a)
refBoxed (UnsafeAlias (Boxed !u !a)) = Boxed u (UnsafeAlias a)

-- A strict field of type 'Ur' beside an element.
data Labelled a = Labelled !(Ur String) a

deriveGenericAnd1 ''Labelled

deriving anyclass instance DistributesAlias Labelled

splitLabelled :: Mut α (Labelled a) %1 -> Labelled (Mut α a)
{-# NOINLINE splitLabelled #-}
splitLabelled = split

refLabelled :: Mut α (Labelled a) %1 -> Labelled (Mut α a)
refLabelled (UnsafeAlias (Labelled u !a)) = Labelled u (UnsafeAlias a)

-- A sum, one of whose constructors has only a field of type 'Ur'.
data Entry a = Present (Ur Int) a | Absent (Ur String) | Several (Ur Int) [a]

deriveGenericAnd1 ''Entry

deriving anyclass instance DistributesAlias Entry

splitEntry :: Mut α (Entry a) %1 -> Entry (Mut α a)
{-# NOINLINE splitEntry #-}
splitEntry = split

refEntry :: Mut α (Entry a) %1 -> Entry (Mut α a)
refEntry (UnsafeAlias (Present !u !a)) = Present u (UnsafeAlias a)
refEntry (UnsafeAlias (Absent !u)) = Absent u
refEntry (UnsafeAlias (Several !u !as)) = case split (UnsafeAlias as) of !bs -> Several u bs

-- Twelve fields, six of them of type 'Ur', strict and lazy mixed.
data Wide a
  = Wide !(Ur Int) a (Ur Int) !a !(Ur Int) a (Ur Int) !a !(Ur Int) a (Ur Int) !a

deriveGenericAnd1 ''Wide

deriving anyclass instance DistributesAlias Wide

splitWide :: Mut α (Wide a) %1 -> Wide (Mut α a)
{-# NOINLINE splitWide #-}
splitWide = split

refWide :: Mut α (Wide a) %1 -> Wide (Mut α a)
refWide (UnsafeAlias (Wide u1 !a1 !u2 a2 u3 !a3 !u4 a4 u5 !a5 !u6 a6)) =
  Wide u1 (UnsafeAlias a1) u2 (UnsafeAlias a2) u3 (UnsafeAlias a3) u4 (UnsafeAlias a4) u5 (UnsafeAlias a5) u6 (UnsafeAlias a6)

-- The only field of the only constructor, which split does not evaluate.
data Single (a :: Type) = Single (Ur Int)

deriveGenericAnd1 ''Single

deriving anyclass instance DistributesAlias Single

splitSingle :: Mut α (Single a) %1 -> Single (Mut α a)
{-# NOINLINE splitSingle #-}
splitSingle = split

test_splitUrField :: TestTree
test_splitUrField =
  testGroup
    "split over a linear field of type Ur"
    [ $(inspectTest ((hasNoGenerics 'splitBoxed) {testName = Just "a lazy field leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'splitBoxed) {testName = Just "a lazy field leaves no dictionary"}))
    , $(inspectTest (('splitBoxed ==- 'refBoxed) {testName = Just "a lazy field is kept as it is"}))
    , $(inspectTest ((hasNoGenerics 'splitLabelled) {testName = Just "a strict field leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'splitLabelled) {testName = Just "a strict field leaves no dictionary"}))
    , $(inspectTest (('splitLabelled ==- 'refLabelled) {testName = Just "a strict field is kept as it is"}))
    , $(inspectTest (('splitLabelled `doesNotUse` 'Ur) {testName = Just "a strict field is not boxed again"}))
    , $(inspectTest ((hasNoGenerics 'splitEntry) {testName = Just "a sum leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'splitEntry) {testName = Just "a sum leaves no dictionary"}))
    , $(inspectTest (('splitEntry ==- 'refEntry) {testName = Just "a sum keeps each field as it is"}))
    , $(inspectTest ((hasNoType 'splitEntry ''(GL.:.:)) {testName = Just "a sum leaves no composition"}))
    , $(inspectTest ((hasNoGenerics 'splitWide) {testName = Just "twelve fields leave no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'splitWide) {testName = Just "twelve fields leave no dictionary"}))
    , $(inspectTest (('splitWide ==- 'refWide) {testName = Just "twelve fields are kept as they are"}))
    , $(inspectTest ((hasNoGenerics 'splitSingle) {testName = Just "a lone field leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'splitSingle) {testName = Just "a lone field leaves no dictionary"}))
    , $(inspectTest (('splitSingle `doesNotUse` 'Ur) {testName = Just "a lone field is not boxed again"}))
    ]
