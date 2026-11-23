{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Record.Linear.Borrow.Experimental.PatternMatchSpec (
  module Data.Record.Linear.Borrow.Experimental.PatternMatchSpec,
) where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure (borrowLinearlyM, linearly, pureAfter, reborrowing, reclaim, runBO)
import Data.List qualified as List
import Data.Record.Linear.Borrow.Experimental.PatternMatch (
  LabelsOrBorrows (..),
  RecordLabel,
  (.#),
  (.@),
 )
import Data.Record.Linear.Borrow.Experimental.PatternMatch.TypingCases
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import GHC.Exts qualified as GHC
import GHC.Records (HasField (..))
import Generics.Linear (Generic (..))
import Generics.Linear qualified as GL
import Generics.Linear.TH (deriveGeneric)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

data Rec3 = Rec3 {a :: Ref Int, b :: Ref Int, c :: Ref Int}

deriveGeneric ''Rec3

newtype AliasN = AliasN Rec3

deriving newtype instance Generic AliasN

-- Malicious virtual HasField instances that both return b.
instance HasField "a" AliasN (Ref Int) where
  getField (AliasN r) = getField @"b" r

instance HasField "b" AliasN (Ref Int) where
  getField (AliasN r) = getField @"b" r

data MixedRec where
  MixedRec :: String -> Ref Int %1 -> MixedRec

instance Generic MixedRec where
  type
    Rep MixedRec =
      GL.D1
        GHC.Any
        ( GL.C1
            GHC.Any
            ( GL.S1 ('GL.MetaSel ('Just "tag") 'GL.NoSourceUnpackedness 'GL.NoSourceStrictness 'GL.DecidedLazy) (GL.MP1 'GHC.Many (GL.K1 GL.R String))
                GL.:*: GL.S1 ('GL.MetaSel ('Just "ref") 'GL.NoSourceUnpackedness 'GL.NoSourceStrictness 'GL.DecidedLazy) (GL.K1 GL.R (Ref Int))
            )
        )
  {-# INLINE [1] from #-}
  from r = fromMixed r
    where
      fromMixed :: MixedRec %1 -> Rep MixedRec p
      fromMixed (MixedRec t v) = GL.M1 (GL.M1 (GL.M1 (GL.MP1 (GL.K1 t)) GL.:*: GL.M1 (GL.K1 v)))
  {-# INLINE [1] to #-}
  to rep = toMixed rep
    where
      toMixed :: Rep MixedRec p %1 -> MixedRec
      toMixed (GL.M1 (GL.M1 (GL.M1 (GL.MP1 (GL.K1 t)) GL.:*: GL.M1 (GL.K1 v)))) = MixedRec t v

data DupA = DupA {sharedName :: Ref Int, onlyA :: Ref Int}

data DupB = DupB {sharedName :: Ref Int}

deriveGeneric ''DupA

deriveGeneric ''DupB

splitTupleDeclared :: (Int, Int, Int, Int)
splitTupleDeclared = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup3 l of
    (l1, l2, l3) -> Rec3 (Ref.new 1 l1) (Ref.new 2 l2) (Ref.new 3 l3)
  let !(mc, ma) = m .@ (#c, #a)
  ma <- RefBorrow.modify (\n -> n `lseq` 10) ma
  mc <- RefBorrow.modify (\n -> n `lseq` 30) mc
  (va, ma) <- reborrowing ma RefBorrow.copyRef
  (vc, mc) <- reborrowing mc RefBorrow.copyRef
  Control.pure (ma `lseq` consume mc)
  pureAfter (case reclaim lend of Rec3 ra rb rc -> Ref.free rb `lseq` (va, vc, Ref.free ra, Ref.free rc))

splitHListDeclared :: (Int, Int)
splitHListDeclared = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup3 l of
    (l1, l2, l3) -> Rec3 (Ref.new 1 l1) (Ref.new 2 l2) (Ref.new 3 l3)
  let !(mb :#- ma :#- RNil) = m .@ #b :#- #a :#- RNil
  ma <- RefBorrow.modify (+ 10) ma
  mb <- RefBorrow.modify (+ 20) mb
  Control.pure (ma `lseq` consume mb)
  pureAfter (case reclaim lend of Rec3 ra rb rc -> Ref.free rc `lseq` (Ref.free ra, Ref.free rb))

newtypeGenericDisjoint :: (Int, Int)
newtypeGenericDisjoint = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup3 l of
    (l1, l2, l3) -> AliasN (Rec3 (Ref.new 0 l1) (Ref.new 0 l2) (Ref.new 0 l3))
  let !(ma, mb) = m .@ (#a, #b)
  ma <- RefBorrow.modify (\n -> n `lseq` 1) ma
  (va, ma) <- reborrowing ma RefBorrow.copyRef
  (vb, mb) <- reborrowing mb RefBorrow.copyRef
  Control.pure (ma `lseq` consume mb)
  pureAfter (case reclaim lend of AliasN (Rec3 ra rb rc) -> Ref.free ra `lseq` Ref.free rb `lseq` Ref.free rc `lseq` (va, vb))

newtypeDotSharpReadsDeclared :: Int
newtypeDotSharpReadsDeclared = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup3 l of
    (l1, l2, l3) -> AliasN (Rec3 (Ref.new 42 l1) (Ref.new 99 l2) (Ref.new 0 l3))
  let !ma = m .# #a
  va <- RefBorrow.copyRef ma
  pureAfter (case reclaim lend of AliasN (Rec3 ra rb rc) -> Ref.free ra `lseq` Ref.free rb `lseq` Ref.free rc `lseq` va)

splitNonlinearGeneric :: Int
splitNonlinearGeneric = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM (\l -> MixedRec "nl" (Ref.new 3 l))
  let !(mtag, mref) = m .@ (#tag, #ref)
  mref <- RefBorrow.modify (+ 4) mref
  Control.pure (mtag `lseq` consume mref)
  pureAfter (case reclaim lend of MixedRec _ r -> Ref.free r)

duplicateRecordFieldsResolve :: (Int, Int)
duplicateRecordFieldsResolve = linearly \lin -> runBO lin Control.do
  (ma0, lendA) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> DupA (Ref.new 7 l1) (Ref.new 8 l2)
  (mb0, lendB) <- borrowLinearlyM (DupB . Ref.new 9)
  let !labA = #sharedName :: RecordLabel DupA "sharedName" (Ref Int)
      !labB = #sharedName :: RecordLabel DupB "sharedName" (Ref Int)
      !ma = ma0 .# labA
      !mb = mb0 .# labB
  va <- RefBorrow.copyRef ma
  vb <- RefBorrow.copyRef mb
  pureAfter
    ( case reclaim lendA of
        DupA r1 r2 -> case reclaim lendB of
          DupB r3 -> Ref.free r1 `lseq` Ref.free r2 `lseq` Ref.free r3 `lseq` (va, vb)
    )

test_patternMatchDeclaredFields :: TestTree
test_patternMatchDeclaredFields =
  testGroup
    "PatternMatch accepts declared fields and ignores virtual HasField"
    [ testCase "splits tuple of declared fields" (splitTupleDeclared @?= (10, 30, 10, 30))
    , testCase "splits RecordLabels list of declared fields" (splitHListDeclared @?= (11, 22))
    , testCase "newtype deriving Generic splits Rec3's declared fields, not virtual HasField" (newtypeGenericDisjoint @?= (1, 0))
    , testCase "newtype deriving Generic (.#) reads Rec3's declared field" (newtypeDotSharpReadsDeclared @?= 42)
    , testCase "splits record with MP1 nonlinear field" (splitNonlinearGeneric @?= 7)
    , testCase "works with DuplicateRecordFields and NoFieldSelectors" (duplicateRecordFieldsResolve @?= (7, 9))
    ]

test_patternMatchRejections :: TestTree
test_patternMatchRejections =
  testGroup
    "PatternMatch rejections"
    [ expectDeferredTypeError
        "rejects virtual HasField #zAlias in (.@)"
        ["R2 declares no field", "zAlias", "The fields that R2 declares are", "A HasField instance written by hand does not declare a field."]
        badVirtualZAlias
    , expectDeferredTypeError
        "rejects virtual HasField #zAlias in (.#)"
        ["R2 declares no field", "zAlias", "The fields that R2 declares are", "A HasField instance written by hand does not declare a field."]
        badVirtualDotSharp
    , expectDeferredTypeError
        "rejects record without linear-generics Generic"
        ["The fields of NoGen are unknown, since it has no instance of linear-generics' Generic.", "deriveGeneric"]
        badNoGeneric
    , expectDeferredTypeError
        "rejects sum type"
        ["A borrow of a type with more than one constructor cannot be split by a field."]
        badSumType
    , expectDeferredTypeError
        "cannot coerce RecordLabel to another field"
        ["Couldn't match"]
        badCoerceField
    , expectDeferredTypeError
        "cannot coerce RecordLabel to another record"
        ["Couldn't match"]
        badCoerceRecord
    , expectDeferredTypeError
        "cannot coerce RecordLabel to another field type"
        ["Couldn't match"]
        badCoerceType
    ]
  where
    expectDeferredTypeError description expectedFragments value =
      testCase description do
        result <- Exception.try @Exception.SomeException (Exception.evaluate value)
        case result of
          Left exception ->
            assertBool
              ("unexpected deferred type error: " <> Exception.displayException exception)
              (List.all (`List.isInfixOf` Exception.displayException exception) expectedFragments)
          Right _ ->
            assertFailure
              ("expected deferred type error containing " <> List.intercalate " and " expectedFragments)
