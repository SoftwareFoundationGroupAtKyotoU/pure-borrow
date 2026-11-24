{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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

module Data.Record.Linear.Borrow.Experimental.SplitSpec (
  module Data.Record.Linear.Borrow.Experimental.SplitSpec,
) where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure (borrowLinearlyM, linearly, pureAfter, reborrowing, reclaim, runBO)
import Data.List qualified as List
import Data.Record.Linear.Borrow.Experimental.Split (
  RecordLabel,
  SplittableRecord,
  splitRecord,
  (!#),
  (+#),
  (-#),
  (.#),
 )
import Data.Record.Linear.Borrow.Experimental.Split.TypingCases
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

data DisjointRec = DisjointRec {fstRef :: Ref Int, sndRef :: Ref Int}

deriveGeneric ''DisjointRec

deriving anyclass instance SplittableRecord DisjointRec

newtype AliasN = AliasN DisjointRec

deriving newtype instance Generic AliasN

deriving anyclass instance SplittableRecord AliasN

-- Malicious virtual HasField instances that both return sndRef.
instance HasField "fstRef" AliasN (Ref Int) where
  getField (AliasN r) = getField @"sndRef" r

instance HasField "sndRef" AliasN (Ref Int) where
  getField (AliasN r) = getField @"sndRef" r

data UrRec = UrRec {label :: Ur String, cell :: Ref Int}

deriveGeneric ''UrRec

deriving anyclass instance SplittableRecord UrRec

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

deriving anyclass instance SplittableRecord MixedRec

data DupA = DupA {sharedName :: Ref Int, onlyA :: Ref Int}

data DupB = DupB {sharedName :: Ref Int}

deriveGeneric ''DupA

deriveGeneric ''DupB

data EmptyRec = EmptyRec

deriveGeneric ''EmptyRec

deriving anyclass instance SplittableRecord EmptyRec

splitDisjointPair :: (Int, Int, Int, Int)
splitDisjointPair = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> DisjointRec (Ref.new 10 l1) (Ref.new 20 l2)
  let !(mx, rest) = splitRecord m -# #fstRef
      !my = rest !# #sndRef
  mx <- RefBorrow.modify (\n -> n `lseq` 11) mx
  my <- RefBorrow.modify (\n -> n `lseq` 22) my
  (vx, mx) <- reborrowing mx RefBorrow.copyRef
  (vy, my) <- reborrowing my RefBorrow.copyRef
  Control.pure (mx `lseq` consume my)
  pureAfter (case reclaim lend of DisjointRec rx ry -> (vx, vy, Ref.free rx, Ref.free ry))

newtypeGenericDisjoint :: (Int, Int)
newtypeGenericDisjoint = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> AliasN (DisjointRec (Ref.new 0 l1) (Ref.new 0 l2))
  let !(mx, rest) = splitRecord m -# #fstRef
      !my = rest !# #sndRef
  mx <- RefBorrow.modify (\n -> n `lseq` 1) mx
  (vx, mx) <- reborrowing mx RefBorrow.copyRef
  (vy, my) <- reborrowing my RefBorrow.copyRef
  Control.pure (mx `lseq` consume my)
  pureAfter (case reclaim lend of AliasN (DisjointRec rx ry) -> Ref.free rx `lseq` Ref.free ry `lseq` (vx, vy))

newtypeDotSharpReadsDeclared :: Int
newtypeDotSharpReadsDeclared = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> AliasN (DisjointRec (Ref.new 42 l1) (Ref.new 99 l2))
  let !mx = m .# #fstRef
  (vx, mx) <- reborrowing mx RefBorrow.copyRef
  Control.pure (consume mx)
  pureAfter (case reclaim lend of AliasN (DisjointRec rx ry) -> Ref.free rx `lseq` Ref.free ry `lseq` vx)

splitUrField :: (String, Int)
splitUrField = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM (\l -> UrRec (Ur "ok") (Ref.new 5 l))
  let !(Ur s, rest) = splitRecord m +# #label
      !mc = rest !# #cell
  mc <- RefBorrow.modify (+ 2) mc
  Control.pure (consume mc)
  pureAfter (case reclaim lend of UrRec (Ur _) r -> (s, Ref.free r))

splitNonlinearGeneric :: (String, Int)
splitNonlinearGeneric = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM (\l -> MixedRec "nl" (Ref.new 3 l))
  let !(mtag, rest) = splitRecord m -# #tag
      !mref = rest !# #ref
  mref <- RefBorrow.modify (+ 4) mref
  Control.pure (mtag `lseq` consume mref)
  pureAfter (case reclaim lend of MixedRec t r -> (t, Ref.free r))

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

consumeEmptyRecord :: ()
consumeEmptyRecord = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> l `lseq` EmptyRec
  Control.pure (consume (splitRecord m))
  pureAfter (case reclaim lend of EmptyRec -> ())

test_splitDeclaredFields :: TestTree
test_splitDeclaredFields =
  testGroup
    "Split accepts declared fields and ignores virtual HasField"
    [ testCase "splits disjoint declared fields" (splitDisjointPair @?= (11, 22, 11, 22))
    , testCase "newtype deriving Generic splits R's declared fields, not virtual HasField" (newtypeGenericDisjoint @?= (1, 0))
    , testCase "newtype deriving Generic (.#) reads R's declared field" (newtypeDotSharpReadsDeclared @?= 42)
    , testCase "skims Ur field and splits remaining Ref" (splitUrField @?= ("ok", 7))
    , testCase "splits record with MP1 nonlinear field" (splitNonlinearGeneric @?= ("nl", 7))
    , testCase "works with DuplicateRecordFields and NoFieldSelectors" (duplicateRecordFieldsResolve @?= (7, 9))
    , testCase "consumes empty single-constructor SplitRecord" (consumeEmptyRecord @?= ())
    ]

test_splitRecordRoles :: TestTree
test_splitRecordRoles =
  testGroup
    "SplitRecord and RecordLabel rejections"
    [ expectDeferredTypeError
        "cannot upcast SplitRecord to put back a field already split out"
        ["Couldn't match type: '[]", "arising from a use of"]
        badResetFields
    , expectDeferredTypeError
        "cannot upcast SplitRecord to a record whose field holds the record"
        ["Couldn't match type", "arising from a use of"]
        badViewRecord
    , expectDeferredTypeError
        "rejects virtual HasField #both"
        ["R2 declares no field", "both", "The fields that R2 declares are", "A HasField instance written by hand does not declare a field."]
        badVirtualBoth
    , expectDeferredTypeError
        "rejects virtual HasField in (-#)"
        ["Couldn't match type", "Nothing", "Just"]
        badVirtualSubtract
    , expectDeferredTypeError
        "rejects virtual HasField in (!#)"
        ["Couldn't match type", "Nothing", "Just"]
        badVirtualBang
    , expectDeferredTypeError
        "rejects virtual HasField in (+#)"
        ["Couldn't match type", "Nothing", "Just"]
        badVirtualPlus
    , expectDeferredTypeError
        "rejects double (-#) on the same field"
        ["Couldn't match type"]
        badDoubleSubtract
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
