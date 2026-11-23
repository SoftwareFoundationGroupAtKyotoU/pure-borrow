{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.Borrow.Pure.BO.SplitSpec (
  module Control.Monad.Borrow.Pure.BO.SplitSpec,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import GHC.Exts qualified as GHC
import Generics.Linear
import Generics.Linear.TH (deriveGenericAnd1)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude qualified as NonLinear

-- An element beside a nonlinear field whose type does not mention the parameter.
-- The label is lazy, so that a test can tell whether 'split' forces it.
data Labelled a where
  Labelled :: String -> a %1 -> Labelled a

instance Generic1 Labelled where
  type
    Rep1 Labelled =
      D1 GHC.Any (C1 GHC.Any (S1 GHC.Any (MP1 'GHC.Many (K1 R String)) :*: S1 GHC.Any Par1))
  {-# INLINE [1] from1 #-}
  from1 labelled = fromLabelled labelled
    where
      fromLabelled :: Labelled a %1 -> Rep1 Labelled a
      fromLabelled (Labelled label a) = M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (Par1 a)))
  {-# INLINE [1] to1 #-}
  to1 rep = toLabelled rep
    where
      toLabelled :: Rep1 Labelled a %1 -> Labelled a
      toLabelled (M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (Par1 a)))) = Labelled label a

deriving anyclass instance DistributesAlias Labelled

-- One constructor has an element beside a nonlinear field, the other only a nonlinear field.
data Entry a where
  Present :: Int -> a %1 -> Entry a
  Absent :: String -> Entry a

instance Generic1 Entry where
  type
    Rep1 Entry =
      D1
        GHC.Any
        ( C1 GHC.Any (S1 GHC.Any (MP1 'GHC.Many (K1 R Int)) :*: S1 GHC.Any Par1)
            :+: C1 GHC.Any (S1 GHC.Any (MP1 'GHC.Many (K1 R String)))
        )
  {-# INLINE [1] from1 #-}
  from1 entry = fromEntry entry
    where
      fromEntry :: Entry a %1 -> Rep1 Entry a
      fromEntry (Present tag a) = M1 (L1 (M1 (M1 (MP1 (K1 tag)) :*: M1 (Par1 a))))
      fromEntry (Absent reason) = M1 (R1 (M1 (M1 (MP1 (K1 reason)))))
  {-# INLINE [1] to1 #-}
  to1 rep = toEntry rep
    where
      toEntry :: Rep1 Entry a %1 -> Entry a
      toEntry (M1 (L1 (M1 (M1 (MP1 (K1 tag)) :*: M1 (Par1 a))))) = Present tag a
      toEntry (M1 (R1 (M1 (M1 (MP1 (K1 reason)))))) = Absent reason

deriving anyclass instance DistributesAlias Entry

entryValue :: Entry (Ref Int) %1 -> Int
entryValue (Present _ ref) = Ref.free ref
entryValue (Absent _) = 0

-- An element beside a nonlinear field of a type that has no instance of any class.
data Scaled a where
  Scaled :: (Int -> Int) -> a %1 -> Scaled a

instance Generic1 Scaled where
  type
    Rep1 Scaled =
      D1 GHC.Any (C1 GHC.Any (S1 GHC.Any (MP1 'GHC.Many (K1 R (Int -> Int))) :*: S1 GHC.Any Par1))
  {-# INLINE [1] from1 #-}
  from1 scaled = fromScaled scaled
    where
      fromScaled :: Scaled a %1 -> Rep1 Scaled a
      fromScaled (Scaled scale a) = M1 (M1 (M1 (MP1 (K1 scale)) :*: M1 (Par1 a)))
  {-# INLINE [1] to1 #-}
  to1 rep = toScaled rep
    where
      toScaled :: Rep1 Scaled a %1 -> Scaled a
      toScaled (M1 (M1 (M1 (MP1 (K1 scale)) :*: M1 (Par1 a)))) = Scaled scale a

deriving anyclass instance DistributesAlias Scaled

-- Write through the element of a split 'Mut', then compare the label of the split result with the lender's.
bumpThroughMut :: (String, String, Int)
bumpThroughMut = linearly \lin -> runBO lin Control.do
  (labelled, lend) <- borrowLinearlyM (\l -> Labelled "label" (Ref.new 1 l))
  case split labelled of
    Labelled label ref -> Control.do
      ref <- RefBorrow.modify (+ 41) ref
      Control.pure (consume ref)
      pureAfter
        ( case reclaim lend of
            Labelled label' ref' -> (label, label', Ref.free ref')
        )

-- Split one 'Share' twice and read through both results.
readThroughShares :: (String, Int, Int)
readThroughShares = linearly \lin -> runBO lin Control.do
  (labelled, lend) <- borrowLinearlyM (\l -> Labelled "label" (Ref.new 7 l))
  case share labelled of
    Ur shared -> case split shared of
      Labelled label ref1 -> case split shared of
        Labelled _ ref2 -> Control.do
          Ur first <- move Control.<$> RefBorrow.copyRef ref1
          Ur second <- move Control.<$> RefBorrow.copyRef ref2
          pureAfter
            ( case reclaim lend of
                Labelled _ ref -> ref `lseq` (label, first, second)
            )

-- Split the 'Lend' beside the live 'Mut', and reclaim only the element at the end.
splitLendEarly :: (String, Int)
splitLendEarly = linearly \lin -> runBO lin Control.do
  (labelled, lend) <- borrowLinearlyM (\l -> Labelled "label" (Ref.new 1 l))
  case split lend of
    Labelled label lendRef -> case split labelled of
      Labelled _ ref -> Control.do
        ref <- RefBorrow.modify (+ 1) ref
        Control.pure (consume ref)
        pureAfter (label, Ref.free (reclaim lendRef))

-- The label is an error that nothing may force.
labelStaysUnforced :: Int
labelStaysUnforced = linearly \lin -> runBO lin Control.do
  (labelled, lend) <-
    borrowLinearlyM (\l -> Labelled (NonLinear.error "split forced the label") (Ref.new 5 l))
  case split labelled of
    Labelled _ ref -> Control.do
      ref <- RefBorrow.modify (+ 1) ref
      Control.pure (consume ref)
      pureAfter
        ( case reclaim lend of
            Labelled _ ref' -> Ref.free ref'
        )

splitPresent :: (Int, Int)
splitPresent = linearly \lin -> runBO lin Control.do
  (entry, lend) <- borrowLinearlyM (\l -> Present 3 (Ref.new 10 l))
  case split entry of
    Present tag ref -> Control.do
      ref <- RefBorrow.modify (+ tag) ref
      Control.pure (consume ref)
      pureAfter (tag, entryValue (reclaim lend))
    Absent _ -> pureAfter (-1, entryValue (reclaim lend))

splitAbsent :: String
splitAbsent = linearly \lin -> runBO lin Control.do
  (entry, lend) <- borrowLinearlyM (\l -> l `lseq` (Absent "none" :: Entry (Ref Int)))
  case split entry of
    Absent reason -> pureAfter (entryValue (reclaim lend) `lseq` reason)
    Present _ ref -> Control.do
      Control.pure (consume ref)
      pureAfter (entryValue (reclaim lend) `lseq` "present")

-- The 'Lend' of a constructor without elements is used up by the split.
splitLendOfAbsent :: String
splitLendOfAbsent = linearly \lin -> runBO lin Control.do
  (entry, lend) <- borrowLinearlyM (\l -> l `lseq` (Absent "none" :: Entry (Ref Int)))
  Control.pure (consume entry)
  case split lend of
    Absent reason -> pureAfter reason
    Present _ lendRef -> pureAfter (Ref.free (reclaim lendRef) `lseq` "present")

-- The kept field is a function, which has no instance for the split to lean on.
applyKeptFunction :: Int
applyKeptFunction = linearly \lin -> runBO lin Control.do
  (scaled, lend) <- borrowLinearlyM (\l -> Scaled (NonLinear.* 3) (Ref.new 5 l))
  case split scaled of
    Scaled scale ref -> Control.do
      ref <- RefBorrow.modify (\n -> case move n of Ur n -> scale n) ref
      Control.pure (consume ref)
      pureAfter
        ( case reclaim lend of
            Scaled _ ref' -> Ref.free ref'
        )

test_splitKeepsNonlinearFields :: TestTree
test_splitKeepsNonlinearFields =
  testGroup
    "split keeps a nonlinear field whose type does not mention the parameter"
    [ testCase "a Mut is split around the field" (bumpThroughMut @?= ("label", "label", 42))
    , testCase "a Share is split twice" (readThroughShares @?= ("label", 7, 7))
    , testCase "a Lend is split while the Mut is live" (splitLendEarly @?= ("label", 2))
    , testCase "the field is not forced" (labelStaysUnforced @?= 6)
    , testCase "a constructor with an element" (splitPresent @?= (3, 13))
    , testCase "a constructor with nonlinear fields only" (splitAbsent @?= "none")
    , testCase "a Lend of a constructor with nonlinear fields only" (splitLendOfAbsent @?= "none")
    , testCase "a field of a type without instances" (applyKeptFunction @?= 15)
    ]

-- A lazy linear field of type 'Ur' beside an element, with the instance of linear-generics' Template Haskell.
data Boxed a = Boxed (Ur String) a

deriveGenericAnd1 ''Boxed

deriving anyclass instance DistributesAlias Boxed

-- One constructor has an element beside a field of type 'Ur', the other only a field of type 'Ur'.
data BoxedEntry a = BoxedPresent !(Ur Int) a | BoxedAbsent !(Ur String)

deriveGenericAnd1 ''BoxedEntry

deriving anyclass instance DistributesAlias BoxedEntry

boxedEntryValue :: BoxedEntry (Ref Int) %1 -> Int
boxedEntryValue (BoxedPresent (Ur _) ref) = Ref.free ref
boxedEntryValue (BoxedAbsent (Ur _)) = 0

-- Write through the element of a split 'Mut', then compare the box of the split result with the lender's.
bumpThroughMutBoxed :: (String, String, Int)
bumpThroughMutBoxed = linearly \lin -> runBO lin Control.do
  (boxed, lend) <- borrowLinearlyM (\l -> Boxed (Ur "label") (Ref.new 1 l))
  case split boxed of
    Boxed (Ur label) ref -> Control.do
      ref <- RefBorrow.modify (+ 41) ref
      Control.pure (consume ref)
      pureAfter
        ( case reclaim lend of
            Boxed (Ur label') ref' -> (label, label', Ref.free ref')
        )

-- Split the 'Lend' beside the live 'Mut', and reclaim only the element at the end.
splitLendEarlyBoxed :: (String, Int)
splitLendEarlyBoxed = linearly \lin -> runBO lin Control.do
  (boxed, lend) <- borrowLinearlyM (\l -> Boxed (Ur "label") (Ref.new 1 l))
  case split lend of
    Boxed (Ur label) lendRef -> case split boxed of
      Boxed (Ur _) ref -> Control.do
        ref <- RefBorrow.modify (+ 1) ref
        Control.pure (consume ref)
        pureAfter (label, Ref.free (reclaim lendRef))

-- The contents of the box are an error that nothing may force.
contentsStayUnforced :: Int
contentsStayUnforced = linearly \lin -> runBO lin Control.do
  (boxed, lend) <-
    borrowLinearlyM (\l -> Boxed (Ur (NonLinear.error "split forced the contents")) (Ref.new 5 l))
  case split boxed of
    Boxed (Ur _) ref -> Control.do
      ref <- RefBorrow.modify (+ 1) ref
      Control.pure (consume ref)
      pureAfter
        ( case reclaim lend of
            Boxed u ref' -> u `lseq` Ref.free ref'
        )

-- A box that writes to a reference made outside it, so that a second run would show as a second 'x'.
boxOverOwnerLendFirst :: (String, String, Int)
boxOverOwnerLendFirst = linearly \lin -> runBO lin Control.do
  (boxed, lend) <-
    borrowLinearlyM
      ( \l -> case dup2 l of
          (l1, l2) -> case Ref.new "abel" l1 of
            !r -> Boxed (move (Ref.free (Ref.atomicModify_ (\cs -> 'x' : cs) r))) (Ref.new 10 l2)
      )
  case split lend of
    Boxed (Ur early) lendRef -> case split boxed of
      Boxed (Ur viaMut) ref -> Control.do
        ref <- RefBorrow.modify (+ 1) ref
        Control.pure (consume ref)
        pureAfter (early, viaMut, Ref.free (reclaim lendRef))

-- The same box, split through one 'Share' twice and then reclaimed.
boxOverOwnerSharedTwice :: (String, String, String)
boxOverOwnerSharedTwice = linearly \lin -> runBO lin Control.do
  (boxed, lend) <-
    borrowLinearlyM
      ( \l -> case dup2 l of
          (l1, l2) -> case Ref.new "abel" l1 of
            !r -> Boxed (move (Ref.free (Ref.atomicModify_ (\cs -> 'x' : cs) r))) (Ref.new (10 :: Int) l2)
      )
  case share boxed of
    Ur shared -> case split shared of
      Boxed (Ur first) _ -> case split shared of
        Boxed (Ur second) _ ->
          pureAfter
            ( case reclaim lend of
                Boxed (Ur atEnd) ref -> ref `lseq` (first, second, atEnd)
            )

splitBoxedPresent :: (Int, Int)
splitBoxedPresent = linearly \lin -> runBO lin Control.do
  (entry, lend) <- borrowLinearlyM (\l -> BoxedPresent (Ur 3) (Ref.new 10 l))
  case split entry of
    BoxedPresent (Ur tag) ref -> Control.do
      ref <- RefBorrow.modify (+ tag) ref
      Control.pure (consume ref)
      pureAfter (tag, boxedEntryValue (reclaim lend))
    BoxedAbsent (Ur _) -> pureAfter (-1, boxedEntryValue (reclaim lend))

-- The 'Lend' of a constructor without elements is used up by the split.
splitLendOfBoxedAbsent :: String
splitLendOfBoxedAbsent = linearly \lin -> runBO lin Control.do
  (entry, lend) <- borrowLinearlyM (\l -> l `lseq` (BoxedAbsent (Ur "none") :: BoxedEntry (Ref Int)))
  Control.pure (consume entry)
  case split lend of
    BoxedAbsent (Ur reason) -> pureAfter reason
    BoxedPresent (Ur _) lendRef -> pureAfter (Ref.free (reclaim lendRef) `lseq` "present")

test_splitKeepsUrFields :: TestTree
test_splitKeepsUrFields =
  testGroup
    "split keeps a linear field of type Ur"
    [ testCase "a Mut is split around the field" (bumpThroughMutBoxed @?= ("label", "label", 42))
    , testCase "a Lend is split while the Mut is live" (splitLendEarlyBoxed @?= ("label", 2))
    , testCase "the contents of the field are not forced" (contentsStayUnforced @?= 6)
    , testCase "a box over a linear owner, the Lend split first" (boxOverOwnerLendFirst @?= ("xabel", "xabel", 11))
    , testCase "a box over a linear owner, a Share split twice" (boxOverOwnerSharedTwice @?= ("xabel", "xabel", "xabel"))
    , testCase "a constructor with an element" (splitBoxedPresent @?= (3, 13))
    , testCase "a Lend of a constructor with fields of type Ur only" (splitLendOfBoxedAbsent @?= "none")
    ]
