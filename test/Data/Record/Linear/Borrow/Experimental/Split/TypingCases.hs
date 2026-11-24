{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Record.Linear.Borrow.Experimental.Split.TypingCases (
  module Data.Record.Linear.Borrow.Experimental.Split.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure (borrowLinearlyM, linearly, pureAfter, reborrowing, reclaim, runBO, splitPair)
import Control.Monad.Borrow.Pure.BO (upcast)
import Data.Coerce (coerce)
import Data.Record.Linear.Borrow.Experimental.Split (
  RecordLabel,
  SplitRecord,
  SplittableRecord,
  splitRecord,
  (!#),
  (+#),
  (-#),
  (.#),
 )
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import GHC.Exts (Multiplicity (..))
import GHC.Records (HasField (..))
import Generics.Linear.TH (deriveGeneric)
import Prelude.Linear

data R = R {x :: Ref Int, y :: Ref Int}

deriveGeneric ''R

deriving anyclass instance SplittableRecord R

-- This must not typecheck: putting back the field y after it was split out would give a second live borrow of it.
badResetFields ::
  SplitRecord R bk α '[ '("x", '( 'One, Ref Int))] %1 ->
  SplitRecord R bk α '[ '("x", '( 'One, Ref Int)), '("y", '( 'One, Ref Int))]
badResetFields = upcast

data Node = Node {next :: Node, val :: Ref Int}

newtype Cursor = Cursor {next :: Node}

deriveGeneric ''Node

deriveGeneric ''Cursor

deriving anyclass instance SplittableRecord Node

deriving anyclass instance SplittableRecord Cursor

-- This must not typecheck: the field next of a Cursor is the whole Node, whose field val was already split out.
badViewRecord ::
  SplitRecord Node bk α '[ '("next", '( 'One, Node))] %1 ->
  SplitRecord Cursor bk α '[ '("next", '( 'One, Node))]
badViewRecord = upcast

data R2 = R2 {w :: Ref Int, z :: Ref Int}

deriveGeneric ''R2

deriving anyclass instance SplittableRecord R2

instance HasField "both" R2 (Ref Int, Ref Int) where
  getField r = (z r, z r)

instance HasField "zAlias" R2 (Ref Int) where
  getField = z

instance HasField "uAlias" R2 (Ur Int) where
  getField _ = Ur 0

-- This must not typecheck: "both" is a hand-written virtual HasField instance, not a declared field of R2.
badVirtualBoth :: (Int, Int)
badVirtualBoth = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> R2 (Ref.new 0 l1) (Ref.new 0 l2)
  let !(m1, m2) = splitPair (m .# #both)
  m1 <- RefBorrow.modify (\n -> n `lseq` 1) m1
  (a, m2) <- reborrowing m2 RefBorrow.copyRef
  Control.pure (m1 `lseq` consume m2)
  pureAfter (case reclaim lend of R2 rw rz -> Ref.free rw `lseq` Ref.free rz `lseq` (0, a))

-- This must not typecheck: (-#) rejects a virtual HasField instance.
badVirtualSubtract :: (Int, Int)
badVirtualSubtract = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> R2 (Ref.new 0 l1) (Ref.new 0 l2)
  let !(m1, rest) = splitRecord m -# #zAlias
      !m2 = rest !# #z
  m1 <- RefBorrow.modify (\n -> n `lseq` 1) m1
  (a, m2) <- reborrowing m2 RefBorrow.copyRef
  Control.pure (m1 `lseq` consume m2)
  pureAfter (case reclaim lend of R2 rw rz -> Ref.free rw `lseq` Ref.free rz `lseq` (0, a))

-- This must not typecheck: (!#) rejects a virtual HasField instance.
badVirtualBang :: Int
badVirtualBang = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> R2 (Ref.new 0 l1) (Ref.new 0 l2)
  let !mz = splitRecord m !# #zAlias
  (a, mz) <- reborrowing mz RefBorrow.copyRef
  Control.pure (consume mz)
  pureAfter (case reclaim lend of R2 rw rz -> Ref.free rw `lseq` Ref.free rz `lseq` a)

-- This must not typecheck: (+#) rejects a virtual HasField instance.
badVirtualPlus :: Int
badVirtualPlus = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> R2 (Ref.new 0 l1) (Ref.new 0 l2)
  let !(Ur x, rest) = splitRecord m +# #uAlias
  Control.pure (consume rest)
  pureAfter (case reclaim lend of R2 rw rz -> Ref.free rw `lseq` Ref.free rz `lseq` x)

-- This must not typecheck: (-#) cannot split out the same field twice.
badDoubleSubtract :: (Int, Int)
badDoubleSubtract = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> R2 (Ref.new 0 l1) (Ref.new 0 l2)
  let !(m1, rest) = splitRecord m -# #z
      !(m2, rest') = rest -# #z
  (a, m1) <- reborrowing m1 RefBorrow.copyRef
  (b, m2) <- reborrowing m2 RefBorrow.copyRef
  Control.pure (m1 `lseq` m2 `lseq` consume rest')
  pureAfter (case reclaim lend of R2 rw rz -> Ref.free rw `lseq` Ref.free rz `lseq` (a, b))

data NoGen = NoGen {ng :: Ref Int}

-- This must not typecheck: NoGen has no linear-generics Generic instance.
badNoGeneric :: Int
badNoGeneric = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM (NoGen . Ref.new 0)
  let !mx = m .# #ng
  (a, mx) <- reborrowing mx RefBorrow.copyRef
  Control.pure (consume mx)
  pureAfter (case reclaim lend of NoGen r -> Ref.free r `lseq` a)

data SumRec = LeftRec {s1 :: Ref Int} | RightRec {s1 :: Ref Int}

deriveGeneric ''SumRec

-- This must not typecheck: SumRec has more than one constructor.
badSumType :: Int
badSumType = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM (LeftRec . Ref.new 0)
  let !mx = m .# #s1
  (a, mx) <- reborrowing mx RefBorrow.copyRef
  Control.pure (consume mx)
  pureAfter
    ( case reclaim lend of
        LeftRec r -> Ref.free r `lseq` a
        RightRec r -> Ref.free r `lseq` a
    )

-- This must not typecheck: RecordLabel has nominal roles on all arguments, so a label cannot be coerced to another field.
badCoerceField :: RecordLabel R "y" (Ref Int)
badCoerceField = coerce (#x :: RecordLabel R "x" (Ref Int))

-- This must not typecheck: RecordLabel has nominal roles on all arguments, so a label cannot be coerced to another record.
newtype WrappedR = WrappedR R

badCoerceRecord :: RecordLabel WrappedR "x" (Ref Int)
badCoerceRecord = coerce (#x :: RecordLabel R "x" (Ref Int))

-- This must not typecheck: RecordLabel has nominal roles on all arguments, so a label cannot be coerced to another field type.
newtype WrapRef = WrapRef (Ref Int)

badCoerceType :: RecordLabel R "x" WrapRef
badCoerceType = coerce (#x :: RecordLabel R "x" (Ref Int))
