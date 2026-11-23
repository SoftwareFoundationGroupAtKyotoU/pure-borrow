{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
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

module Data.Record.Linear.Borrow.Experimental.PatternMatch.TypingCases (
  module Data.Record.Linear.Borrow.Experimental.PatternMatch.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure (borrowLinearlyM, linearly, pureAfter, reborrowing, reclaim, runBO)
import Data.Coerce (coerce)
import Data.Record.Linear.Borrow.Experimental.PatternMatch (RecordLabel, (.#), (.@))
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import GHC.Records (HasField (..))
import Generics.Linear.TH (deriveGeneric)
import Prelude.Linear

data R = R {x :: Ref Int, y :: Ref Int}

deriveGeneric ''R

data R2 = R2 {w :: Ref Int, z :: Ref Int}

deriveGeneric ''R2

instance HasField "zAlias" R2 (Ref Int) where
  getField = z

-- This must not typecheck: "zAlias" is a virtual HasField instance, not a declared field of R2.
badVirtualZAlias :: (Int, Int)
badVirtualZAlias = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> R2 (Ref.new 0 l1) (Ref.new 0 l2)
  let !(m1, m2) = m .@ (#z, #zAlias)
  m1 <- RefBorrow.modify (\n -> n `lseq` 1) m1
  (a, m2) <- reborrowing m2 RefBorrow.copyRef
  Control.pure (m1 `lseq` consume m2)
  pureAfter (case reclaim lend of R2 rw rz -> Ref.free rw `lseq` Ref.free rz `lseq` (0, a))

-- This must not typecheck: (.#) also rejects a virtual HasField instance.
badVirtualDotSharp :: Int
badVirtualDotSharp = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> R2 (Ref.new 0 l1) (Ref.new 0 l2)
  let !mz = m .# #zAlias
  (a, mz) <- reborrowing mz RefBorrow.copyRef
  Control.pure (consume mz)
  pureAfter (case reclaim lend of R2 rw rz -> Ref.free rw `lseq` Ref.free rz `lseq` a)

data NoGen = NoGen {ng1 :: Ref Int, ng2 :: Ref Int}

-- This must not typecheck: NoGen has no linear-generics Generic instance.
badNoGeneric :: (Int, Int)
badNoGeneric = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> NoGen (Ref.new 0 l1) (Ref.new 0 l2)
  let !(m1, m2) = m .@ (#ng1, #ng2)
  (a, m1) <- reborrowing m1 RefBorrow.copyRef
  (b, m2) <- reborrowing m2 RefBorrow.copyRef
  Control.pure (m1 `lseq` consume m2)
  pureAfter (case reclaim lend of NoGen r1 r2 -> Ref.free r1 `lseq` Ref.free r2 `lseq` (a, b))

data SumRec = LeftRec {s1 :: Ref Int, s2 :: Ref Int} | RightRec {s1 :: Ref Int, s2 :: Ref Int}

deriveGeneric ''SumRec

-- This must not typecheck: SumRec has more than one constructor.
badSumType :: (Int, Int)
badSumType = linearly \lin -> runBO lin Control.do
  (m, lend) <- borrowLinearlyM \l -> case dup2 l of
    (l1, l2) -> LeftRec (Ref.new 0 l1) (Ref.new 0 l2)
  let !(m1, m2) = m .@ (#s1, #s2)
  (a, m1) <- reborrowing m1 RefBorrow.copyRef
  (b, m2) <- reborrowing m2 RefBorrow.copyRef
  Control.pure (m1 `lseq` consume m2)
  pureAfter
    ( case reclaim lend of
        LeftRec r1 r2 -> Ref.free r1 `lseq` Ref.free r2 `lseq` (a, b)
        RightRec r1 r2 -> Ref.free r1 `lseq` Ref.free r2 `lseq` (a, b)
    )

-- This must not typecheck: RecordLabel' has nominal roles on both arguments, so a label cannot be coerced to another field.
badCoerceField :: RecordLabel R "y" (Ref Int)
badCoerceField = coerce (#x :: RecordLabel R "x" (Ref Int))

-- This must not typecheck: RecordLabel' has nominal roles on both arguments, so a label cannot be coerced to another record.
newtype WrappedR = WrappedR R

badCoerceRecord :: RecordLabel WrappedR "x" (Ref Int)
badCoerceRecord = coerce (#x :: RecordLabel R "x" (Ref Int))

-- This must not typecheck: RecordLabel' has nominal roles on both arguments, so a label cannot be coerced to another field type.
newtype WrapRef = WrapRef (Ref Int)

badCoerceType :: RecordLabel R "x" WrapRef
badCoerceType = coerce (#x :: RecordLabel R "x" (Ref Int))
