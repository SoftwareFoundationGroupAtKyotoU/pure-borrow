{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module RecordSplit where

import Control.Monad.Borrow.Pure (Borrow, Mut)
import Data.Record.Linear.Borrow.Experimental.PatternMatch (FieldBorrows, LabelsOrBorrows (..), (.#), (.@))
import Data.Record.Linear.Borrow.Experimental.Split (SplittableRecord)
import Data.Ref.Linear (Ref)
import GHC.Records (HasField)
import Generics.Linear.TH (deriveGeneric)

data R2 = R2 {w :: Ref Int, z :: Ref Int}

data SumRec = LeftRec {s1 :: Ref Int} | RightRec {s1 :: Ref Int}

deriveGeneric ''R2

deriveGeneric ''SumRec

-- This fixture must not typecheck: each binding below would split a record by one field twice, or by a field of a type whose fields are unknown, and a type with two constructors would become a splittable record.
-- EXPECT: Split record fields must be distinct, but got duplicate field
-- EXPECT: are unknown, since it has no instance of linear-generics' Generic
-- EXPECT: take the label as an argument
-- EXPECT: A union type cannot be a splittable record
-- EXPECT: badDuplicateTuple m = m .@
-- EXPECT: badDuplicateHList m = m .@
-- EXPECT: badPolymorphic m = m .#
-- EXPECT: instance SplittableRecord SumRec

badDuplicateTuple :: Mut α R2 %1 -> (Mut α (Ref Int), Mut α (Ref Int))
badDuplicateTuple m = m .@ (#z, #z)

badDuplicateHList :: Borrow bk α R2 %1 -> FieldBorrows bk α '[ '("z", Ref Int), '("w", Ref Int), '("z", Ref Int)]
badDuplicateHList m = m .@ (#z :#- #w :#- #z :#- RNil)

badPolymorphic :: (HasField "x" r (Ref Int)) => Mut α r %1 -> Mut α (Ref Int)
badPolymorphic m = m .# #x

deriving anyclass instance SplittableRecord SumRec
