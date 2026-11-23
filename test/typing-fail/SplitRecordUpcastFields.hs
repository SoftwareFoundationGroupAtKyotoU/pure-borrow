{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SplitRecordUpcastFields where

import Control.Monad.Borrow.Pure.BO (upcast)
import Data.Record.Linear.Borrow.Experimental.Split (SplitRecord, SplittableRecord)
import Data.Ref.Linear (Ref)
import GHC.Exts (Multiplicity (..))
import Generics.Linear.TH (deriveGeneric)

data R = R {x :: Ref Int, y :: Ref Int}

deriveGeneric ''R

deriving anyclass instance SplittableRecord R

-- This fixture must not typecheck: putting back the field y after it was split out would give a second live borrow of it.
-- EXPECT: Couldn't match type: '[]
-- EXPECT: arising from a use of
reset ::
  SplitRecord R bk α '[ '("x", '( 'One, Ref Int))] %1 ->
  SplitRecord R bk α '[ '("x", '( 'One, Ref Int)), '("y", '( 'One, Ref Int))]
reset = upcast
