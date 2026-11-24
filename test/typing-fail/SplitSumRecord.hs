{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SplitSumRecord where

import Data.Record.Linear.Borrow.Experimental.Split (SplittableRecord)
import Data.Ref.Linear (Ref)
import Generics.Linear.TH (deriveGeneric)

data SumRec = LeftRec {s1 :: Ref Int} | RightRec {s1 :: Ref Int}

deriveGeneric ''SumRec

-- EXPECT: A union type cannot be a splittable record
deriving anyclass instance SplittableRecord SumRec
