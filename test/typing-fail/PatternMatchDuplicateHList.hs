{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module PatternMatchDuplicateHList where

import Control.Monad.Borrow.Pure (Borrow)
import Data.Record.Linear.Borrow.Experimental.PatternMatch (FieldBorrows, LabelsOrBorrows (..), (.@))
import Data.Ref.Linear (Ref)
import Generics.Linear.TH (deriveGeneric)

data R2 = R2 {w :: Ref Int, z :: Ref Int}

deriveGeneric ''R2

-- EXPECT: Split record fields must be distinct, but got duplicate field
-- EXPECT: z
bad :: Borrow bk α R2 %1 -> FieldBorrows bk α '[ '("z", Ref Int), '("w", Ref Int), '("z", Ref Int)]
bad m = m .@ (#z :#- #w :#- #z :#- RNil)
