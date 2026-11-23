{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module PatternMatchDuplicateField where

import Control.Monad.Borrow.Pure (Mut)
import Data.Record.Linear.Borrow.Experimental.PatternMatch ((.@))
import Data.Ref.Linear (Ref)
import Generics.Linear.TH (deriveGeneric)

data R2 = R2 {w :: Ref Int, z :: Ref Int}

deriveGeneric ''R2

-- EXPECT: Split record fields must be distinct, but got duplicate field
bad :: Mut α R2 %1 -> (Mut α (Ref Int), Mut α (Ref Int))
bad m = m .@ (#z, #z)
