{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}

module PatternMatchPolymorphic where

import Control.Monad.Borrow.Pure (Mut)
import Data.Record.Linear.Borrow.Experimental.PatternMatch ((.#))
import Data.Ref.Linear (Ref)
import GHC.Records (HasField)

-- EXPECT: are unknown, since it has no instance of linear-generics' Generic
-- EXPECT: take the label as an argument
bad :: (HasField "x" r (Ref Int)) => Mut α r %1 -> Mut α (Ref Int)
bad m = m .# #x
