{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SplitLinearFieldOfAnotherType where

import Control.Monad.Borrow.Pure (DistributesAlias)
import Data.Ref.Linear (Ref)
import Generics.Linear.TH (deriveGenericAnd1)

data Counted a = Counted (Ref Int) a

deriveGenericAnd1 ''Counted

-- This fixture must not typecheck: 'Control.Monad.Borrow.Pure.split' would
-- hand out the linearly owned 'Data.Ref.Linear.Ref' of a borrowed structure
-- as an owned value, while the lender still gets it back.
-- EXPECT: A borrow cannot be distributed over a field of type
-- EXPECT: that the Generic1 instance represents as linear.
deriving anyclass instance DistributesAlias Counted
