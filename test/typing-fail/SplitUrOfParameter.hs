{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SplitUrOfParameter where

import Control.Monad.Borrow.Pure (DistributesAlias, Ur)
import Generics.Linear.TH (deriveGenericAnd1)

data Wrapped a = Wrapped !(Ur a) a

deriveGenericAnd1 ''Wrapped

-- This fixture must not typecheck: 'Control.Monad.Borrow.Pure.split' would put a 'Control.Monad.Borrow.Pure.Mut' under 'Ur', where it would be unrestricted.
-- EXPECT: A borrow cannot be distributed over Ur
deriving anyclass instance DistributesAlias Wrapped
