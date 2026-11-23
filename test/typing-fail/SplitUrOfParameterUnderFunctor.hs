{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SplitUrOfParameterUnderFunctor where

import Control.Monad.Borrow.Pure (DistributesAlias, Ur)
import Generics.Linear.TH (deriveGenericAnd1)

data Wrapped a = Wrapped !(Ur (Maybe a)) a

deriveGenericAnd1 ''Wrapped

-- This fixture must not typecheck: 'Control.Monad.Borrow.Pure.split' would put a 'Control.Monad.Borrow.Pure.Mut' under 'Ur', inside a 'Maybe', where it would be unrestricted.
-- EXPECT: No instance for
-- EXPECT: :.: Ur)
deriving anyclass instance DistributesAlias Wrapped
