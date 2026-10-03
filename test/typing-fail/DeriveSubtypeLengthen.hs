{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeLengthen where

import Control.Monad.Borrow.Pure (Share, Static)
import Data.Coerce.Directed (upcast)
import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: an instance from deriveSubtype relates a record of borrows as its fields allow, so it cannot lengthen them to 'Static'.
-- EXPECT: <=!!
data Env α = Env (Share α Int) (Share α Bool)

deriveSubtype ''Env

lengthen :: Env α %1 -> Env Static
lengthen = upcast
