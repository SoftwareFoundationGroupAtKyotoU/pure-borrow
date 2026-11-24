{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GenericGuardAlias where

import Control.Monad.Borrow.Pure.BO (Alias)
import Generics.Linear.TH (deriveGeneric1)

-- This fixture must not typecheck: an instance through the hidden constructor would give a borrow whose kind and lifetime Generically1 could change.
-- EXPECT: Duplicate instance declarations
deriveGeneric1 ''Alias
