{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GenericGuardEndToken where

import Control.Monad.Borrow.Pure.Lifetime.Token (EndToken)
import Generics.Linear.TH (deriveGeneric)

-- This fixture must not typecheck: an instance through the hidden constructor would give an end token of any lifetime.
-- EXPECT: Duplicate instance declarations
deriveGeneric ''EndToken
