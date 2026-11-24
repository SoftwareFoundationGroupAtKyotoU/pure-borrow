{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GenericGuardVector where

import Data.Vector.Mutable.Linear.Borrow (Vector)
import Generics.Linear.TH (deriveGeneric)

-- This fixture must not typecheck: an instance through the hidden constructor would give a second owner of the vector and of its elements.
-- EXPECT: Duplicate instance declarations
deriveGeneric ''Vector
