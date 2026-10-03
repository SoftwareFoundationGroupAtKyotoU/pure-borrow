{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericallyDerivingVia where

import Data.Coerce.Directed (type (<:))
import Generics.Linear (Generically (..))
import Generics.Linear.TH (deriveGeneric)

-- This fixture must not typecheck: the 0.1.0.0 way of deriving (<:) for a type of one's own names deriveSubtype instead.
-- EXPECT: can no longer be derived via Generically
-- EXPECT: deriveSubtype from Data.Coerce.Directed.Unsafe
data Two a = Two a a

$(deriveGeneric ''Two)

deriving via Generically (Two b) instance (a <: b) => Two a <: Two b
