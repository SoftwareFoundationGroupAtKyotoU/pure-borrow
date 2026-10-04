{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module SubtypeDerivingVia where

import Control.Monad.Borrow.Pure (Mut)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Data.Coerce.Directed (type (<:))

data Payload = Payload

-- This fixture must not typecheck: coercing the reflexive instance would let 'Data.Coerce.Directed.upcast' lengthen a mutable borrow to 'Static', so that it outlives the 'Control.Monad.Borrow.Pure.reclaim' of its owner.
-- 0.1.0.0 accepted it, since the witness of '(<:)' had phantom roles; the target's role is now representational, and 'Mut' is nominal in its lifetime.
-- EXPECT: Couldn't match type
-- EXPECT: Static
-- EXPECT: in a derived instance for
deriving via (Mut α Payload) instance {-# OVERLAPPING #-} Mut α Payload <: Mut Static Payload
