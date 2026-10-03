{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module SubtypeDerivingVia where

import Control.Monad.Borrow.Pure (Mut)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Data.Coerce.Directed (type (<:))

data Payload = Payload

-- This fixture must not typecheck: coercing the reflexive instance would
-- let 'Data.Coerce.Directed.upcast' lengthen a mutable borrow to 'Static',
-- so that it outlives the 'Control.Monad.Borrow.Pure.reclaim' of its owner.
-- The safe modules export '(<:)' only as a synonym of the class, so no
-- instance can be declared against it, derived or written.
-- EXPECT: Illegal instance for type synonym
deriving via (Mut α Payload) instance {-# OVERLAPPING #-} Mut α Payload <: Mut Static Payload
