{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module SubtypeClauseVia where

import Control.Monad.Borrow.Pure (Mut)
import Control.Monad.Borrow.Pure.Lifetime (Lifetime, Static)
import Data.Coerce.Directed (type (<:))

data Payload = Payload

-- This fixture must not typecheck: the derived instance would let 'Data.Coerce.Directed.upcast' turn a live @Mut α Payload@ into a @Mut Static Payload@, which outlives the 'Control.Monad.Borrow.Pure.reclaim' of its owner.
-- A deriving clause names the class applied to all but its last argument, so an eta-reduced synonym would expand there to the hidden class; '(<:)' takes both arguments, so the clause names it unsaturated and is rejected.
-- EXPECT: Illegal instance for type synonym
newtype Escape (α :: Lifetime) = Escape (Mut Static Payload)
  deriving ((<:) (Mut α Payload)) via (Escape α)
