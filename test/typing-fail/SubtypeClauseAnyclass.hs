{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module SubtypeClauseAnyclass where

import Control.Monad.Borrow.Pure (Mut)
import Control.Monad.Borrow.Pure.Lifetime (Lifetime, Static)
import Data.Coerce.Directed (type (<:))

data Payload = Payload

-- This fixture must not typecheck: the derived instance would let 'Data.Coerce.Directed.upcast' turn a live @Mut α Payload@ into a @Mut Static Payload@, which outlives the 'Control.Monad.Borrow.Pure.reclaim' of its owner, with only a missing-method warning.
-- '(<:)' takes both arguments, so the clause names it unsaturated and is rejected rather than expanded to the hidden class.
-- EXPECT: Illegal instance for type synonym
newtype Escape (α :: Lifetime) = Escape (Mut Static Payload)
  deriving anyclass ((<:) (Mut α Payload))
