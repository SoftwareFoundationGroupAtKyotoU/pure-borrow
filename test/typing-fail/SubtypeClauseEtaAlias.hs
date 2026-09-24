{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module SubtypeClauseEtaAlias where

import Control.Monad.Borrow.Pure (Mut)
import Control.Monad.Borrow.Pure.Lifetime (Lifetime, Static)
import Data.Coerce.Directed (type (<:))

data Payload = Payload

-- This fixture must not typecheck: an eta-reduced second synonym would let a deriving clause name '(<:)' unsaturated and reach the hidden class, whose instance would let 'Data.Coerce.Directed.upcast' turn a live @Mut α Payload@ into a @Mut Static Payload@.
-- GHC rejects the synonym itself, even under LiberalTypeSynonyms.
-- EXPECT: should have 2 arguments, but has been given none
type Up = (<:)

newtype Escape (α :: Lifetime) = Escape (Mut Static Payload)
  deriving (Up (Mut α Payload)) via (Escape α)
