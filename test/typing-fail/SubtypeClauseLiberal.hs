{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module SubtypeClauseLiberal where

import Control.Monad.Borrow.Pure (Mut)
import Control.Monad.Borrow.Pure.Lifetime (Lifetime, Static)
import Data.Coerce.Directed (type (<:))

data Payload = Payload

-- A second synonym, whose application would saturate '(<:)'.
type Up a b = a <: b

-- This fixture must not typecheck: 'LiberalTypeSynonyms' and a second synonym must not let a deriving clause reach the class behind '(<:)', whose instance would let 'Data.Coerce.Directed.upcast' turn a live @Mut α Payload@ into a @Mut Static Payload@.
-- EXPECT: Illegal instance for type synonym
newtype Escape (α :: Lifetime) = Escape (Mut Static Payload)
  deriving (Up (Mut α Payload)) via (Escape α)
