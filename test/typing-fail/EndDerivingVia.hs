{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module EndDerivingVia where

import Control.Monad.Borrow.Pure.Lifetime (Lifetime)
import Control.Monad.Borrow.Pure.Lifetime.Token (End)

-- This fixture must not typecheck: deriving 'End' via the lifetime itself
-- draws only an orphan warning, which is off by default, and would let
-- 'reclaim' run at any time.
-- EXPECT: Illegal instance for type synonym
deriving via (α :: Lifetime) instance End α
