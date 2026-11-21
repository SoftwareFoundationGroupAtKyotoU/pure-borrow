{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module AssocEqualityAnyLifetime where

import Control.Monad.Borrow.Pure.BO (assocBOEq)
import Control.Monad.Borrow.Pure.Lifetime (Lifetime)
import Data.Type.Equality (inner, outer, trans, (:~:))

-- This fixture must not typecheck: 'inner' and 'outer' take an equality between the two associations of a meet apart into an equality between any two lifetimes.
-- EXPECT: does not export
anyEq :: forall (α :: Lifetime) (β :: Lifetime). α :~: β
anyEq =
  trans
    (inner (inner (outer (assocBOEq @β @β @α @()))))
    (inner (outer (inner (outer (assocBOEq @β @α @α @())))))
