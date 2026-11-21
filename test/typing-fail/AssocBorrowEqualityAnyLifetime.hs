{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module AssocBorrowEqualityAnyLifetime where

import Control.Monad.Borrow.Pure.BO (assocBorrowEq)
import Control.Monad.Borrow.Pure.Lifetime (Lifetime)
import Data.Type.Equality (inner, outer, trans, (:~:))

-- This fixture must not typecheck: 'inner' and 'outer' take an equality between the two associations of a meet apart into an equality between any two lifetimes.
-- EXPECT: does not export
anyEq :: forall (α :: Lifetime) (β :: Lifetime). α :~: β
anyEq =
  trans
    (inner (inner (inner (outer (assocBorrowEq @_ @β @β @α @())))))
    (inner (outer (inner (inner (outer (assocBorrowEq @_ @β @α @α @()))))))
