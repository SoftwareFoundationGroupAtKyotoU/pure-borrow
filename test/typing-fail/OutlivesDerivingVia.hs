{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module OutlivesDerivingVia where

import Control.Monad.Borrow.Pure.Lifetime (Lifetime, Static, type (<=))

-- This fixture must not typecheck: coercing the instance for Static <= Static would make every lifetime outlive 'Static', so that 'Data.Coerce.Directed.upcast' lengthens any borrow to it.
-- The witness of '(<=)' is a GADT, nominal in both lifetimes, so GHC rejects the coercion.
-- EXPECT: Couldn't match type
-- EXPECT: Static
-- EXPECT: in a derived instance for
deriving via (Static :: Lifetime) instance {-# OVERLAPPING #-} Static <= α
