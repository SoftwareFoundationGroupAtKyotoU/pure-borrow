{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module UpcastUnrestrictedToLinear where

import Control.Monad.Borrow.Pure (upcast)

-- This fixture must not typecheck: an unrestricted function cannot be upcast
-- to a linear one, or any linear resource (a 'Mut', a 'Lend', an owner) could
-- be duplicated.
-- EXPECT: Cannot satisfy: multiplicity Many <= One
dupL :: forall a. a %1 -> (a, a)
dupL = upcast @(a -> (a, a)) @(a %1 -> (a, a)) (\x -> (x, x))
