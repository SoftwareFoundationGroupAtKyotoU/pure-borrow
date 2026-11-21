module TokenCoerceAny where

import Control.Monad.Borrow.Pure.Lifetime.Token (Linearly)
import Data.Coerce (coerce)
import GHC.Exts (Any)

-- This fixture must not typecheck: a Linearly must not be made out of any value at hand.
-- EXPECT: Couldn't match representation of type
-- EXPECT: is not in scope
forge :: Any -> Linearly
forge = coerce
