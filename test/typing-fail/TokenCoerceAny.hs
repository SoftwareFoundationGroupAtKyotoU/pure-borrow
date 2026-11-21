module TokenCoerceAny where

-- The token modules are imported whole, so that a constructor any of them exported would be in scope.
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe
import Data.Coerce (coerce)
import GHC.Exts (Any)

-- This fixture must not typecheck: a Linearly must not be made out of any value at hand.
-- EXPECT: Couldn't match representation of type
-- EXPECT: is not in scope
forge :: Any -> Linearly
forge = coerce
