module TokenCoerceLinearly where

-- The token modules are imported whole, so that a constructor any of them exported would be in scope.
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe
import Data.Coerce (coerce)

-- This fixture must not typecheck: an EndToken is unrestricted, so a Linearly made from one could be used twice.
-- EXPECT: Couldn't match representation of type
-- EXPECT: is not in scope
linearlyFromEnd :: EndToken α -> Linearly
linearlyFromEnd = coerce
