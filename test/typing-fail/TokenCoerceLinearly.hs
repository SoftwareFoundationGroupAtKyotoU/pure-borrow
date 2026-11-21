module TokenCoerceLinearly where

import Control.Monad.Borrow.Pure.Lifetime.Token (EndToken, Linearly)
import Data.Coerce (coerce)

-- This fixture must not typecheck: an EndToken is unrestricted, so a Linearly made from one could be used twice.
-- EXPECT: Couldn't match representation of type
-- EXPECT: is not in scope
linearlyFromEnd :: EndToken α -> Linearly
linearlyFromEnd = coerce
