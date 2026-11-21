module TokenCoerceEndRelabel where

import Control.Monad.Borrow.Pure.Lifetime.Token (EndToken)
import Data.Coerce (coerce)

-- This fixture must not typecheck: an EndToken relabelled to another lifetime would reclaim while that lifetime runs.
-- EXPECT: Couldn't match type
-- EXPECT: arising from a use of
relabel :: EndToken α -> EndToken β
relabel = coerce
