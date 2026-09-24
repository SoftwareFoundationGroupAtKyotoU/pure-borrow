module LinearOnlyEmptyInstance where

import Control.Monad.Borrow.Pure.Lifetime.Token (LinearOnly)

data Dummy = Dummy

-- This fixture must not typecheck: an instance with no method would let
-- 'Control.Monad.Borrow.Pure.withLinearly' mint a 'Linearly' from a freely
-- constructible value.
-- It used to compile with only a missing-method warning; the method's default
-- is now an unsatisfiable constraint that says how to write the instance.
-- EXPECT: A LinearOnly instance must come from a type that already has one.
instance LinearOnly Dummy
