{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module OutlivesInstance where

import Control.Monad.Borrow.Pure.Lifetime (Static, type (<=))

-- This fixture must not typecheck: with this instance every lifetime would
-- outlive 'Static', so any borrow could be upcast to 'Static'.
-- The safe modules export '(<=)' only as a synonym of the class.
-- EXPECT: Illegal instance for type synonym
instance {-# INCOHERENT #-} Static <= α
