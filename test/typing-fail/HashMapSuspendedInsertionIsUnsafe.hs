{-# LANGUAGE NoImplicitPrelude #-}

module HashMapSuspendedInsertionIsUnsafe where

-- This fixture must not compile: the suspended insertion of the borrow-aware hash map is only in its .Unsafe module, since a plan used with another table, or after the table changed, writes outside it.
-- EXPECT: does not export
import Data.HashMap.RobinHood.Mutable.Linear.Borrow (unsafeInsertPrepared)
