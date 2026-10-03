{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Control.Monad.Borrow.Pure.Experimental.Loop.TypingCases (
  module Control.Monad.Borrow.Pure.Experimental.Loop.TypingCases,
) where

import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Experimental.Loop (foldBorrow)
import Data.Monoid.Linear (Sum (..))
import Data.Vector.Mutable.Linear qualified as LV
import Prelude.Linear (Either, Int)

-- This must not typecheck: folding a mutable container through a borrow
-- would read its elements in pure code, possibly after the borrow's lifetime.
badFoldMutableVector :: Share α (LV.Vector Int) -> Sum Int
badFoldMutableVector shared = foldBorrow (\element -> Sum (copy element)) shared

-- This must not typecheck: a sum type is split with 'splitEither', not folded,
-- and the instance that 'Either' declares says so.
badFoldEither :: Share α (Either Int Int) -> Sum Int
badFoldEither shared = foldBorrow (\element -> Sum (copy element)) shared
