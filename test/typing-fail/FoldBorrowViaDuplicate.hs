{-# LANGUAGE LinearTypes #-}

module FoldBorrowViaDuplicate where

import Control.Monad.Borrow.Pure (Mut)
import Control.Monad.Borrow.Pure.Experimental.Loop (Fold, foldBorrowVia)

-- This fixture must not typecheck: a splitter that hands out one Mut twice would alias it.
-- EXPECT: arising from multiplicity of
both :: Fold (Mut α Int) (Mut α Int)
both k = foldBorrowVia (\b -> [b, b]) k
