{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module EndInstance where

import Control.Monad.Borrow.Pure.Lifetime.Token (End)

-- This fixture must not typecheck: an 'End' instance would let 'reclaim'
-- hand an owner back while its borrows are still live.
-- The safe modules export 'End' only as a synonym of the class.
-- EXPECT: Illegal instance for type synonym
instance End α
