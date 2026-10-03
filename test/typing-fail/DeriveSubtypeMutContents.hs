{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeMutContents where

import Control.Monad.Borrow.Pure (Mut, Share, type (>=))
import Data.Coerce.Directed (upcast)
import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: what a 'Mut' points to stays invariant through a derived instance, so a borrow that ends sooner cannot replace one that lasts.
-- EXPECT: <=!!
newtype Box α a = Box (Mut α a)

deriveSubtype ''Box

shortenContents :: (γ >= β) => Box α (Share γ Int) %1 -> Box α (Share β Int)
shortenContents = upcast
