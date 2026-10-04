{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeLibraryType where

import Control.Monad.Borrow.Pure.BO.Internal (Alias (..))
import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: pure-borrow gives its own types their instances.
-- For Alias the fields alone would make the contents of a Mut covariant, since its mutability lies in the index ak, which no field mentions.
-- EXPECT: is defined in pure-borrow
deriveSubtype ''Alias
