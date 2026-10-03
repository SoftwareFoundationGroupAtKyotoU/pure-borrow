{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeQualifiedConstructor where

import Data.Coerce.Directed.Unsafe (deriveSubtype)
import Data.Functor.Identity qualified as I

-- This fixture must not typecheck: deriveSubtype needs the constructors in scope unqualified.
-- EXPECT: needs its constructor Identity
deriveSubtype ''I.Identity
