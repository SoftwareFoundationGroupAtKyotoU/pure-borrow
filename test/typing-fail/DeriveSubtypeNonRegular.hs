{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeNonRegular where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: a field that mentions the type at other arguments than its parameters makes the derived context grow without end at every use.
-- EXPECT: at other arguments than its parameters
data Nested a = Flat a | Nest (Nested [a])

deriveSubtype ''Nested
