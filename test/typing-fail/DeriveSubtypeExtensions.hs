{-# LANGUAGE TemplateHaskell #-}

module DeriveSubtypeExtensions where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: the instance needs extensions that GHC2021 does not enable, and the splice names them all at once.
-- EXPECT: enable UndecidableInstances
data Pair a = Pair a a

deriveSubtype ''Pair
