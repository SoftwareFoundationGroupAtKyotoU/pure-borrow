{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.HashMap.RobinHood.Mutable.Linear.TypingCases (
  module Data.HashMap.RobinHood.Mutable.Linear.TypingCases,
) where

import Control.Monad.Borrow.Pure.BO (upcast)
import Data.HashMap.RobinHood.Mutable.Linear qualified as HM
import Prelude.Linear

-- | A newtype over 'Int', standing for a key type with another 'Data.Hashable.Hashable' instance.
newtype Key = Key Int

-- This must not typecheck: a table placed by the hashes of Int would be searched with the hashes of Key.
badRekey :: HM.HashMap Int Int %1 -> HM.HashMap Key Int
badRekey = upcast
