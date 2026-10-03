{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Coerce.Directed.TypingCases (
  module Data.Coerce.Directed.TypingCases,
) where

import Data.Coerce.Directed.Internal (MultLe)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exts (Multiplicity (..))

-- 'upcast' between function types requires the source multiplicity to be at
-- most the target one, with 'One' below 'Many': a linear function may be used
-- as an unrestricted one, never the other way round.

oneBelowMany :: MultLe 'One 'Many :~: 'True
oneBelowMany = Refl

manyBelowMany :: MultLe 'Many 'Many :~: 'True
manyBelowMany = Refl

-- This must not typecheck: an equation that ignored its arguments once made
-- 'Many' compare below 'One', which let 'upcast' turn @a -> (a, a)@ into
-- @a %1 -> (a, a)@.
badManyBelowOne :: MultLe 'Many 'One :~: 'True
badManyBelowOne = Refl
