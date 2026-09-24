{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Vector.Mutable.Linear.TypingCases (
  module Data.Vector.Mutable.Linear.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Data.Vector qualified as V
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Prelude.Linear

-- | An element with no 'Movable' instance, standing for a linearly owned resource such as a 'Data.Ref.Linear.Ref'.
data Owned = Owned

instance Consumable Owned where
  consume Owned = ()

{- | 'VL.modifyBoxedVector' hands every element to the GC-owned result, so the elements must be 'Movable'.

The vector is not empty, so the deferred constraint is forced when the first element is moved.
-}
badModifyNonMovable :: V.Vector Owned
badModifyNonMovable =
  VL.modifyBoxedVector (\vector -> Control.pure (consume vector)) (V.singleton Owned)
