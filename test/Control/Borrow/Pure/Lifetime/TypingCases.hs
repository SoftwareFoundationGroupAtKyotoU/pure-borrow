{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Control.Borrow.Pure.Lifetime.TypingCases (
  module Control.Borrow.Pure.Lifetime.TypingCases,
) where

import Control.Borrow.Pure.Lifetime.Internal

data Dict c where
  MkDict :: (c) => Dict c

type family L1 :: Lifetime where

type family L2 :: Lifetime where

type family L3 :: Lifetime where

transitive :: forall a b c. Dict (a <= b) -> Dict (b <= c) -> Witness a c
transitive MkDict MkDict = witness
