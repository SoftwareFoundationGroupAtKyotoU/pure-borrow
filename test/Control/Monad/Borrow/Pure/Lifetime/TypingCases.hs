{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Control.Monad.Borrow.Pure.Lifetime.TypingCases (
  module Control.Monad.Borrow.Pure.Lifetime.TypingCases,
) where

import Control.Monad.Borrow.Pure.Lifetime.Internal

data Dict c where
  MkDict :: (c) => Dict c

withDict :: Dict c -> ((c) => a) -> a
withDict MkDict x = x

type family L1 :: Lifetime where

type family L2 :: Lifetime where

type family L3 :: Lifetime where

transitive :: (a <= b, b <= c) => Witness a c
transitive = witness

infElimL :: forall a b c -> (a <= b) => Witness (a /\ c) b
infElimL _ _ _ = witness

infElimR :: forall a b c -> (a <= b) => Witness (c /\ a) b
infElimR _ _ _ = witness

infIntro :: forall a b c -> (a <= b, a <= c) => Witness a (b /\ c)
infIntro _ _ _ = witness

infComm :: forall a b -> Witness (a /\ b) (b /\ a)
infComm _ _ = witness

infMonotone :: forall a b c -> (a <= b) => Witness (a /\ c) (b /\ c)
infMonotone _ _ _ = witness

infL :: forall a b -> Witness (a /\ b) a
infL _ _ = witness

infR :: forall a b -> Witness (a /\ b) b
infR _ _ = witness
