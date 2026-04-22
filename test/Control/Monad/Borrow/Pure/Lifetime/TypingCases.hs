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

transitive :: (α <= β, β <= γ) => Witness α γ
transitive = witness

infElimL :: forall α β γ -> (α <= β) => Witness (α /\ γ) β
infElimL _ _ _ = witness

infElimR :: forall α β γ -> (α <= β) => Witness (γ /\ α) β
infElimR _ _ _ = witness

infIntro :: forall α β γ -> (α <= β, α <= γ) => Witness α (β /\ γ)
infIntro _ _ _ = witness

infComm :: forall α β -> Witness (α /\ β) (β /\ α)
infComm _ _ = witness

infMonotone :: forall α β γ -> (α <= β) => Witness (α /\ γ) (β /\ γ)
infMonotone _ _ _ = witness

infL :: forall α β -> Witness (α /\ β) α
infL _ _ = witness

infR :: forall α β -> Witness (α /\ β) β
infR _ _ = witness
