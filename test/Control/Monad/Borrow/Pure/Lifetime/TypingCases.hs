{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Control.Monad.Borrow.Pure.Lifetime.TypingCases (
  module Control.Monad.Borrow.Pure.Lifetime.TypingCases,
) where

import Control.DeepSeq (rnf)
import Control.Monad.Borrow.Pure.Lifetime.Internal

data Dict c where
  MkDict :: (c) => Dict c

withDict :: Dict c -> ((c) => a) -> a
withDict MkDict x = x

type family L1 :: Lifetime where

type family L2 :: Lifetime where

type family L3 :: Lifetime where

transitive :: forall α β γ. (α <= β, β <= γ) => ()
transitive = rnf (witness @α @γ)

infElimL ::
  forall (α :: Lifetime) (β :: Lifetime) (γ :: Lifetime) ->
  (α <= β) =>
  ()
infElimL (type α) (type β) (type γ) = rnf (witness @(α /\ γ) @β)

infElimR ::
  forall (α :: Lifetime) (β :: Lifetime) (γ :: Lifetime) ->
  (α <= β) =>
  ()
infElimR (type α) (type β) (type γ) = rnf (witness @(γ /\ α) @β)

infIntro :: forall α β γ -> (α <= β, α <= γ) => Witness α (β /\ γ)
infIntro _ _ _ = witness

infComm :: forall α β -> Witness (α /\ β) (β /\ α)
infComm _ _ = witness

infMonotone ::
  forall (α :: Lifetime) (β :: Lifetime) (γ :: Lifetime) ->
  (α <= β) =>
  ()
infMonotone (type α) (type β) (type γ) =
  rnf (witness @(α /\ γ) @(β /\ γ))

infL :: forall α β -> Witness (α /\ β) α
infL _ _ = witness

infR :: forall α β -> Witness (α /\ β) β
infR _ _ = witness
