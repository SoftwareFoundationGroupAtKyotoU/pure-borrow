{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.Borrow.Pure.Experimental.Reborrowable (
  Reborrowable (..),
  locally,
  locally_,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Prelude.Linear

class Reborrowable bor where
  {- |
  Executes an operation on a borrow in sub lifetime.
  You may need @-XImpredicativeTypes@ extension to use this function.

  Generalization of 'reborrowing'' and 'sharing'' that works for both 'Mut' and 'Share' borrows.
  -}
  locally' ::
    bor α a %1 ->
    (forall β. bor (β /\ α) a %1 -> BO (β /\ α') (After β r)) %1 ->
    BO α' (r, bor α a)

instance Reborrowable Mut where
  {-# SPECIALIZE instance Reborrowable Mut #-}
  locally' = reborrowing'
  {-# INLINE locally' #-}

instance Reborrowable Share where
  {-# SPECIALIZE instance Reborrowable Share #-}
  locally' shr k = Control.do
    let %1 !(Ur sh) = move shr
    (,sh) Control.<$> srunBO (k (upcast sh))
  {-# INLINE locally' #-}

locally ::
  (Reborrowable bor) =>
  bor α a %1 ->
  (forall β. bor (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (r, bor α a)
{-# INLINE locally #-}
locally bor k = locally' bor \mut -> Control.pure Control.<$> k mut

locally_ ::
  (Reborrowable bor, Consumable r) =>
  bor α a %1 ->
  (forall β. bor (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (bor α a)
{-# INLINE locally_ #-}
locally_ bor k = uncurry lseq Control.<$> locally bor k
