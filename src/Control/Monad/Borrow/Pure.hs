{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.Borrow.Pure (
  BO (),
  execBO,
  runBO,
  runBOLend,
  runBO_,
  sexecBO,
  scope_,
  srunBO,
  scope,

  -- * Parallel computation
  parBO,

  -- * Borrows and refernces
  Mut (),
  Share (),
  Lend (),
  borrow,
  borrow_,
  sharing,
  sharing_,
  reborrowing,
  reborrowing_,
  share,
  reclaim,
  reborrow,
  Derefable (),
  derefShare,
  genericDerefShare,
  GenericDerefable,

  -- ** Collapsing borrows
  unMutMut,
  unShrMut,
  unMutShr,
  unShrShr,

  -- ** Case-splitting for borrows
  SplittableRef,
  SplittableRefAt,
  AccessibleRef,
  AccessibleRefAt,
  DistributesRef (),
  split,
  GenericDistributesRef,
  genericSplit,

  -- *** Specialized case-splitting for variables
  splitList,
  splitPair,
  splitEither,

  -- * References
  module Control.Monad.Borrow.Pure.Var,

  -- * Re-exports
  module Control.Monad.Borrow.Pure.Lifetime,
  module Control.Monad.Borrow.Pure.Lifetime.Token,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Var
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce.Directed (upcast)
import Data.Proxy (Proxy (..))
import Prelude.Linear

runBO :: Linearly %1 -> (forall α. BO α (End α -> a)) %1 -> a
{-# INLINE runBO #-}
runBO lin bo =
  case newLifetime lin of
    MkSomeNow now -> DataFlow.do
      (now, f) <- execBO bo now
      case endLifetime now of
        Ur end -> f end

runBOLend :: Linearly %1 -> (forall α. BO α (Lend α a)) %1 -> a
{-# INLINE runBOLend #-}
runBOLend lin bo = runBO lin (bo Control.<&> \lend end -> reclaim end lend)

runBO_ :: Linearly %1 -> (forall α. BO α a) %1 -> a
{-# INLINE runBO_ #-}
runBO_ lin bo = runBO lin (const Control.<$> bo)

-- | Flipped version of 'sexecBO'.
scope_ :: Now α %1 -> BO (α /\ β) a %1 -> BO β (Now α, a)
{-# INLINE scope_ #-}
scope_ = flip sexecBO

srunBO :: (forall α. Proxy α -> BO (α /\ β) (End α -> a)) %1 -> Linearly %1 -> BO β a
{-# INLINE srunBO #-}
srunBO bo lin =
  case newLifetime lin of
    MkSomeNow now -> Control.do
      (now, f) <- sexecBO (bo Proxy) now
      Ur end <- Control.pure (endLifetime now)
      Control.pure (f end)

scope :: Linearly %1 -> (forall α. Proxy α -> BO (α /\ β) (End α -> a)) %1 -> BO β a
scope = flip srunBO

{- | Executes an operation on 'Share'd reference in sub lifetime.
You may need @-XImpredicativeTypes@ extension to use this function.

See also: 'sharing'.
-}
sharing_ ::
  forall α a r.
  Mut α a %1 ->
  (forall β. (β <= α) => Share β a -> BO β r) %1 ->
  BO α (r, Mut α a)
{-# INLINE sharing_ #-}
sharing_ v k = sharing v (\mut -> k mut Control.<&> \a _ -> a)

{- | Executes an operation on 'Share'd reference in sub lifetime.
You may need @-XImpredicativeTypes@ extension to use this function.

See also: 'sharing_'.
-}
sharing ::
  forall α a r.
  Mut α a %1 ->
  (forall β. (β <= α) => Share β a -> BO β (End β -> r)) %1 ->
  BO α (r, Mut α a)
{-# INLINE sharing #-}
sharing v k = DataFlow.do
  (lin, v) <- withLinearly v
  scope lin \(Proxy :: Proxy β) ->
    DataFlow.do
      (v, lend) <- reborrow v
      share v & \(Ur v) -> Control.do
        v <- k v
        Control.pure $ \end -> (v (upcast end), reclaim (upcast end) lend)

reborrowing ::
  Mut α a %1 ->
  (forall β. (β <= α) => Mut β a -> BO β (End β -> r)) %1 ->
  BO α (r, Mut α a)
reborrowing mutα k =
  withLinearly mutα & \(lin, v) ->
    scope lin \(Proxy :: Proxy β) ->
      reborrow v & \(v, lend) ->
        Control.do
          v <- k v
          Control.pure $ \end -> (v (upcast end), reclaim (upcast end) lend)

reborrowing_ ::
  Mut α a %1 ->
  (forall β. (β <= α) => Mut β a -> BO β r) %1 ->
  BO α (r, Mut α a)
reborrowing_ mutα k = reborrowing mutα (\mut -> k mut Control.<&> \a _ -> a)
