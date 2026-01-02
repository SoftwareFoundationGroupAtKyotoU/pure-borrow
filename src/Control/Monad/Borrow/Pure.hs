{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  askLinearly,
  withLinearlyBO,

  -- ** In-place modification with mutable borrows
  modifyBO,
  modifyBO_,
  modifyLinearOnlyBO,
  modifyLinearOnlyBO_,

  -- * Parallel computation
  parBO,

  -- * Borrowing
  Alias,
  AliasKind,
  BorrowKind,
  Borrow,
  Mut,
  Share,
  Lend,
  borrow,
  borrow_,
  borrowLinearOnly,
  sharing',
  sharing,
  sharing_,
  reborrowing',
  reborrowing,
  reborrowing_,
  share,
  reclaim,
  reborrow,
  joinMut,
  Copyable (),
  copy,
  copyMut,
  genericCopyShare,
  GenericCopyable,

  -- ** Splitting aliases
  DistributesAlias (),
  split,
  GenericDistributesAlias,
  genericSplit,
  splitPair,
  splitEither,

  -- ** Misc Utilities

  -- *** Manual lifetime reassociation
  assocRBO,
  assocLBO,
  assocBOEq,
  assocBorrowL,
  assocBorrowR,
  assocBorrowEq,
  assocLendL,
  assocLendR,
  assocLendEq,

  -- * References
  module Control.Monad.Borrow.Pure.Ref,

  -- * Re-exports
  module Control.Monad.Borrow.Pure.Lifetime,
  module Control.Monad.Borrow.Pure.Lifetime.Token,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Ref
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
runBOLend lin bo = runBO lin (bo Control.<&> reclaim)

runBO_ :: Linearly %1 -> (forall α. BO α a) %1 -> a
{-# INLINE runBO_ #-}
runBO_ lin bo = runBO lin (const Control.<$> bo)

-- | Flipped version of 'sexecBO'.
scope_ :: Now α %1 -> BO (α /\ β) a %1 -> BO β (Now α, a)
{-# INLINE scope_ #-}
scope_ = flip sexecBO

srunBO :: (forall α. Proxy α -> BO (α /\ β) (End α -> a)) %1 -> BO β a
{-# INLINE srunBO #-}
srunBO bo = withLinearlyBO \lin ->
  newLifetime' lin \now -> Control.do
    (now, f) <- sexecBO (bo Proxy) now
    Ur end <- Control.pure (endLifetime now)
    Control.pure (f end)

-- | A variant of 'borrow' that obtains 'Linearly' viar 'LinearOnly'.
borrowLinearOnly :: (LinearOnly a) => a %1 -> (Mut α a, Lend α a)
borrowLinearOnly !a = case withLinearly a of
  (!lin, !a) -> borrow a lin

{- | Executes an operation on 'Share'd borrow in sub lifetime.
You may need @-XImpredicativeTypes@ extension to use this function.

See also: 'sharing' and 'sharing''.
-}
sharing_ ::
  forall α α' a.
  Mut α a %1 ->
  (forall β. Share (β /\ α) a -> BO (β /\ α') ()) %1 ->
  BO α' (Mut α a)
{-# INLINE sharing_ #-}
sharing_ v k = sharing v k Control.<&> \((), a) -> a

{- | Executes an operation on 'Share'd borrow in sub lifetime.
You may need @-XImpredicativeTypes@ extension to use this function.

See also: 'sharing'' and 'sharing_'.
-}
sharing ::
  forall α α' a r.
  Mut α a %1 ->
  (forall β. Share (β /\ α) a -> BO (β /\ α') r) %1 ->
  BO α' (r, Mut α a)
{-# INLINE sharing #-}
sharing v k = sharing' v (\mut -> k mut Control.<&> \a _ -> a)

{- | Executes an operation on 'Share'd borrow in sub lifetime.
You may need @-XImpredicativeTypes@ extension to use this function.

See also: 'sharing' and 'sharing_'.
-}
sharing' ::
  Mut α a %1 ->
  (forall β. Share (β /\ α) a -> BO (β /\ α') (End β -> r)) %1 ->
  BO α' (r, Mut α a)
{-# INLINE sharing' #-}
sharing' v k = DataFlow.do
  srunBO \_ ->
    DataFlow.do
      (v, lend) <- reborrow v
      share v & \(Ur v) -> Control.do
        k v Control.<&> \v end -> (v (upcast end), reclaim lend (upcast end))

reborrowing' ::
  Mut α a %1 ->
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') (End β -> r)) %1 ->
  BO α' (r, Mut α a)
reborrowing' v k = DataFlow.do
  srunBO \(Proxy :: Proxy β) -> DataFlow.do
    (v, lend) <- reborrow v
    Control.do
      v <- k v
      Control.pure $ \end -> (v (upcast end), reclaim lend (upcast end))

reborrowing ::
  Mut α a %1 ->
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (r, Mut α a)
reborrowing mutα k = reborrowing' mutα (\mut -> k mut Control.<&> \a _ -> a)

reborrowing_ ::
  Mut α a %1 ->
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') ()) %1 ->
  BO α' (Mut α a)
reborrowing_ mutα k = reborrowing mutα k Control.<&> \((), a) -> a

-- | Modifies linear resources in-place, together with results.
modifyBO ::
  a %1 ->
  Linearly %1 ->
  (forall α. Mut α a %1 -> BO α r) %1 ->
  (r, a)
modifyBO v lin k = DataFlow.do
  (lin, lin') <- dup lin
  runBO lin Control.do
    let %1 !(mut, lend) = borrow v lin'
    r <- k mut
    Control.pure \end -> (r, reclaim lend end)

-- | Modifies linear resources in-place, without results.
modifyBO_ ::
  a %1 ->
  Linearly %1 ->
  (forall α. Mut α a %1 -> BO α ()) %1 ->
  a
modifyBO_ v lin k = DataFlow.do
  (lin, lin') <- dup lin
  runBO lin Control.do
    let %1 !(mut, lend) = borrow v lin'
    k mut
    Control.pure $ reclaim lend

-- | Modifies linear resources in-place, together with results.
modifyLinearOnlyBO ::
  (LinearOnly a) =>
  a %1 ->
  (forall α. Mut α a %1 -> BO α r) %1 ->
  (r, a)
modifyLinearOnlyBO v k = DataFlow.do
  (lin, v) <- withLinearly v
  runBO lin Control.do
    let %1 !(mut, lend) = borrowLinearOnly v
    !r <- k mut
    Control.pure \end -> (r, reclaim lend end)

-- | Modifies linear resources in-place, together with results.
modifyLinearOnlyBO_ ::
  (LinearOnly a) =>
  a %1 ->
  (forall α. Mut α a %1 -> BO α ()) %1 ->
  a
modifyLinearOnlyBO_ v k = DataFlow.do
  (lin, v) <- withLinearly v
  runBO lin Control.do
    let %1 !(mut, lend) = borrowLinearOnly v
    k mut
    Control.pure $ reclaim lend

withLinearlyBO :: (Linearly %1 -> BO α r) %1 -> BO α r
{-# INLINE withLinearlyBO #-}
withLinearlyBO k = Control.do
  lin <- askLinearly
  !a <- k lin
  Control.pure a
