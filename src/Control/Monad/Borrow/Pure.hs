{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
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
  asksLinearly,
  asksLinearlyM,

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
  (<$~),
  sharing_,
  (<$=),
  reborrowing',
  reborrowing,
  (<%~),
  reborrowing_,
  (<%=),
  share,
  reclaim',
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
  pureAfter,
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

runBO :: forall a. Linearly %1 -> (forall α. BO α (After α a)) %1 -> a
{-# INLINE runBO #-}
runBO lin bo =
  case newLifetime lin of
    MkSomeNow (now :: Now α) -> DataFlow.do
      (now, f) <- execBO @α @(After α a) bo now
      case endLifetime now of
        Ur end -> withEnd @α end f

runBOLend :: Linearly %1 -> (forall α. BO α (Lend α a)) %1 -> a
{-# INLINE runBOLend #-}
runBOLend lin bo = runBO lin Control.do
  lend <- bo
  Control.pure (reclaim' lend)

runBO_ :: Linearly %1 -> (forall α. BO α a) %1 -> a
{-# INLINE runBO_ #-}
runBO_ lin bo = runBO lin Control.do
  a <- bo
  pureAfter a

-- | Flipped version of 'sexecBO'.
scope_ :: Now α %1 -> BO (α /\ β) a %1 -> BO β (Now α, a)
{-# INLINE scope_ #-}
scope_ = flip sexecBO

srunBO :: (forall α. Proxy α -> BO (α /\ β) (After α a)) %1 -> BO β a
{-# INLINE srunBO #-}
srunBO bo = asksLinearlyM \lin ->
  newLifetime' lin \now -> Control.do
    (now, f) <- sexecBO (bo Proxy) now
    Ur end <- Control.pure (endLifetime now)
    Control.pure (withEnd end f)

-- | A variant of 'borrow' that obtains 'Linearly' viar 'LinearOnly'.
borrowLinearOnly :: (LinearOnly a) => a %1 -> (Mut α a, Lend α a)
borrowLinearOnly !a = case withLinearly a of
  (!lin, !a) -> borrow a lin

{- | Executes an operation on 'Share'd borrow in sub lifetime.
You may need @-XImpredicativeTypes@ extension to use this function.

See also: '(<$=)', 'sharing' and 'sharing''.
-}
sharing_ ::
  forall α α' a.
  Mut α a %1 ->
  (forall β. Share (β /\ α) a -> BO (β /\ α') ()) %1 ->
  BO α' (Mut α a)
{-# INLINE sharing_ #-}
sharing_ v k = sharing v k Control.<&> \((), a) -> a

-- | Flipped infix version of 'sharing_', smoewhat analgous to '(Control.<$>)' and @(<%=)@ in @lens@ package.
(<$=) ::
  (forall β. Share (β /\ α) a -> BO (β /\ α') ()) %1 ->
  Mut α a %1 ->
  BO α' (Mut α a)
{-# INLINE (<$=) #-}
(<$=) = flip sharing_

{- | Executes an operation on 'Share'd borrow in sub lifetime.
You may need @-XImpredicativeTypes@ extension to use this function.

See also: '(<$~)', 'sharing'', and 'sharing_'.
-}
sharing ::
  forall α α' a r.
  Mut α a %1 ->
  (forall β. Share (β /\ α) a -> BO (β /\ α') r) %1 ->
  BO α' (r, Mut α a)
{-# INLINE sharing #-}
sharing v k = sharing' v (\mut -> Control.pure Control.<$> k mut)

-- | Flipped infix version of 'sharing', smoewhat analgous to '(Control.<$>)' and @(<%~)@ in @lens@ package.
(<$~) ::
  (forall β. Share (β /\ α) a -> BO (β /\ α') r) %1 ->
  Mut α a %1 ->
  BO α' (r, Mut α a)
{-# INLINE (<$~) #-}
(<$~) = flip sharing

infix 4 <$~

{- | Executes an operation on 'Share'd borrow in sub lifetime.
You may need @-XImpredicativeTypes@ extension to use this function.

See also: 'sharing' and 'sharing_'.
-}
sharing' ::
  Mut α a %1 ->
  (forall β. Share (β /\ α) a -> BO (β /\ α') (After β r)) %1 ->
  BO α' (r, Mut α a)
{-# INLINE sharing' #-}
sharing' v k = DataFlow.do
  srunBO \_ ->
    DataFlow.do
      (v, lend) <- reborrow v
      share v & \(Ur v) -> Control.do
        k v Control.<&> \v -> (,) Control.<$> v Control.<*> upcast (reclaim' lend)

reborrowing' ::
  Mut α a %1 ->
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') (After β r)) %1 ->
  BO α' (r, Mut α a)
reborrowing' v k = srunBO \(Proxy :: Proxy β) -> DataFlow.do
  (v, lend) <- reborrow v
  Control.do
    v <- k v
    Control.pure $ (,) Control.<$> v Control.<*> upcast (reclaim' lend)

reborrowing ::
  Mut α a %1 ->
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (r, Mut α a)
reborrowing mutα k = reborrowing' mutα (\mut -> Control.pure Control.<$> k mut)

-- | Flipped infix version of 'reborrowing', smoewhat analgous to '(Control.<$>)' and @(<%~)@ in @lens@ package.
(<%~) ::
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  Mut α a %1 ->
  BO α' (r, Mut α a)
{-# INLINE (<%~) #-}
(<%~) = flip reborrowing

infix 4 <%~

reborrowing_ ::
  Mut α a %1 ->
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') ()) %1 ->
  BO α' (Mut α a)
reborrowing_ mutα k = reborrowing mutα k Control.<&> \((), a) -> a

-- | Flipped infix version of 'reborrowing_', smoewhat analgous to '(Control.<$>)' and @(<%=)@ in @lens@ package.
(<%=) ::
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') ()) %1 ->
  Mut α a %1 ->
  BO α' (Mut α a)
{-# INLINE (<%=) #-}
(<%=) = flip reborrowing_

infix 4 <%=

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
    Control.pure $ (r,) Control.<$> reclaim' lend

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
    Control.pure $ reclaim' lend

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
    Control.pure $ (r,) Control.<$> reclaim' lend

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
    Control.pure (reclaim' lend)

asksLinearlyM :: (Linearly %1 -> BO α r) %1 -> BO α r
{-# INLINE asksLinearlyM #-}
asksLinearlyM k = Control.do
  lin <- askLinearly
  !a <- k lin
  Control.pure a

asksLinearly :: (Linearly %1 -> r) %1 -> BO α r
{-# INLINE asksLinearly #-}
asksLinearly k = asksLinearlyM $ Control.pure . k

pureAfter :: ((End α) => a) %1 -> BO α (After α a)
{-# INLINE pureAfter #-}
pureAfter a = Control.pure (After a)
