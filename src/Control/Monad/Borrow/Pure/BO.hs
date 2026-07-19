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

{- |
This module provides all the safe API of 'BO' monad, including the advanced, low-level combinators that are not meant to be used by most users.
For the conceptual overview, please refer to "Control.Monad.Borrow.Pure", which is the prelude of this package.
-}
module Control.Monad.Borrow.Pure.BO (
  -- $header

  -- * Core 'BO' monad
  BO (),
  execBO,
  runBO,
  runBOLend,
  runBO_,
  sexecBO,
  scope_,
  srunBO,
  srunBO_,
  askLinearly,
  asksLinearly,
  asksLinearlyM,
  evaluateBO,

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
  subShare,
  coerceShare,
  shareCoercion,
  borrowM,
  borrowLinearlyM,
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
  pureAfter,
  reborrow,
  joinMut,
  joinLend,

  -- *** Lower-level operators
  borrow,
  borrowLinearOnly,

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

  -- * Re-exports
  module Control.Monad.Borrow.Pure.Lifetime,
  module Control.Monad.Borrow.Pure.Lifetime.Token,
  module Data.Coerce.Directed,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce (Coercible)
import Data.Coerce.Directed
import Data.Type.Coercion (Coercion (..))
import Prelude.Linear

{- |
Runs a 'BO' computation and returns the result of postprocessing 'After' the lifetime has ended.

See also: 'runBOLend' and 'runBO_'.
-}
runBO :: forall a. Linearly %1 -> (forall α. BO α (After α a)) %1 -> a
{-# INLINE runBO #-}
runBO lin bo =
  case newLifetime lin of
    MkSomeNow (now :: Now α) -> DataFlow.do
      (now, f) <- execBO @α @(After α a) bo now
      case endLifetime now of
        Ur end -> withEnd @α end f

-- | A variant of 'runBO' that returns the original rsource retained by the 'Lend'er
runBOLend :: Linearly %1 -> (forall α. BO α (Lend α a)) %1 -> a
{-# INLINE runBOLend #-}
runBOLend lin bo = runBO lin Control.do
  lend <- bo
  Control.pure (reclaim' lend)

-- | A variant of 'runBO' that returns the direct value of 'BO' computation.
runBO_ :: Linearly %1 -> (forall α. BO α a) %1 -> a
{-# INLINE runBO_ #-}
runBO_ lin bo = runBO lin Control.do
  a <- bo
  pureAfter a

-- | Flipped version of 'sexecBO'.
scope_ :: Now α %1 -> BO (α /\ β) a %1 -> BO β (Now α, a)
{-# INLINE scope_ #-}
scope_ = flip sexecBO

-- | A variant of 'borrow' that obtains 'Linearly' viar 'LinearOnly'.
borrowLinearOnly :: forall α a. (LinearOnly a) => a %1 -> (Mut α a, Lend α a)
{-# INLINE borrowLinearOnly #-}
borrowLinearOnly !a = case withLinearly a of
  (!lin, !a) -> borrow a lin

{- | A variant of 'sharing'' that discards the final result of the computation.
There is also a flipped infix version '(<$=)'.

See also: 'sharing'. For 'Mut'able borrows, see 'reborrowing_'.
-}
sharing_ ::
  forall α α' a r.
  (Consumable r) =>
  Mut α a %1 ->
  (forall β. Share (β /\ α) a -> BO (β /\ α') r) %1 ->
  BO α' (Mut α a)
{-# INLINE sharing_ #-}
sharing_ v k = uncurry lseq Control.<$> sharing v k

-- | Flipped infix version of 'sharing_', smoewhat analgous to '(Control.<$>)' and @(<%=)@ in @lens@ package.
(<$=) ::
  (forall β. Share (β /\ α) a -> BO (β /\ α') ()) %1 ->
  Mut α a %1 ->
  BO α' (Mut α a)
{-# INLINE (<$=) #-}
(<$=) = flip sharing_

{- | A variant of 'sharing'' that returns the direct value of the computation on the shared borrow.
There is also a flipped infix version '(<$~)'.

See also: 'sharing_'. For 'Mut'able borrows, see 'reborrowing'.
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

See also: 'sharing' and 'sharing_'. For 'Mut'able borrows, see 'reborrowing''.
-}
sharing' ::
  Mut α a %1 ->
  (forall β. Share (β /\ α) a -> BO (β /\ α') (After β r)) %1 ->
  BO α' (r, Mut α a)
{-# INLINE sharing' #-}
sharing' v k = DataFlow.do
  srunBO DataFlow.do
    (v, lend) <- reborrow v
    share v & \(Ur v) -> Control.do
      k v Control.<&> \v -> (,) Control.<$> v Control.<*> upcast (reclaim' lend)

{- | Executes an operation on 'Mut'able borrow in sub lifetime.
You may need @-XImpredicativeTypes@ extension to use this function.

See also: 'reborrowing', and 'reborrowing_'. For 'Share'd borrows, see 'sharing', 'sharing'', and 'sharing_'.
-}
reborrowing' ::
  Mut α a %1 ->
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') (After β r)) %1 ->
  BO α' (r, Mut α a)
{-# INLINE reborrowing' #-}
reborrowing' v k = srunBO DataFlow.do
  (v, lend) <- reborrow v
  Control.do
    v <- k v
    Control.pure $ (,) Control.<$> v Control.<*> upcast (reclaim' lend)

{- | A variant of 'reborrowing'' that returns the direct value of the operation on the reborrowed mutable borrow.
There is also a flipped infix version '(<%~)'.

See also: 'reborrowing_' and 'sharing'.
-}
reborrowing ::
  Mut α a %1 ->
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (r, Mut α a)
{-# INLINE reborrowing #-}
reborrowing mutα k = reborrowing' mutα (\mut -> Control.pure Control.<$> k mut)

-- | Flipped infix version of 'reborrowing', smoewhat analgous to '(Control.<$>)' and @(<%~)@ in @lens@ package.
(<%~) ::
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  Mut α a %1 ->
  BO α' (r, Mut α a)
{-# INLINE (<%~) #-}
(<%~) = flip reborrowing

infix 4 <%~

{- |
A variant of 'reborrowing'' that discards the final result of the computation.
There is also a flipped infix version '(<%=)'.

See also: 'reborrowing' and 'sharing_'.
-}
reborrowing_ ::
  (Consumable r) =>
  Mut α a %1 ->
  (forall β. Mut (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (Mut α a)
{-# INLINE reborrowing_ #-}
reborrowing_ mutα k = reborrowing mutα (Control.fmap consume . k) Control.<&> \((), a) -> a

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
{-# INLINE modifyBO #-}
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
{-# INLINE modifyBO_ #-}
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
{-# INLINE modifyLinearOnlyBO #-}
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
{-# INLINE modifyLinearOnlyBO_ #-}
modifyLinearOnlyBO_ v k = DataFlow.do
  (lin, v) <- withLinearly v
  runBO lin Control.do
    let %1 !(mut, lend) = borrowLinearOnly v
    k mut
    Control.pure (reclaim' lend)

asksLinearly :: (Linearly %1 -> r) %1 -> BO α r
{-# INLINE asksLinearly #-}
asksLinearly k = asksLinearlyM $ Control.pure . k

pureAfter :: ((End α) => a) %1 -> BO α (After α a)
{-# INLINE pureAfter #-}
pureAfter a = Control.pure (After a)

{- | Shorten a shared borrow to a sublifetime.

This is the inference-friendly, borrow-kind-fixed variant of 'upcast'. It is
particularly useful when a shared borrow is captured outside a read-only loop
and must be used inside the loop's shorter lifetime.
-}
subShare :: (α >= β) => Share α a -> Share β a
{-# INLINE subShare #-}
subShare shr = upcast shr

coerceShare :: forall b α a. (Coercible a b) => Share α a %1 -> Share α b
{-# INLINE coerceShare #-}
coerceShare = coerceLin

shareCoercion :: forall a b α. (Coercible a b) => Coercion (Share α a) (Share α b)
{-# INLINE shareCoercion #-}
shareCoercion = Coercion

{- |
Borrow a resource linearly for the same lifetime as the ambient 'BO' computation.
Returns the pair of the mutable borrow to the resource, and 'Lend'er to be invoked later to 'reclaim' the resource at the 'End' of the lifetime.

See also 'borrowLinearlyM'.

If you want to borrow a resource indepdendent of the ambient lifetime, you can use 'borrow' instead.
-}
borrowM :: a %1 -> BO α (Mut α a, Lend α a)
{-# INLINE borrowM #-}
borrowM !a = asksLinearly \lin -> borrow a lin

-- | A variant of 'borrowM' that does linear allocation first.
borrowLinearlyM :: (Linearly %1 -> a) %1 -> BO α (Mut α a, Lend α a)
{-# INLINE borrowLinearlyM #-}
borrowLinearlyM k = asksLinearlyM $ borrowM . k

-- | Runs a 'BO' computation within the ephemeral sublifetime and returns the result.
srunBO :: (forall α. BO (α /\ β) (After α a)) %1 -> BO β a
{-# INLINE srunBO #-}
srunBO bo = asksLinearlyM \lin ->
  newLifetime' lin \now -> Control.do
    (now, f) <- sexecBO bo now
    Ur end <- Control.pure (endLifetime now)
    Control.pure (withEnd end f)

-- | A variant of 'srunBO' that returns the direct value of 'BO' computation.
srunBO_ :: (forall α. BO (α /\ β) a) %1 -> BO β a
{-# INLINE srunBO_ #-}
srunBO_ k = srunBO Control.do a <- k; Control.pure $ After a
