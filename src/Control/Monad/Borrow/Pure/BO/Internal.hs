{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Monad.Borrow.Pure.BO.Internal (
  module Control.Monad.Borrow.Pure.BO.Internal,
) where

import Control.Applicative qualified as NonLinear
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (evaluate)
import Control.Exception qualified as SystemIO
import Control.Functor.Linear qualified as Control
import Control.Monad qualified as NonLinear
import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Control.Monad.ST.Strict (ST)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce qualified
import Data.Coerce.Directed.Unsafe
import Data.Functor.Identity (Identity)
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.Monoid qualified as Mon
import Data.Ord qualified as Ord
import Data.Semigroup qualified as Sem
import Data.Tuple (Solo (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.Base (TYPE)
import GHC.Base qualified as GHC
import GHC.Exts (Multiplicity (..), State#, runRW#)
import GHC.ST qualified as ST
import GHC.TypeError (ErrorMessage (..))
import Generics.Linear
import Prelude.Linear
import Prelude.Linear qualified as PL
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)
import System.IO.Linear qualified as L
import Unsafe.Coerce (unsafeCoerce#)
import Unsafe.Linear qualified as Unsafe

-- NOTE: NOINLINE here is REALLY important, otherwise GHC will inline 'UnsafeLinearly' and common subexpression elimination
-- causes severe soundness bug that the same expression reuses the same
-- linear resource and sometimes SEGV.
askLinearly :: BO α Linearly
{-# NOINLINE askLinearly #-}
askLinearly = GHC.noinline $ Control.pure UnsafeLinearly

asksLinearlyM :: (Linearly %1 -> BO α r) %1 -> BO α r
{-# INLINE asksLinearlyM #-}
asksLinearlyM k = Control.do
  lin <- askLinearly
  !a <- k lin
  Control.pure a

-- NOTE: We want to use @TypeData@ extension for 'ForBO', but it makes Haddock panic!

type ForBO :: Lifetime -> Type
data ForBO α

{- | Computation returning @a@ that can be performed only during the lifetime @α@.
     Internally it is a linear ST monad.
-}
newtype BO α a = BO (State# (ForBO α) %1 -> (# State# (ForBO α), a #))

instance (Semigroup w) => Semigroup (BO α w) where
  (<>) = Control.liftA2 (<>)
  {-# INLINE (<>) #-}

instance (Monoid w) => Monoid (BO α w) where
  mempty = Control.pure mempty
  {-# INLINE mempty #-}

unsafeUnBO :: BO α a %1 -> State# (ForBO α) %1 -> (# State# (ForBO α), a #)
{-# INLINE unsafeUnBO #-}
unsafeUnBO (BO f) = f

assocRBO :: BO ((α /\ β) /\ γ) a %1 -> BO (α /\ (β /\ γ)) a
{-# INLINE assocRBO #-}
assocRBO = unsafeCastBO

assocLBO :: BO (α /\ (β /\ γ)) a %1 -> BO ((α /\ β) /\ γ) a
{-# INLINE assocLBO #-}
assocLBO = unsafeCastBO

assocBOEq :: forall α β γ a. BO ((α /\ β) /\ γ) a :~: BO (α /\ (β /\ γ)) a
{-# INLINE assocBOEq #-}
assocBOEq = Unsafe.coerce $ Refl @(BO (α /\ β /\ γ) a)

instance Data.Functor (BO α) where
  fmap f (BO g) = BO \s -> case g s of
    (# s', a #) -> (# s', f a #)
  {-# INLINE fmap #-}

instance Control.Functor (BO α) where
  fmap f (BO g) = BO \s -> case g s of
    (# s', a #) -> (# s', f a #)
  {-# INLINE fmap #-}

instance Data.Applicative (BO α) where
  pure a = Control.pure a
  {-# INLINE pure #-}

  (<*>) = \f g -> f Control.<*> g
  {-# INLINE (<*>) #-}

  liftA2 f (BO g) (BO h) = BO \s -> case g s of
    (# s', a #) -> case h s' of
      (# s'', b #) -> (# s'', f a b #)
  {-# INLINE liftA2 #-}

instance Control.Applicative (BO α) where
  pure a = BO \s -> (# s, a #)
  {-# INLINE pure #-}

  BO f <*> BO g = BO \s -> case f s of
    (# s', h #) -> case g s' of
      (# s'', a #) -> (# s'', h a #)
  {-# INLINE (<*>) #-}

  liftA2 f (BO g) (BO h) = BO \s -> case g s of
    (# s', a #) -> case h s' of
      (# s'', b #) -> (# s'', f a b #)
  {-# INLINE liftA2 #-}

instance Control.Monad (BO α) where
  BO fa >>= f = BO \s -> case fa s of
    (# s', a #) -> (f a) PL.& \(BO g) -> g s'
  {-# INLINE (>>=) #-}

  BO fa >> BO fb = BO \s -> case fa s of
    (# s', () #) -> fb s'
  {-# INLINE (>>) #-}

-- | Unsafely converts a 'BO' computation to linear 'L.IO'.
unsafeBOToLinIO :: BO α a %1 -> L.IO a
{-# INLINE unsafeBOToLinIO #-}
unsafeBOToLinIO (BO f) = L.IO (Unsafe.coerce f)

{- |
Unsafely performs a linear 'L.IO' computation in 'BO' monad.

This is really, really unsafe. If you don't know what you are doing,
you MUST NOT use this function, otherwise you can break purity in a hard way.
-}
unsafeLinIOToBO :: L.IO a %1 -> BO α a
{-# INLINE unsafeLinIOToBO #-}
unsafeLinIOToBO (L.IO f) = BO (Unsafe.coerce f)

runBO# :: forall {rep} α (o :: TYPE rep). (State# (ForBO α) %1 -> o) %1 -> o
{-# INLINE runBO# #-}
runBO# = Unsafe.toLinear \f -> runRW# \s ->
  f (unsafeCoerce# s)

execBO :: BO α a %1 -> Now α %1 -> (Now α, a)
{-# INLINE execBO #-}
execBO (BO f) !now =
  case runBO# f of
    (# s, !a #) -> dropState# s `PL.lseq` (now, a)

dropState# :: State# a %1 -> ()
{-# INLINE dropState# #-}
dropState# = Unsafe.toLinear \ !_ -> ()

-- | See also 'Control.Monad.Borrow.Pure.scope'.
sexecBO :: BO (α /\ β) a %1 -> Now α %1 -> BO β (Now α, a)
{-# INLINE sexecBO #-}
sexecBO f now = unsafeCastBO ((now,) PL.. Unsafe.toLinear (\ !a -> a) Control.<$> f)

{- |
Coerces lifetime in 'BO' computation usafely and brutally.

This is really, really unsafe. If you don't know what you are doing,
you MUST NOT use this function, otherwise you will break the soundness of the type system.
-}
unsafeCastBO :: BO α a %1 -> BO β a
{-# INLINE unsafeCastBO #-}
unsafeCastBO = Unsafe.coerce

-- | Unsafely peforms a 'ST' computation in 'BO' monad.
unsafeSTToBO :: ST s a %1 -> BO α a
{-# INLINE unsafeSTToBO #-}
unsafeSTToBO (ST.ST f) = BO (Unsafe.coerce f)

{- |
Unsafely peforms a 'BO' computation in 'ST' monad.

This is really unsafe. If you don't know what you are doing, you MUST NOT use this function, otherwise you can break purity in a hard way.
-}
unsafeBOToST :: BO α a %1 -> ST s a
{-# INLINE unsafeBOToST #-}
unsafeBOToST (BO f) = ST.ST (Unsafe.coerce f)

{- |
Unsafely performs a standard, non-linear 'IO' computation in 'BO' monad.

This is really, really unsafe. If you don't know what you are doing,
you MUST NOT use this function, otherwise you can break purity in a hard way.
-}
unsafeSystemIOToBO :: IO a %1 -> BO α a
{-# INLINE unsafeSystemIOToBO #-}
unsafeSystemIOToBO (GHC.IO a) = BO (Unsafe.coerce a)

-- | Unsafely performs a 'BO' in the standard, non-linear 'IO' monad.
unsafeBOToSystemIO :: BO α a %1 -> IO a
{-# INLINE unsafeBOToSystemIO #-}
unsafeBOToSystemIO (BO f) = GHC.IO (Unsafe.coerce f)

unsafePerformEvaluateUndupableBO :: BO α a %1 -> a
unsafePerformEvaluateUndupableBO (BO f) = runBO# \s ->
  case Unsafe.toLinear GHC.noDuplicate# s of
    s -> case f s of
      (# s, !a #) -> dropState# s `PL.lseq` a

-- | Run two computations in parallel, returning their results as a tuple.
parBO :: BO α a %1 -> BO α b %1 -> BO α (a, b)
parBO = Unsafe.toLinear2 \a b -> unsafeSystemIOToBO do
  aVar <- newEmptyMVar
  bVar <- newEmptyMVar
  NonLinear.void $
    forkIO $
      putMVar aVar NonLinear.=<< evaluate NonLinear.=<< unsafeBOToSystemIO a
  NonLinear.void $
    forkIO $
      putMVar bVar NonLinear.=<< evaluate NonLinear.=<< unsafeBOToSystemIO b
  !a' <- takeMVar aVar
  !b' <- takeMVar bVar
  NonLinear.pure (a', b')

evaluateBO :: a %1 -> BO α a
{-# INLINE evaluateBO #-}
evaluateBO a = unsafeSystemIOToBO (Unsafe.toLinear SystemIO.evaluate a)

-- | Alias of kind 'ak' to a resource of type 'a'.
type Alias :: AliasKind -> Type -> Type
newtype Alias ak a = UnsafeAlias a

unsafeUnalias :: Alias ak a %1 -> a
unsafeUnalias (UnsafeAlias x) = x

{- |
Retags an alias with another 'AliasKind', leaving the aliased resource alone.

The role annotation below makes @ak@ nominal precisely so that this retagging is
not derivable, so every use is a proof obligation about the kind being moved to:
a 'Share' must not be widened into a 'Mut', a borrower must not become a lender,
and the lifetime it is retagged to must be one throughout which the resource is
really borrowed.

This is a coercion, not a coincidence of representation: 'Alias' is a newtype
over the resource, so the retagged alias is the very same value.
-}
unsafeCastAlias :: Alias ak a %1 -> Alias ak' a
{-# INLINE unsafeCastAlias #-}
unsafeCastAlias = coerceLin

{-
Note [Restoring a borrow must break its Core identity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Not every read of a borrowed resource is threaded through this monad's state token.
A growable vector keeps its length and its backing buffer behind a `Data.Ref.Linear.Ref`, and the reads that only project that header -- `size`, `capacity`, `getContents` and their neighbours -- go through `Data.Ref.Linear.unsafeReadRef`, an @INLINE@ pass-through to the `GHC.Magic.noinline`-wrapped `Data.Ref.Linear.Unlifted.unsafeReadRef#`, which opens its own `runRW#`.
To GHC such a read is a plain function of the borrow.
Two of them on the same borrow /variable/ are therefore the same Core expression, and common-subexpression elimination is entitled to serve the second from the first.

What keeps that honest is that every operation which replaces the header writes through `Data.Ref.Linear.Unlifted.unsafeWriteRef#`, which is @NOINLINE@, and hands back the reference that write returned.
The optimizer cannot see through it, so a read after a growth scrutinises a different expression than a read before it, and the two do not merge.
The whole ordering guarantee for header traffic rests on that one data dependency.

A delimiter that returns the borrow its caller passed in throws the dependency away.
The restored borrow is then the caller's own binder, so a read after the scope is syntactically the read before it, and CSE deletes the later one -- serving a stale length and a stale buffer across every growth the scope performed.
Writing through the stale buffer at an index the fresh length admits runs off the end of the allocation, which is how this last surfaced downstream: a SIGSEGV inside the collector, or silently wrong data under a nursery large enough that the damage is never traversed.

So a delimiter restores its borrow through `reviveAlias`, and both of its properties are load-bearing.

Do not weaken the @OPAQUE@ to @NOINLINE@.
Measured on GHC 9.12.4 at -O2 a @NOINLINE@ version is an equally good barrier today, and for a knowable reason: the argument's demand comes out lazy and the result is already an unboxed tuple, so no worker/wrapper split occurs and no @$w@ worker is generated.
But that is a property of one demand signature rather than a guarantee, and @OPAQUE@ is the one pragma GHC documents as suppressing inlining, worker/wrapper, specialisation and rules together.
The barrier is the whole of the memory safety of these delimiters, so it should rest on a contract rather than on a coincidence.

It lives in `BO` rather than being a pure function because a pure barrier depends on nothing the scope produced, so nothing would stop it -- or the reads that consume its result -- from floating above the scope body.
Consuming the state token pins the restoration after the scope's effects.
A pure @OPAQUE@ barrier also measures a few percent worse in a tight loop, but that is a side benefit and not the reason.

The cost is one out-of-line call per scope exit, around 0.7ns on aarch64-darwin with GHC 9.12.4, plus whatever it costs that a loop-carried borrow can no longer be unboxed past the barrier: measured together at 14-20% of a tight L1-resident loop that does nothing but the scope, and unmeasurable in any benchmark this repository ships.
No closure is allocated and the recursion remains a self tail call; what the call adds is a non-tail continuation and, in a loop, a boxed rather than unboxed loop-carried borrow.

This is a statement about the delimiters, not about `Ref` in general.
Threading the header reads through the state token, so that they are ordered like every other mutable operation and this whole hazard disappears, would be the durable fix; it is an API break and is not attempted here.
The barrier is also one-directional -- it stops a post-scope read being served from a pre-scope one, but nothing stops a pre-scope read from sinking below the scope -- so that deferral should not drift indefinitely.
-}

{- | Return a borrow to the caller of a delimiter, through a barrier the optimizer cannot see through.

Semantically this is `Control.pure` at 'Alias', and it carries no proof obligation of its own: its argument and result types are identical, so it cannot widen a 'Share' into a 'Mut', relabel a 'Lend', or lengthen a lifetime, and the `BO` index it returns at is as free as `Control.pure`'s already is.
That is also why it comes with no @TypingCases@ entry: there is no program that should stop typechecking because of it.

What it does carry is an obligation on /callers/.
Any delimiter that runs a continuation and then hands the caller back the borrow it was given must restore it through this.
Returning the caller's own occurrence instead lets common-subexpression elimination serve a post-scope read of the resource from a pre-scope one, across every write the scope performed.
See Note [Restoring a borrow must break its Core identity] for why, and for why the @OPAQUE@ and the state token are both load-bearing.
-}
reviveAlias :: Alias ak a %1 -> BO α (Alias ak a)
{-# OPAQUE reviveAlias #-}
reviveAlias a = BO \s -> (# s, a #)

type role Alias nominal representational

-- | Alias kind.
data AliasKind
  = -- | Borrower.
    Borrow BorrowKind Lifetime
  | -- | Lender.
    Lend Lifetime

-- | Borrower kind.
data BorrowKind
  = -- | Mutable.
    Mut
  | -- | Shared.
    Share

-- | Borrower of kind @bk@ that is active during the lifetime @α@.
type Borrow :: BorrowKind -> Lifetime -> Type -> Type
type Borrow bk α = Alias ('Borrow bk α)

-- | Mutable borrower, which is affine and can update the data.
type Mut :: Lifetime -> Type -> Type
type Mut α = Borrow 'Mut α

assocBorrowR ::
  Borrow bk ((α /\ β) /\ γ) a %1 ->
  Borrow bk (α /\ (β /\ γ)) a
{-# INLINE assocBorrowR #-}
assocBorrowR = coerceLin

assocBorrowL ::
  Borrow bk (α /\ (β /\ γ)) a %1 ->
  Borrow bk ((α /\ β) /\ γ) a
{-# INLINE assocBorrowL #-}
assocBorrowL = coerceLin

assocBorrowEq ::
  forall (bk :: BorrowKind) α β γ a.
  Borrow bk ((α /\ β) /\ γ) a :~: Borrow bk (α /\ (β /\ γ)) a
{-# INLINE assocBorrowEq #-}
assocBorrowEq = Unsafe.coerce $ Refl @(Borrow bk ((α /\ β) /\ γ) a)

assocLendR ::
  Lend ((α /\ β) /\ γ) a %1 ->
  Lend (α /\ (β /\ γ)) a
{-# INLINE assocLendR #-}
assocLendR = coerceLin

assocLendL ::
  Lend (α /\ (β /\ γ)) a %1 ->
  Lend ((α /\ β) /\ γ) a
{-# INLINE assocLendL #-}
assocLendL = coerceLin

assocLendEq :: forall α β γ a. (Lend ((α /\ β) /\ γ) a) :~: (Lend (α /\ (β /\ γ)) a)
{-# INLINE assocLendEq #-}
assocLendEq = Unsafe.coerce $ Refl @(Lend (α /\ β /\ γ) a)

instance (bk ~ 'Mut) => LinearOnly (Borrow bk α a) where
  linearOnly = UnsafeLinearOnly

deriving via
  AsAffine (Alias bor a)
  instance
    (bor ~ ('Borrow bk α)) => Consumable (Alias bor a)

-- | Shared borrower, which is unrestricted but usually can only read from the data.
type Share :: Lifetime -> Type -> Type
type Share α = Borrow 'Share α

instance (ak ~ 'Borrow bk α) => Affine (Alias ak a) where
  aff = UnsafeAff
  {-# INLINE aff #-}

instance (k ~ 'Borrow 'Share α) => Dupable (Alias k a) where
  dup2 = Unsafe.toLinear $ NonLinear.join (,)
  {-# INLINE dup2 #-}

instance (k ~ 'Borrow 'Share α) => Movable (Alias k a) where
  move = Unsafe.toLinear Ur
  {-# INLINE move #-}

instance (α >= β, a <: b) => BO α a <: BO β b where
  subtype = UnsafeSubtype

instance (α >= β, a <: b, b <: a) => Mut α a <: Mut β b where
  subtype = UnsafeSubtype

instance (α >= β, a <: b) => Share α a <: Share β b where
  subtype = UnsafeSubtype

-- | Lender, which can retrieve the lifetime at the lifetime @α@.
type Lend :: Lifetime -> Type -> Type
type Lend α = Alias ('Lend α)

instance (α <= β, a <: b) => Lend α a <: Lend β b where
  subtype = UnsafeSubtype

{- |
Borrow a resource linearly and obtain the mutable borrow to it and 'Lend' witness to 'reclaim the resource to lend at the 'End' of the lifetime.

For typical usage, you should use 'Control.Monad.Borrow.Pure.borrowM' to avoid type ambiguity.
-}
borrow :: forall α a. a %1 -> Linearly %1 -> (Mut α a, Lend α a)
borrow = Unsafe.toLinear2 \ !a !_ ->
  (UnsafeAlias a, UnsafeAlias a)

-- | Shares a mutable borrow, invalidating the original one.
share :: Borrow k α a %1 -> Ur (Share α a)
share = Unsafe.toLinear \(UnsafeAlias !a) -> Ur (UnsafeAlias a)

-- | Reclaims a 'borrow'ed resource at the 'End' of lifetime @α'.
reclaim' :: Lend α a %1 -> After α a
reclaim' l = After (reclaim l)

-- | Reclaims a 'borrow'ed resource at the 'End' of lifetime @α'.
reclaim :: (End α) => Lend α a %1 -> a
reclaim = \(UnsafeAlias !a) -> a

-- | Reborrow a mutable borrow into a sublifetime.
reborrow :: forall β α a. (α >= β) => Mut α a %1 -> (Mut β a, Lend β (Mut α a))
reborrow = Unsafe.toLinear \ !mutA ->
  (Data.Coerce.coerce mutA, Data.Coerce.coerce mutA)

{- |
Run and discard the result of a continuation with a representation-identical
borrow narrowed to a fresh sublifetime, then, on normal return, restore the
original mutable borrow.

This is the trusted non-finalizing delimiter used by the scalar public
result-discarding combinators. The rank-2 continuation cannot return its
private @β@ at a caller-nameable lifetime; existentially hiding it supplies no
ambient outlives evidence. The outer 'Mut' is retained only inside this
function while the continuation runs. The continuation result is consumed
before the outer borrow is restored. The continuation and state token are each
consumed exactly once. Since the lifetime indices have runtime-erased
representations, no runtime lifetime token or lender is required.

The borrow is handed back through 'reviveAlias' rather than returned directly;
see Note [Restoring a borrow must break its Core identity] there.
-}
unsafeBorrowScope_ ::
  forall bk α α' a r.
  (Consumable r) =>
  Mut α a %1 ->
  (forall β. Borrow bk (β /\ α) a %(BorrowMultiplicity bk) -> BO (β /\ α') r) %1 ->
  BO α' (Mut α a)
{-# INLINE unsafeBorrowScope_ #-}
unsafeBorrowScope_ = Unsafe.toLinear2 \mut k ->
  unsafeSrunBO_ Control.do
    r <- k (unsafeCastAlias mut)
    -- @consume r@ stays in the returned value rather than in the action, so it runs when the caller forces the restored borrow.
    -- Sequencing it at scope exit instead would make this delimiter stricter than the one @+slow@ restores, and the two are required to stay observationally equivalent.
    restored <- reviveAlias mut
    Control.pure (consume r `lseq` restored)

{- |
Run a continuation with a representation-identical borrow narrowed to a fresh
sublifetime, then, on normal return, restore the original mutable borrow
alongside the continuation's result.

This is the trusted delimiter used by the scalar public result-returning
combinators, and every obligation discharged in 'unsafeBorrowScope_' is
discharged here in the same way. The result type is fixed by the caller, so it
cannot mention the private @β@ and no borrow at @β@ escapes in it.
-}
unsafeBorrowScope ::
  forall bk α α' a r.
  Mut α a %1 ->
  (forall β. Borrow bk (β /\ α) a %(BorrowMultiplicity bk) -> BO (β /\ α') r) %1 ->
  BO α' (r, Mut α a)
{-# INLINE unsafeBorrowScope #-}
unsafeBorrowScope = Unsafe.toLinear2 \mut k ->
  unsafeSrunBO_ Control.do
    r <- k (unsafeCastAlias mut)
    (r,) Control.<$> reviveAlias mut

{- |
The finalizing variant of 'unsafeBorrowScope': the continuation returns its
result 'After' the sublifetime, and this discharges that 'After' before
restoring the original mutable borrow.

Beyond the obligations of 'unsafeBorrowScope', the 'EndToken' supplied to
'withEnd' is the runtime-erased one. That is sound for the same reason it is in
'Control.Monad.Borrow.Pure.BO.srunBO': the continuation has already returned, so
the sublifetime it was typechecked in is over by the time the token is applied,
and the caller-fixed result type cannot mention that lifetime.
-}
unsafeBorrowScope' ::
  forall bk α α' a r.
  Mut α a %1 ->
  (forall β. Borrow bk (β /\ α) a %(BorrowMultiplicity bk) -> BO (β /\ α') (After β r)) %1 ->
  BO α' (r, Mut α a)
{-# INLINE unsafeBorrowScope' #-}
unsafeBorrowScope' = Unsafe.toLinear2 \mut k ->
  unsafeSrunBO_ Control.do
    after <- k (unsafeCastAlias mut)
    (withEnd UnsafeEnd after,) Control.<$> reviveAlias mut

type BorrowMultiplicity :: BorrowKind -> Multiplicity
type family BorrowMultiplicity bk where
  BorrowMultiplicity 'Mut = One
  BorrowMultiplicity 'Share = Many

{- |
Run a rank-2 'BO' action in a statically delimited fresh sublifetime without
constructing a runtime lifetime token.

The action is typechecked parametrically for every private lifetime, so it
cannot rely on the implementation's erased instantiation at the ambient
lifetime or return a borrow at a caller-nameable lifetime. Existentially hiding
the private lifetime supplies no evidence needed to use such a borrow in an
ambient 'BO'. The state-token coercion executes the action exactly once. This
is the non-finalizing analogue of 'srunBO'; it cannot eliminate 'After' or
provide 'End' evidence.
-}
unsafeSrunBO_ ::
  forall β a.
  (forall α. BO (α /\ β) a) %1 ->
  BO β a
{-# INLINE unsafeSrunBO_ #-}
unsafeSrunBO_ action = unsafeCastBO (action @β)

-- | Collapse a borrower to a mutable borrower.
joinMut :: Borrow bk α (Mut β a) %1 -> Borrow bk (α /\ β) a
joinMut = coerceLin

joinLend :: Lend α (Lend α a) %1 -> Lend α a
joinLend = coerceLin

-- | Distribute an alias over a functor.
class DistributesAlias f where
  split_ :: Alias ak (f x) %1 -> f (Alias ak x)
  default split_ ::
    (GenericDistributesAlias f) =>
    Alias ak (f x) %1 -> f (Alias ak x)
  split_ = genericSplit

split ::
  forall f x ak.
  (DistributesAlias f) =>
  Alias ak (f x) %1 -> f (Alias ak x)
{-# INLINE [1] split #-}
split = split_

deriving anyclass instance DistributesAlias Identity

deriving anyclass instance DistributesAlias []

deriving anyclass instance DistributesAlias Maybe

deriving anyclass instance DistributesAlias Solo

deriving anyclass instance DistributesAlias Ord.Down

deriving anyclass instance DistributesAlias Sem.Dual

deriving anyclass instance DistributesAlias Sem.Max

deriving anyclass instance DistributesAlias Sem.Min

deriving anyclass instance DistributesAlias Sem.First

deriving anyclass instance DistributesAlias Sem.Last

deriving anyclass instance DistributesAlias Mon.First

deriving anyclass instance DistributesAlias Mon.Last

splitPair :: Alias ak (a, b) %1 -> (Alias ak a, Alias ak b)
{-# INLINE splitPair #-}
splitPair = coerceLin

splitEither :: Alias ak (Either a b) %1 -> Either (Alias ak a) (Alias ak b)
{-# INLINE splitEither #-}
splitEither = coerceLin

instance (Unsatisfiable ('Text "Use splitEither directly!")) => DistributesAlias (Either e) where
  {-# INLINE split_ #-}
  split_ = unsatisfiable

instance (Unsatisfiable ('Text "Use splitPair instead!")) => DistributesAlias ((,) a) where
  {-# INLINE split_ #-}
  split_ = unsatisfiable

type GenericDistributesAlias f = (Generic1 f, GDistributeAlias (Rep1 f))

genericSplit ::
  forall f x ak.
  (GenericDistributesAlias f) =>
  Alias ak (f x) %1 -> f (Alias ak x)
{-# INLINE genericSplit #-}
genericSplit =
  to1
    . gdistributeAlias @(Rep1 f)
    . unsafeMapAlias from1

unsafeMapAlias :: (a %1 -> b) %1 -> Alias ak a %1 -> Alias ak b
{-# INLINE unsafeMapAlias #-}
unsafeMapAlias f = coerceLin (\x -> let !y = f x in y)

instance (GenericDistributesAlias f) => DistributesAlias (Generically1 f) where
  {-# INLINE split_ #-}
  split_ = Generically1 . genericSplit . unsafeMapAlias \(Generically1 f) -> f

class GDistributeAlias f where
  gdistributeAlias :: Alias ak (f x) %1 -> f (Alias ak x)

instance
  ( GDistributeAlias f
  , GDistributeAlias g
  ) =>
  GDistributeAlias (f :*: g)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias !(UnsafeAlias !(f :*: g)) =
    DataFlow.do
      !f <- gdistributeAlias $ UnsafeAlias f
      !g <- gdistributeAlias $ UnsafeAlias g
      f :*: g

instance
  ( GDistributeAlias f
  , GDistributeAlias g
  ) =>
  GDistributeAlias (f :+: g)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias (UnsafeAlias x) = case x of
    L1 !l -> L1 (gdistributeAlias (UnsafeAlias l))
    R1 !r -> R1 (gdistributeAlias (UnsafeAlias r))

instance
  (Unsatisfiable (Text "Nonlinear fields cannot distribute borrows!")) =>
  GDistributeAlias (MP1 GHC.Many f)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias = unsatisfiable

instance (GDistributeAlias f) => GDistributeAlias (MP1 GHC.One f) where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias =
    MP1 . gdistributeAlias . UnsafeAlias . unMP1 . unsafeUnalias

instance (GDistributeAlias f) => GDistributeAlias (M1 i c f) where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias (UnsafeAlias (M1 x)) =
    M1 $ gdistributeAlias $ UnsafeAlias x

instance DistributesAlias Par1 where
  {-# INLINE split_ #-}
  split_ (UnsafeAlias (Par1 a)) = Par1 (UnsafeAlias a)

instance
  ( DistributesAlias f
  , DistributesAlias g
  , Data.Functor f
  ) =>
  GDistributeAlias (f :.: g)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias (UnsafeAlias (Comp1 !fg)) =
    Comp1 $ Data.fmap split_ $ split_ $ UnsafeAlias fg

instance GDistributeAlias Par1 where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias (UnsafeAlias (Par1 !a)) = Par1 (UnsafeAlias a)

instance
  (Unsatisfiable (Text "A type containing non-parametric field with type `" :<>: ShowType c :<>: Text "', which cannot be safely splitted!")) =>
  GDistributeAlias (K1 i c)
  where
  {-# INLINE gdistributeAlias #-}
  gdistributeAlias = unsatisfiable

instance GDistributeAlias U1 where
  gdistributeAlias = coerceLin
  {-# INLINE gdistributeAlias #-}
