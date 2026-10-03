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
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid qualified as Mon
import Data.Ord qualified as Ord
import Data.Semigroup qualified as Sem
import Data.Tuple (Solo (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.Base (TYPE)
import GHC.Base qualified as GHC
import GHC.Exts (Multiplicity (..), State#, runRW#)
import GHC.ST qualified as ST
import GHC.TypeError (ErrorMessage (..), Unsatisfiable, unsatisfiable)
import Generics.Linear
import Prelude.Linear
import Prelude.Linear qualified as PL
import System.IO.Linear qualified as L
import Unsafe.Coerce (unsafeCoerce#)
import Unsafe.Linear qualified as Unsafe

-- NOTE: NOINLINE here is REALLY important, otherwise GHC will inline 'UnsafeLinearly' and common subexpression elimination
-- causes severe soundness bug that the same expression reuses the same
-- linear resource and sometimes SEGV.
askLinearly :: BO α Linearly
{-# NOINLINE askLinearly #-}
askLinearly = GHC.noinline $ Control.pure UnsafeLinearly

{- | A witness that the 'Static' lifetime is ongoing, which it always is.

To run a @'BO' 'Static'@ action inside @'BO' β@, 'Data.Coerce.Directed.upcast' it: 'Static' outlives every lifetime.
This token is for the APIs that take a 'Now' explicitly: 'execBO' runs a @'BO' 'Static'@ action with it directly, while 'sexecBO' and 'Control.Monad.Borrow.Pure.BO.scope_' take an action at @'Static' '/\' β@, to which a @'BO' 'Static'@ action has to be upcast first.
They hand the token back; drop it with @'consume' ('Control.Monad.Borrow.Pure.Affine.aff' now)@.

Like 'askLinearly', it is only available inside 'BO', so the token is always bound linearly, and 'withLinearly' cannot turn it into an unrestricted 'Linearly'.
-}
nowStatic :: BO α (Now Static)
-- NOINLINE for the same reason as 'askLinearly': every bind must yield a distinct token.
{-# NOINLINE nowStatic #-}
nowStatic = GHC.noinline $ Control.pure UnsafeNow

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

{- | Run a state-threaded computation to completion.

'GHC.noDuplicate#' comes first, so that a thunk running a 'BO' computation performs its effects at most once even when two threads force it together; see Note [Pure Ref primitives run their effects at most once] in "Data.Ref.Linear.Unlifted.Internal".
-}
runBO# :: forall {rep} α (o :: TYPE rep). (State# (ForBO α) %1 -> o) %1 -> o
{-# INLINE runBO# #-}
runBO# = Unsafe.toLinear \f -> runRW# \s ->
  f (unsafeCoerce# (GHC.noDuplicate# s))

{- | Run a computation in the lifetime of the given 'Now', and hand the 'Now' back once the computation is over.

The 'Now' comes back through 'reviveNow', after the computation's last effect, so that the evidence of the lifetime's end derived from it depends on those effects; see Note [Owners handed back by reclaim].
-}
execBO :: BO α a %1 -> Now α %1 -> (Now α, a)
{-# INLINE execBO #-}
execBO bo !now = runBOResult Control.do
  !a <- bo
  now <- reviveNow now
  Control.pure (now, a)

-- | Run a computation and return its result, dropping the final state token.
runBOResult :: BO α a %1 -> a
{-# INLINE runBOResult #-}
runBOResult (BO f) = case runBO# f of
  (# s, !a #) -> dropState# s `PL.lseq` a

{- | Hand a 'Now' back after the effects that precede it, through a barrier the optimizer cannot see through.

Semantically this is 'Control.pure'; see Note [Owners handed back by reclaim] for why it is opaque and threads the state token.
-}
reviveNow :: Now α %1 -> BO β (Now α)
{-# OPAQUE reviveNow #-}
reviveNow now = BO \s -> (# s, now #)

dropState# :: State# a %1 -> ()
{-# INLINE dropState# #-}
dropState# = Unsafe.toLinear \ !_ -> ()

-- | See also 'Control.Monad.Borrow.Pure.scope'.
sexecBO :: BO (α /\ β) a %1 -> Now α %1 -> BO β (Now α, a)
{-# INLINE sexecBO #-}
sexecBO f now = Control.do
  a <- unsafeCastBO f
  -- See 'execBO': the 'Now' comes back after the computation's effects.
  now <- reviveNow now
  Control.pure (now, a)

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
-- 'runBO#' makes the evaluation undupable already.
unsafePerformEvaluateUndupableBO (BO f) = runBO# \s ->
  case f s of
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
Until 0.2.0.0 not every read of a borrowed resource was threaded through this monad's state token.
The growable vectors' header reads, the robin-hood hash map's table reads and `Data.Ref.Linear.Borrow.update` read a `Data.Ref.Linear.Ref` with `Data.Ref.Linear.unsafeReadRef`, an @INLINE@ pass-through to the `GHC.Magic.noinline`-wrapped `Data.Ref.Linear.Unlifted.unsafeReadRef#`, which opens its own `runRW#`.
To GHC such a read is a plain function of the borrow.
Two of them on the same borrow /variable/ are therefore the same Core expression, and common-subexpression elimination is entitled to serve the second from the first.

What kept that honest was that every operation which replaced a reference's contents wrote through `Data.Ref.Linear.Unlifted.unsafeWriteRef#`, which is @NOINLINE@, and handed back the reference that write returned.
The optimizer cannot see through it, so a read after a write scrutinised a different expression than a read before it, and the two did not merge.

A delimiter that returns the borrow its caller passed in throws that dependency away.
The restored borrow is then the caller's own binder, so a read after the scope is syntactically the read before it, and CSE deletes the later one -- serving a stale length and a stale buffer across every growth the scope performed.
Writing through the stale buffer at an index the fresh length admits runs off the end of the allocation, which is how this last surfaced downstream: a SIGSEGV inside the collector, or silently wrong data under a nursery large enough that the damage is never traversed.

So a delimiter restores its borrow through `reviveAlias`, and both of its properties are load-bearing.

Do not weaken the @OPAQUE@ to @NOINLINE@.
Measured on GHC 9.12.4 at -O2 a @NOINLINE@ version is an equally good barrier today, and for a knowable reason: the argument's demand comes out lazy and the result is already an unboxed tuple, so no worker/wrapper split occurs and no @$w@ worker is generated.
But that is a property of one demand signature rather than a guarantee, and @OPAQUE@ is the one pragma GHC documents as suppressing inlining, worker/wrapper, specialisation and rules together.
The barrier is the whole of the memory safety of these delimiters, so it should rest on a contract rather than on a coincidence.
The other barriers show that the coincidence is narrow: made @NOINLINE@, 'reviveOwner', 'reviveAliasWithEnd#' and 'endHere' are split by worker/wrapper, whose workers drop the token or return a constant one, and the kernels of the @pure-borrow-no-state-hack@ suite fail.

It lives in `BO` rather than being a pure function because a pure barrier depends on nothing the scope produced, so nothing would stop it -- or the reads that consume its result -- from floating above the scope body.
Consuming the state token pins the restoration after the scope's effects.
A pure @OPAQUE@ barrier also measures a few percent worse in a tight loop, but that is a side benefit and not the reason.

The cost is one out-of-line call per scope exit, around 0.7ns on aarch64-darwin with GHC 9.12.4, plus whatever it costs that a loop-carried borrow can no longer be unboxed past the barrier: measured together at 14-20% of a tight L1-resident loop that does nothing but the scope, and unmeasurable in any benchmark this repository ships.
No closure is allocated and the recursion remains a self tail call; what the call adds is a non-tail continuation and, in a loop, a boxed rather than unboxed loop-carried borrow.

Threading the reads through the state token, so that they are ordered like every other mutable operation, is the durable fix, and every read through a borrow in this package now takes it (see Note [Growable header reads] in "Data.Vector.Mutable.Growable.Linear.Borrow.Internal").
No read *through a borrow* can be served stale in the way above any more.
A read through an *owner* after a scope can, by the same mechanism, which Note [Owners handed back by reclaim] addresses.
The barrier stays all the same, because the obligation belongs to the delimiter rather than to the set of pure readers that happen to exist: a pure reader added later, here or in a data structure built on the @.Unsafe@ modules, would otherwise depend on it silently.
It is also one-directional -- it stops a post-scope read being served from a pre-scope one, but nothing stops a pre-scope read from sinking below the scope -- so it is no substitute for ordering reads by the state token.
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

{- | 'reviveAlias' for a delimiter that also discharges its continuation's 'After': it hands back, in the same out-of-line call, an t'EndToken' for the sublifetime the delimiter has just closed.

It restores a borrow or a bundle of them, such as the t'Control.Monad.Borrow.Pure.Experimental.Borrows.Muts' of 'Control.Monad.Borrow.Pure.Experimental.Borrows.reborrowings''.
Discharging the 'After' with this token rather than with the constant 'UnsafeEnd' makes whatever the 'After' reclaims depend on the scope's effects; see Note [Owners handed back by reclaim].
It carries the obligation of 'reviveAlias', and it asserts that @β@ has ended: call it only once the continuation typed in @β@, whose result type cannot mention @β@, has returned, as 'unsafeBorrowScope'' does.
-}
reviveAliasWithEnd# :: forall β α a. a %1 -> State# (ForBO α) %1 -> (# State# (ForBO α), EndToken β, a #)
{-# OPAQUE reviveAliasWithEnd# #-}
reviveAliasWithEnd# a s = (# s, UnsafeEnd, a #)

{- | An t'EndToken' taken from the state thread, for a delimiter that discharges an 'After' without restoring a borrow.

It asserts that @β@ has ended: call it only once the computation typed in @β@, whose result type cannot mention @β@, has returned, as 'Control.Monad.Borrow.Pure.BO.srunBO' does.
A delimiter that applied the constant 'UnsafeEnd' instead would lose the ordering that Note [Owners handed back by reclaim] relies on.
-}
endHere :: BO α (EndToken β)
{-# OPAQUE endHere #-}
endHere = BO \s -> (# s, UnsafeEnd #)

-- | 'withEnd' for a token bound linearly, as the delimiters receive it.
withEndL :: EndToken α %1 -> After α r %1 -> r
{-# INLINE withEndL #-}
withEndL = Unsafe.toLinear withEnd

{- | The exit of a delimiter that discharges an 'After': restore the borrow, or the bundle, and discharge the 'After' with the token that 'reviveAliasWithEnd#' hands back with it.

It asserts that @β@ has ended; see 'reviveAliasWithEnd#'.
-}
restoreWithEnd :: forall β α a r. a %1 -> After β r %1 -> BO α (r, a)
{-# INLINE restoreWithEnd #-}
restoreWithEnd a after = BO \s -> case reviveAliasWithEnd# @β a s of
  (# s, end, restored #) -> (# s, (withEndL end after, restored) #)

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

{- | Reclaims a 'borrow'ed resource at the 'End' of lifetime @α'.

The resource comes back through 'reviveOwner', applied to the evidence that @α@ has ended; see Note [Owners handed back by reclaim].
-}
reclaim :: forall α a. (End α) => Lend α a %1 -> a
reclaim = \(UnsafeAlias !a) -> reviveOwner (endToken @α) a

{-
Note [Owners handed back by reclaim]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'borrow' hands out its 'Mut' and its 'Lend' as the same object, and 'reclaim' hands that object back, so after inlining, the owner a scope returns is the very Core variable that went into 'borrow'.
Nothing in it depends on the writes made through the borrow in between: those are ordered by the state token, and the owner is not.
A pure read of the reclaimed owner ('Data.Ref.Linear.free', 'consume', a vector's @toList@) is then syntactically a pure read of that owner made before the borrow, and common-subexpression elimination serves the later one from the earlier.
This happened: 'Data.Ref.Linear.Dupable' for 'Data.Ref.Linear.Ref' read the reference purely and handed the same reference back, and a program that duplicated a reference, bumped one copy through a scope and freed it afterwards got the value from before the bump, or, for a reference to a reference, a second owner of a reference it had given away (the kernels of @Control.Monad.Borrow.Pure.OrderingSpec@).
Floating combined with strictness analysis is the other risk: the state token is @State# (ForBO α)@, which the demand analyser's IO hack, keyed on @State# RealWorld@, does not cover, and under @-fno-state-hack@ GHC floats an 'After' whose inputs all exist before the scope out of the state-threaded lambda and evaluates it before the scope's writes.

So the evidence that the lifetime has ended carries the dependency, and the reclaimed owner is computed from it:

\* 'execBO' and 'sexecBO' hand their 'Now' back through 'reviveNow', an opaque action that runs after the computation's last effect;
\* 'endLifetime' is opaque, so the 'EndToken' it derives from that 'Now' is the result of a call rather than a constant;
\* the erased delimiters take their token from the state thread rather than using the constant 'UnsafeEnd': 'reviveAliasWithEnd#' restores the borrow, or the bundle, and hands back the token in one out-of-line call, and 'Control.Monad.Borrow.Pure.BO.srunBO' calls 'endHere';
\* 'reclaim' returns the owner through 'reviveOwner', an opaque function of the token.

A read of the reclaimed owner is then a read of a call result whose inputs exist only once the scope's effects have run.

That holds only while the token stays an unknown value on its way from the call that produced it to 'reviveOwner'.
If the optimizer learns which value a token is, it puts that value in place of the variable, and 'reviveOwner' is then applied to a constant and the pre-scope owner, which exist before the scope.
With a single nullary constructor, forcing the token was enough to teach it: after a @case@ or a @seq@, the token is known to be that constructor.
This happened: while 'withEnd' forced its token, the 'Control.Monad.Borrow.Pure.BO.srunBO' kernels of @Control.Monad.Borrow.Pure.OrderingSpec@, built with @-fno-state-hack@, read the owner before the scope's writes, and one handed back an inner reference the scope had already freed, on GHC 9.10, 9.12 and 9.14; a function written with the safe modules alone, forcing the token before 'withEnd', did the same.
Two changes close it:

\* 'EndToken' has a lazy field that nothing reads, so forcing a token reveals only @UnsafeEndToken x@, with @x@ still the unknown result of the call.
  This is what protects a token that user code forces.
  It must stay a @data@ type with that field: a nullary constructor brings the problem back, and so does a newtype, whose field would be the token itself.
  'Now', from which 'endLifetime' derives the token, has such a field for the same reason; see Note [Tokens carry a field] in "Control.Monad.Borrow.Pure.Lifetime.Token.Internal".
\* 'withEnd' leaves the token alone, since nothing needs it forced before 'reviveOwner'.
  With the nullary token, that alone fixed the library's own delimiters, but not the user's function.

Both properties of 'reviveOwner' are load-bearing.
Its result must be opaque: with an inline @case@ on the token in its place, the kernels above fail again, because the alternative returns the caller's own variable.
And it must force the token: 'withEnd', which the safe modules export, accepts any 'EndToken', including a bottom one, so a 'reclaim' that ignored its token would hand an owner back while its borrows are still live, giving two live 'Mut's over one resource.
It forces the token inside its opaque body, and it covers a token supplied through 'withEnd' and through 'GHC.Exts.withDict' alike.
Do not make it @INLINE@ or @NOINLINE@, and do not replace it by a @case@: under @NOINLINE@, worker/wrapper splits it into a worker that drops the token, @$wreviveOwner = \a -> a@, which 'reclaim' calls on the owner alone.
The same holds for 'reviveNow', 'endLifetime', 'reviveAliasWithEnd#' and 'endHere': each is @OPAQUE@, which is what keeps its token an argument or a result of a call.

@OPAQUE@ hides the body but not the demand signature, and that of 'reviveOwner', @<1A><1L>@, says that the token is forced and its field unused.
That is the shape through which a caller's worker rebuilt a token from a constant while 'endLifetime' dropped the field of its 'Now' (Note [Tokens carry a field] in "Control.Monad.Borrow.Pure.Lifetime.Token.Internal"), and here it is harmless, for three reasons.
The one caller, 'reclaim', takes the token from the 'End' evidence, which comes only from 'withEnd' or 'GHC.Exts.withDict', and GHC desugars both through its wired-in @nospec@: the function that supplies the token sees an unknown function applied to it, not this demand.
The signature leaves the token boxed, so a function that has the evidence as a constraint passes the dictionary on as it is; it did so under @-fdicts-strict@ and @-fno-state-hack@ as well.
And no function of the safe modules takes an 'EndToken' apart and rebuilds it, which is where a constant would come in: the constructor is exported only from "Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe", and the one instance, of 'Data.Coerce.Directed.Internal.<:', is a coercion.
Programs that reclaim through a token taken as an argument, forced or not, or through the evidence of a constraint, and that read the owner both before the scope and after it, read the value the scope wrote under each of those flags.
Should any of the three change, for instance with a safe function that rebuilds an 'EndToken', make 'reviveOwner' use the field, as 'endLifetime' uses the field of its 'Now'.

The barrier is one-directional, like 'reviveAlias': it stops a read after the scope being served from one before it.
A read before the scope cannot sink below it, because 'borrow' consumes the owner it would read.

The cost is an out-of-line call per 'reclaim', two per run of 'execBO' ('reviveNow' and 'endLifetime'), and one per crossing of a delimiter that discharges an 'After'.
A closed 'After' is no longer floated to a constant, so on each such crossing 'withDict' runs as well; allocation is unchanged, except that an 'After' reclaiming an owner of several fields captures the fields.
-}

{- | Hand a reclaimed resource back through a barrier the optimizer cannot see through, strict in the end token.

Semantically this is the identity on the resource.
Both of its properties are load-bearing; see Note [Owners handed back by reclaim].
-}
reviveOwner :: EndToken α -> a %1 -> a
{-# OPAQUE reviveOwner #-}
reviveOwner UnsafeEnd a = a

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
'withEnd' comes from 'reviveAliasWithEnd#', which restores the borrow in the same call.
Asserting that the sublifetime has ended is sound for the same reason it is in
'Control.Monad.Borrow.Pure.BO.srunBO': the continuation has already returned, so
the sublifetime it was typechecked in is over by the time the token is applied,
and the caller-fixed result type cannot mention that lifetime.
Taking the token from the state thread, rather than using the constant 'UnsafeEnd', makes whatever the 'After' reclaims depend on the scope's effects; see Note [Owners handed back by reclaim].
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
    restoreWithEnd mut after

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

deriving anyclass instance DistributesAlias NonEmpty

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
