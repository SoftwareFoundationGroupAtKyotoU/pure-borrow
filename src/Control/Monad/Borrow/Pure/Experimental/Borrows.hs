{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
The module provides 'Aliases', which is a heterogeneous list of 'Alias'es in the same lifetime.
-}
module Control.Monad.Borrow.Pure.Experimental.Borrows (
  Aliases (..),
  Muts,
  Shares,
  Borrows,
  Lends,
  reborrows,
  reborrowings',
  reborrowings,
  reborrowings_,
  reviveAliases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad qualified as NonLinear
import Control.Monad.Borrow.Pure.Affine
import Control.Monad.Borrow.Pure.Affine.Unsafe (unsafeAff)
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Internal
import Control.Monad.Borrow.Pure.Experimental.Reborrowable
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe qualified as Unsafe.Token
import Data.Coerce.Directed.Unsafe
import Data.Kind
import Prelude.Linear hiding (foldMap)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Linear qualified as Unsafe

#ifdef PURE_BORROW_SLOW_SCOPES
import Control.Syntax.DataFlow qualified as DataFlow
#endif

type Aliases :: AliasKind -> [Type] -> Type
data Aliases k xs where
  BNil :: Aliases k '[]
  (:-) :: !(Alias k x) %1 -> !(Aliases k xs) %1 -> Aliases k (x ': xs)

type role Aliases nominal nominal

infixr 5 :-

type Lends :: Lifetime -> [Type] -> Type
type Lends α = Aliases ('Lend α)

type Borrows :: BorrowKind -> Lifetime -> [Type] -> Type
type Borrows bk α = Aliases ('Borrow bk α)

type Muts :: Lifetime -> [Type] -> Type
type Muts α = Borrows 'Mut α

type Shares :: Lifetime -> [Type] -> Type
type Shares α = Borrows 'Share α

{- |
Only borrow bundles are affine; a @'Lends' α xs@ must not be discardable.
Dropping a bundle of lenders would abandon the owners it holds, and a lender is the sole capability to 'reclaim' one.
The @k ~ \'Borrow bk α@ constraint therefore excludes @\'Lend α@, exactly as the scalar @'Affine' ('Alias' ak a)@ instance does.
-}
instance (k ~ 'Borrow bk α) => Affine (Aliases k xs) where
  aff = unsafeAff
  {-# INLINE aff #-}

deriving via
  AsAffine (Aliases k xs)
  instance
    (k ~ 'Borrow bk α) =>
    Consumable (Aliases k xs)

instance (k ~ 'Borrow 'Share α) => Dupable (Aliases k xs) where
  dup2 = Unsafe.toLinear $ NonLinear.join (,)
  {-# INLINE dup2 #-}

instance (k ~ 'Borrow 'Share α) => Movable (Aliases k xs) where
  move = Unsafe.toLinear Ur
  {-# INLINE move #-}

instance (α >= β, xs <: ys, ys <: xs) => Muts α xs <: Muts β ys where
  subtype = UnsafeSubtype

instance (α >= β, xs <: ys) => Shares α xs <: Shares β ys where
  subtype = UnsafeSubtype

instance (α <= β, a <: b) => Lends α a <: Lends β b where
  subtype = UnsafeSubtype

instance Reborrowable (Muts α) where
  type LifetimeOf (Muts α) = α
  type WithLifetime (Muts α) β = Muts β
  locally' = reborrowings'
  {-# INLINE locally' #-}
  locally = reborrowings
  {-# INLINE locally #-}
  locally_ = reborrowings_
  {-# INLINE locally_ #-}

-- | A plural form of 'reborrow', which reborrows multiple borrows in the given 'Muts' at once.
reborrows :: forall β α a. (α >= β) => Muts α a %1 -> (Muts β a, Lend β (Muts α a))
reborrows = Unsafe.toLinear \v -> (unsafeCoerce v, unsafeCoerce v)

{- | Return a bundle of borrows to the caller of a delimiter, through a barrier the optimizer cannot see through.

This is the plural counterpart of 'Control.Monad.Borrow.Pure.BO.Unsafe.reviveAlias', and it exists for the same reason.
See Note [Restoring a borrow must break its Core identity] in "Control.Monad.Borrow.Pure.BO.Internal".

'reborrowings'' would otherwise restore the caller's own occurrence, since 'reborrows' hands the same value out as both borrow and lender and 'reclaim' is a newtype unwrap.
It happens not to misbehave today, because 'reclaim'' is reached through 'withEnd', whose @withDict@ desugars through the wired-in @nospec@ and survives every Core-to-Core pass — but that is a coincidence of one desugaring, and it is exactly the kind of accident the Note argues a delimiter must not rest on.

This is exported so that the Core obligations in @pure-borrow-inspection@ can state what the erased plural delimiters must compile to, barrier included.
Exporting it weakens nothing: it is 'Control.Functor.Linear.pure' behind an @OPAQUE@, so the worst a caller can do with it is add a barrier that was not needed.
-}
reviveAliases :: Aliases k xs %1 -> BO α (Aliases k xs)
{-# OPAQUE reviveAliases #-}
reviveAliases as = Control.pure as

{- |
Retag a bundle's lifetime, leaving its borrow kind, spine, order and payloads
untouched.

The plural counterpart of 'Control.Monad.Borrow.Pure.BO.Unsafe.unsafeCastAlias',
and unlike it this cannot be a @coerceLin@: 'Aliases' is a GADT rather than a
newtype over its payload, so no 'Data.Coerce.Coercible' relates two alias kinds
and the coercion has to be a raw one.

Why that raw coercion is representationally sound: @k@ occurs in 'Aliases' only
inside the @!('Alias' k x)@ field of @(':-')@, and 'Alias' is a newtype over
@x@, so the alias kind has no runtime witness anywhere in the structure and two
kinds give the same layout. That is precisely the argument @type role Aliases
nominal nominal@ declines to make on the caller's behalf, and this function
punches through it, so the argument has to be made here instead.

The caller's obligation is the narrower one the signature now enforces in part:
the retagged bundle must be a lifetime narrowing the type system would have
permitted, and it must not escape the scope that narrowed it. Keeping the
borrow kind fixed in the type is deliberate — every use in this module narrows
a lifetime and nothing more, and a future edit that retagged a 'Lends' bundle
as a 'Muts' one should be a type error rather than a silence.
-}
unsafeCastAliases :: Borrows bk α xs %1 -> Borrows bk β xs
{-# INLINE unsafeCastAliases #-}
unsafeCastAliases = Unsafe.toLinear unsafeCoerce

{-
Note [The plural delimiters are separate functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These three mirror `unsafeBorrowScope`, `unsafeBorrowScope_` and
`unsafeBorrowScope'` line for line, and discharge exactly the obligations
documented there; only the retagging and the restoration barrier differ, because
`Muts α xs` is `Aliases ('Borrow 'Mut α) xs` rather than an `Alias`.

They are not written as one delimiter shared with the scalar case.
A shared one would have to abstract over the type constructor and over the
lifetime retagging, which is what `Reborrowable`'s `WithLifetime` does -- and
instantiating an unsafe coercion at that class would make it available to
third-party instances that need not use this representation at all.
That is the coercion the design forbids, so per-representation delimiters plus
instance-supplied methods is the shape, and the duplication is the price.

They are deliberately not exported.
A trusted delimiter belongs behind the combinators that discharge its
obligations, and `Experimental.Borrows` is a documented module rather than an
`.Internal` one.
-}

{- |
The plural non-finalizing delimiter, restoring the bundle on normal return.

See Note [The plural delimiters are separate functions] and the obligations on
'Control.Monad.Borrow.Pure.BO.Unsafe.unsafeBorrowScope'.
-}
unsafeBorrowsScope ::
  forall α α' xs r.
  Muts α xs %1 ->
  (forall β. Muts (β /\ α) xs %1 -> BO (β /\ α') r) %1 ->
  BO α' (r, Muts α xs)
{-# INLINE unsafeBorrowsScope #-}
unsafeBorrowsScope = Unsafe.toLinear2 \muts k ->
  unsafeSrunBO_ Control.do
    r <- k (unsafeCastAliases muts)
    (r,) Control.<$> reviveAliases muts

{- |
The plural result-discarding delimiter.

The result is consumed in the returned value rather than at scope exit, so this
stays observationally equivalent to the implementation @+slow@ restores.
-}
unsafeBorrowsScope_ ::
  forall α α' xs r.
  (Consumable r) =>
  Muts α xs %1 ->
  (forall β. Muts (β /\ α) xs %1 -> BO (β /\ α') r) %1 ->
  BO α' (Muts α xs)
{-# INLINE unsafeBorrowsScope_ #-}
unsafeBorrowsScope_ = Unsafe.toLinear2 \muts k ->
  unsafeSrunBO_ Control.do
    r <- k (unsafeCastAliases muts)
    restored <- reviveAliases muts
    Control.pure (consume r `lseq` restored)

{- |
The plural finalizing delimiter, whose continuation returns its result 'After'
the sublifetime.

The 'EndToken' is the runtime-erased one, sound for the reason given on
'Control.Monad.Borrow.Pure.BO.Unsafe.unsafeBorrowScope'': the continuation has
returned by the time it is applied, so the sublifetime it was typechecked in is
over, and the caller-fixed result type cannot mention it.
-}
unsafeBorrowsScope' ::
  forall α α' xs r.
  Muts α xs %1 ->
  (forall β. Muts (β /\ α) xs %1 -> BO (β /\ α') (After β r)) %1 ->
  BO α' (r, Muts α xs)
{-# INLINE unsafeBorrowsScope' #-}
unsafeBorrowsScope' = Unsafe.toLinear2 \muts k ->
  unsafeSrunBO_ Control.do
    after <- k (unsafeCastAliases muts)
    (withEnd Unsafe.Token.UnsafeEnd after,) Control.<$> reviveAliases muts

-- | A plural form of 'reborrowing''.
reborrowings' ::
  Muts α a %1 ->
  (forall β. Muts (β /\ α) a %1 -> BO (β /\ α') (After β r)) %1 ->
  BO α' (r, Muts α a)
{-# INLINE reborrowings' #-}
#ifdef PURE_BORROW_SLOW_SCOPES
reborrowings' v k = Control.do
  (r, restored) <- srunBO DataFlow.do
    (v, lend) <- reborrows v
    Control.do
      v <- k v
      Control.pure $ (,) Control.<$> v Control.<*> upcast (reclaim' lend)
  (r,) Control.<$> reviveAliases restored
#else
reborrowings' = unsafeBorrowsScope'
#endif

-- | A plural form of 'reborrowing'.
reborrowings ::
  Muts α a %1 ->
  (forall β. Muts (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (r, Muts α a)
{-# INLINE reborrowings #-}
#ifdef PURE_BORROW_SLOW_SCOPES
reborrowings mutα k = reborrowings' mutα (\mut -> Control.pure Control.<$> k mut)
#else
reborrowings = unsafeBorrowsScope
#endif

-- | A plural form of 'reborrowing_'.
reborrowings_ ::
  (Consumable r) =>
  Muts α a %1 ->
  (forall β. Muts (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (Muts α a)
{-# INLINE reborrowings_ #-}
#ifdef PURE_BORROW_SLOW_SCOPES
reborrowings_ mutα k = reborrowings mutα (Control.fmap consume . k) Control.<&> \((), a) -> a
#else
reborrowings_ = unsafeBorrowsScope_
#endif
