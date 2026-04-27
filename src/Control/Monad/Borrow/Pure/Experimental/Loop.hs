{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
This module provides 'Foldable' class, and provides a way to loop through it while reborrowing existing 'Borrow's into sublifetime.
The module also introduces 'Borrows', which is a heterogeneous list of 'Borrow's in the same lifetime.
-}
module Control.Monad.Borrow.Pure.Experimental.Loop (
  Borrows (..),
  forReborrowingN,
  forReborrowingNOf_,
  forReborrowingN_,
  Foldable (..),
  traverse_,
  for_,
  foldBorrowOf,
  foldBorrow,
  forReborrowing,
  forReborrowing_,
  GenericFoldable,
  genericFoldMap,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Affine
import Control.Monad.Borrow.Pure.Affine.Unsafe (unsafeAff)
import Control.Monad.Borrow.Pure.Unsafe
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Data.Bifunctor.Linear qualified as Bi
import Data.Coerce.Directed
import Data.Functor.Linear qualified as Data
import Data.Kind
import Data.List.NonEmpty.Linear (NonEmpty)
import Data.List.NonEmpty.Linear qualified as LNE
import Data.Monoid (Ap (..))
import Generics.Linear
import Prelude.Linear hiding (foldMap)
import Prelude.Linear qualified as PL
import Unsafe.Linear qualified as Unsafe

type Borrows :: BorrowKind -> Lifetime -> [Type] -> Type
data Borrows bk α xs where
  BNil :: Borrows bk α '[]
  (:-) :: !(Borrow bk α x) %1 -> !(Borrows bk α xs) %1 -> Borrows bk α (x ': xs)

instance Affine (Borrows bk α xs) where
  aff = unsafeAff

deriving via AsAffine (Borrows bk α xs) instance Consumable (Borrows bk α xs)

instance (β <= α) => Borrows bk α xs <: Borrows bk' β xs where
  subtype = UnsafeSubtype

infixr 5 :-

{- |
@'forReborrowingN' iterates over the elements of 'Data.Traversable' @t@
inside the delimited sublifetime, reborrowing the 'Borrows' in @bors@ for that sublifetime.
-}
forReborrowingN ::
  (Data.Traversable t) =>
  Borrows bk α xs %1 ->
  t b %1 ->
  ( forall β.
    Borrows bk (β /\ α) xs %1 ->
    b %1 ->
    BO (β /\ α) c
  ) ->
  BO α (t c, Borrows bk α xs)
{-# INLINE forReborrowingN #-}
forReborrowingN = Unsafe.toLinear \ !bors tb k -> Control.do
  tc <- Data.forM tb \b -> srunBO Control.do
    !c <- k (upcast bors) b
    Control.pure $ After c
  Control.pure (tc, bors)

type Fold s a = forall w. (Monoid w) => (a %1 -> w) -> s %1 -> w

-- See https://github.com/tweag/linear-base/issues/190 for the discussion.
class Foldable t where
  foldMap :: (Monoid w) => (a %1 -> w) -> t a %1 -> w

foldBorrowOf :: Fold s a %1 -> Fold (Borrow bk α s) (Borrow bk α a)
{-# INLINE foldBorrowOf #-}
foldBorrowOf fld k = fld (k . UnsafeAlias) . unsafeUnalias

foldBorrow :: (Foldable t) => Fold (Borrow bk α (t a)) (Borrow bk α a)
{-# INLINE foldBorrow #-}
foldBorrow = foldBorrowOf foldMap

traverse_ :: (Foldable t, Data.Applicative m) => (a %1 -> m ()) -> t a %1 -> m ()
{-# INLINE traverse_ #-}
traverse_ f = unAp . foldMap (Ap . f)

for_ :: (Foldable t, Data.Applicative m) => t a %1 -> (a %1 -> m ()) -> m ()
{-# INLINE for_ #-}
for_ = flip traverse_

unAp :: Ap m a %1 -> m a
unAp (Ap m) = m
{-# INLINE unAp #-}

forReborrowingNOf_ ::
  Fold s a %1 ->
  Borrows bk α xs %1 ->
  s %1 ->
  ( forall β.
    Borrows bk (β /\ α) xs %1 ->
    a %1 ->
    BO (β /\ α) ()
  ) ->
  BO α (Borrows bk α xs)
forReborrowingNOf_ fld = Unsafe.toLinear \bors s k ->
  case fld (\a -> Ap $ srunBO $ After () Control.<$ k (upcast bors) a) s of
    Ap m -> bors Control.<$ m

forReborrowingN_ ::
  (Foldable t) =>
  Borrows bk α xs %1 ->
  t a %1 ->
  ( forall β.
    Borrows bk (β /\ α) xs %1 ->
    a %1 ->
    BO (β /\ α) ()
  ) ->
  BO α (Borrows bk α xs)
{-# INLINE forReborrowingN_ #-}
forReborrowingN_ = forReborrowingNOf_ foldMap

forReborrowing ::
  (Data.Traversable t) =>
  Borrow bk α x %1 ->
  t b %1 ->
  ( forall β.
    Borrow bk (β /\ α) x %1 ->
    b %1 ->
    BO (β /\ α) c
  ) ->
  BO α (t c, Borrow bk α x)
{-# INLINE forReborrowing #-}
forReborrowing bor t k = Control.do
  Bi.second unSingleton Control.<$> forReborrowingN (bor :- BNil) t (k . unSingleton)

forReborrowing_ ::
  (Foldable t) =>
  Borrow bk α x %1 ->
  t b %1 ->
  ( forall β.
    Borrow bk (β /\ α) x %1 ->
    b %1 ->
    BO (β /\ α) ()
  ) ->
  BO α (Borrow bk α x)
{-# INLINE forReborrowing_ #-}
forReborrowing_ bor t k = Control.do
  unSingleton Control.<$> forReborrowingN_ (bor :- BNil) t (k . unSingleton)

unSingleton :: Borrows bk α '[x] %1 -> Borrow bk α x
{-# INLINE unSingleton #-}
unSingleton = \case (bor :- BNil) -> bor

instance Foldable [] where
  foldMap = PL.foldMap
  {-# INLINE foldMap #-}

instance Foldable Maybe where
  foldMap f = maybe mempty f
  {-# INLINE foldMap #-}

instance (Consumable e) => Foldable ((,) e) where
  foldMap f = uncurry lseq . Bi.bimap consume f
  {-# INLINE foldMap #-}

instance (Consumable e) => Foldable (Either e) where
  foldMap f = either ((`lseq` mempty) . consume) f
  {-# INLINE foldMap #-}

instance Foldable NonEmpty where
  foldMap f = foldMap f . LNE.toList
  {-# INLINE foldMap #-}

instance Foldable U1 where
  foldMap _f = \U1 -> mempty
  {-# INLINE foldMap #-}

instance Foldable V1 where
  foldMap _ = \case {}
  {-# INLINE foldMap #-}

instance (Foldable f) => Foldable (M1 i c f) where
  foldMap f = coerceLin $ foldMap @f f
  {-# INLINE foldMap #-}

instance (Foldable f) => Foldable (MP1 m f) where
  foldMap f (MP1 x) = foldMap f x
  {-# INLINE foldMap #-}

instance (Foldable f, Foldable g) => Foldable (f :*: g) where
  foldMap f (x :*: y) = foldMap f x <> foldMap f y

instance (Foldable f, Foldable g) => Foldable (f :+: g) where
  foldMap f = \case
    L1 x -> foldMap f x
    R1 y -> foldMap f y
  {-# INLINE foldMap #-}

type GenericFoldable t = (Generic1 t, Foldable (Rep1 t))

genericFoldMap :: (GenericFoldable t, Monoid w) => (a %1 -> w) -> t a %1 -> w
{-# INLINE genericFoldMap #-}
genericFoldMap f = foldMap f . from1

instance (GenericFoldable t) => Foldable (Generically1 t) where
  foldMap f = genericFoldMap f . (\(Generically1 x) -> x)
  {-# INLINE foldMap #-}
