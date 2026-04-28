{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
This module provides 'Foldable' class, and provides a way to loop through it while reborrowing existing 'Borrow's into sublifetime.
The module also introduces 'Borrows', which is a heterogeneous list of 'Borrow's in the same lifetime.
-}
module Control.Monad.Borrow.Pure.Experimental.Loop (
  Borrows (..),
  forReborrowing,
  forReborrowingOf_,
  forReborrowing_,
  iforReborrowingOf_,
  iforReborrowing_,
  Fold,
  Foldable (..),
  IndexedFold,
  ifoldMapDefaultOf,
  FoldableWithIndex (..),
  traverse_,
  for_,
  toListOf,
  toList,
  foldBorrow,
  foldBorrowOf,
  GenericFoldable,
  genericFoldMap,
  ifoldMapDefault,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Experimental.Borrows
import Control.Monad.Borrow.Pure.Experimental.Reborrowable
import Control.Monad.Borrow.Pure.Unsafe
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Data.Bifunctor.Linear qualified as Bi
import Data.Functor.Linear qualified as Data
import Data.HashMap.Mutable.Linear qualified as LHM
import Data.List.NonEmpty.Linear (NonEmpty)
import Data.List.NonEmpty.Linear qualified as LNE
import Data.Monoid.Linear
import Data.Vector.Mutable.Linear qualified as LV
import Generics.Linear
import Prelude.Linear hiding (foldMap)
import Prelude.Linear qualified as PL
import Unsafe.Linear qualified as Unsafe

{- |
@'forReborrowingN' iterates over the elements of 'Data.Traversable' @t@
inside the delimited sublifetime, reborrowing the 'Borrows' in @bors@ for that sublifetime.
-}
forReborrowing ::
  (Data.Traversable t, Reborrowable bor) =>
  bor α xs %1 ->
  t b %1 ->
  ( forall β.
    bor (β /\ α) xs %1 ->
    b %1 ->
    BO (β /\ α) c
  ) ->
  BO α (t c, bor α xs)
{-# INLINE forReborrowing #-}
forReborrowing bors tb k =
  flip Control.runStateT bors $
    Data.for tb \a -> Control.StateT \bors ->
      locally bors (\bors -> k bors a)

type Fold s a = forall w. (Monoid w) => (a %1 -> w) -> s %1 -> w

-- See https://github.com/tweag/linear-base/issues/190 for the discussion.
class Foldable t where
  foldMap :: (Monoid w) => (a %1 -> w) -> t a %1 -> w

type IndexedFold i s a = forall w. (Monoid w) => (i %1 -> a %1 -> w) -> s %1 -> w

class (Foldable t) => FoldableWithIndex i t | t -> i where
  ifoldMap :: (Monoid w) => (i %1 -> a %1 -> w) -> t a %1 -> w
  default ifoldMap ::
    (Foldable t, i ~ Int, Monoid w) =>
    (i %1 -> a %1 -> w) -> t a %1 -> w
  ifoldMap = ifoldMapDefault
  {-# INLINE ifoldMap #-}

ifoldMapDefaultOf :: forall s a. Fold s a %1 -> IndexedFold Int s a
{-# INLINE ifoldMapDefaultOf #-}
ifoldMapDefaultOf fld k s =
  flip Control.evalState (Ur 0) $ unAp $ flip fld s $ \a -> Ap Control.do
    Ur i <- Control.get
    Control.put $ Ur $! i + 1
    Control.pure $ k i a

ifoldMapDefault :: (Foldable t) => IndexedFold Int (t a) a
{-# INLINE ifoldMapDefault #-}
ifoldMapDefault = ifoldMapDefaultOf foldMap

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

newtype Ap m a = Ap (m a)
  deriving newtype (Data.Functor, Control.Functor, Data.Applicative, Control.Applicative)

instance (Data.Applicative f, Semigroup w) => Semigroup (Ap f w) where
  (<>) = Data.liftA2 (<>)
  {-# INLINE (<>) #-}

instance (Data.Applicative f, Monoid w) => Monoid (Ap f w) where
  mempty = Data.pure mempty
  {-# INLINE mempty #-}

unAp :: Ap m a %1 -> m a
unAp (Ap m) = m
{-# INLINE unAp #-}

forReborrowingOf_ ::
  (Reborrowable bor) =>
  Fold s a %1 ->
  bor α xs %1 ->
  s %1 ->
  ( forall β.
    bor (β /\ α) xs %1 ->
    a %1 ->
    BO (β /\ α) ()
  ) ->
  BO α (bor α xs)
{-# INLINE forReborrowingOf_ #-}
forReborrowingOf_ fld bors s k =
  flip Control.execStateT bors $
    unAp $
      flip fld s $
        Ap . \a -> Control.StateT \bors -> locally bors (\bors -> k bors a)

forReborrowing_ ::
  (Foldable t, Reborrowable bor) =>
  bor α xs %1 ->
  t a %1 ->
  ( forall β.
    bor (β /\ α) xs %1 ->
    a %1 ->
    BO (β /\ α) ()
  ) ->
  BO α (bor α xs)
{-# INLINE forReborrowing_ #-}
forReborrowing_ = forReborrowingOf_ foldMap

iforReborrowingOf_ ::
  (Reborrowable bor) =>
  IndexedFold i s a %1 ->
  bor α xs %1 ->
  s %1 ->
  ( forall β.
    bor (β /\ α) xs %1 ->
    i %1 ->
    a %1 ->
    BO (β /\ α) ()
  ) ->
  BO α (bor α xs)
{-# INLINE iforReborrowingOf_ #-}
iforReborrowingOf_ fld bors s k =
  flip Control.execStateT bors $
    unAp $
      flip fld s \i a ->
        Ap $ Control.StateT \bors -> locally bors (\bors -> k bors i a)

iforReborrowing_ ::
  (FoldableWithIndex i t, Reborrowable bor) =>
  bor α xs %1 ->
  t a %1 ->
  ( forall β.
    bor (β /\ α) xs %1 ->
    i %1 ->
    a %1 ->
    BO (β /\ α) ()
  ) ->
  BO α (bor α xs)
{-# INLINE iforReborrowing_ #-}
iforReborrowing_ = iforReborrowingOf_ ifoldMap

toListOf :: Fold s a %1 -> s %1 -> [a]
{-# INLINE toListOf #-}
toListOf fld = fromDList . fld singletonDL

toList :: (Foldable t) => t a %1 -> [a]
{-# INLINE toList #-}
toList = toListOf foldMap

newtype DList a = DList ([a] %1 -> [a])

fromDList :: DList a %1 -> [a]
{-# INLINE fromDList #-}
fromDList (DList f) = f []

singletonDL :: a %1 -> DList a
{-# INLINE singletonDL #-}
singletonDL a = DList (a :)

instance Semigroup (DList a) where
  DList f <> DList g = DList (f . g)
  {-# INLINE (<>) #-}

instance Monoid (DList a) where
  mempty = DList id
  {-# INLINE mempty #-}

instance Foldable [] where
  foldMap = PL.foldMap
  {-# INLINE foldMap #-}

deriving anyclass instance FoldableWithIndex Int []

instance Foldable Maybe where
  foldMap f = maybe mempty f
  {-# INLINE foldMap #-}

instance FoldableWithIndex () Maybe where
  ifoldMap f = foldMap (f ())
  {-# INLINE ifoldMap #-}

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

instance Foldable LV.Vector where
  foldMap f vec =
    LV.size vec & \case
      (Ur n, vec) -> DataFlow.do
        let {-# INLINE loop #-}
            loop !vec !i !w
              | i < n =
                  LV.unsafeGet i vec & \(Ur a, vec) -> DataFlow.do
                    let !w' = w <> f a
                    loop vec (i + 1) w'
              | otherwise = vec `lseq` w
        loop vec 0 mempty
  {-# INLINE foldMap #-}

deriving anyclass instance FoldableWithIndex Int LV.Vector

instance Foldable (LHM.HashMap k) where
  foldMap f hm = foldMap (Unsafe.toLinear \(_, v) -> f v) $ unur $ LHM.toList hm

instance FoldableWithIndex k (LHM.HashMap k) where
  ifoldMap f = foldMap (uncurry f) . unur . LHM.toList
