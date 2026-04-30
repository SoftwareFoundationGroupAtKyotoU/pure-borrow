{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Ref.Linear (
  Ref,
  new,
  free,
  unsafeReadRef,
  unsafeWriteRef,
  atomicModify,
  atomicModify_,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine
import Control.Monad.Borrow.Pure.Affine.Unsafe (unsafeAff)
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..))
import Control.Monad.Borrow.Pure.Clone
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
import Data.Ref.Linear.Unlifted
import GHC.TypeError
import Prelude.Linear (Consumable (..), Dupable (..))
import Prelude.Linear qualified as PL
import Unsafe.Linear qualified as Unsafe

-- | Linearly owned mutable reference.
data Ref a = Ref (Ref# a)

type role Ref nominal

new :: a %1 -> Linearly %1 -> Ref a
{-# INLINE new #-}
new a lin = Ref (newRef# a lin)

instance LinearOnly (Ref a) where
  linearOnly = UnsafeLinearOnly

instance (Consumable a) => Consumable (Ref a) where
  consume = consume PL.. free
  {-# INLINE consume #-}

instance (PL.Dupable a) => PL.Dupable (Ref a) where
  dup2 = Unsafe.toLinear \ !v ->
    withLinearly v PL.& \(l, !v) ->
      let !v2 = Unsafe.toLinear (\(!_, !v) -> v) PL.$ dup2 PL.$ free v
       in (v, new v2 l)
  {-# INLINE dup2 #-}

instance Affine (Ref a) where
  aff = unsafeAff

atomicModify_ :: (a %1 -> a) %1 -> Ref a %1 -> Ref a
{-# INLINE atomicModify_ #-}
atomicModify_ f (Ref v) = Ref (atomicModify_# f v)

atomicModify :: (a %1 -> (b, a)) %1 -> Ref a %1 -> (b, Ref a)
{-# INLINE atomicModify #-}
atomicModify f (Ref v) = case atomicModify# f v of
  (# b, v' #) -> (b, Ref v')

free :: Ref a %1 -> a
{-# INLINE free #-}
free (Ref v) = freeRef# v

unsafeReadRef :: Ref a %1 -> (a, Ref a)
{-# INLINE unsafeReadRef #-}
unsafeReadRef (Ref v) = case unsafeReadRef# v of
  (# a, v' #) -> (a, Ref v')

unsafeWriteRef :: Ref a %1 -> a %1 -> Ref a
{-# INLINE unsafeWriteRef #-}
unsafeWriteRef (Ref v) a = Ref (unsafeWriteRef# v a)

instance
  (Unsatisfiable (ShowType (Ref a) :<>: Text " cannot be copied!")) =>
  Copyable (Ref a)
  where
  copy = unsatisfiable

instance (Dupable a) => Clone (Ref a) where
  clone = Unsafe.toLinear \(UnsafeAlias ref) -> Control.do
    !a <- Control.pure PL.$ free ref
    !a' <- Unsafe.toLinear (\(!_, !a') -> Control.pure a') PL.$ PL.dup a
    new a' Control.<$> askLinearly
  {-# INLINE clone #-}
