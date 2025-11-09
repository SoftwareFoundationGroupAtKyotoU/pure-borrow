{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Ref.Linear (
  Ref,
  new,
  freeRef,
  unsafeReadRef,
  unsafeWriteRef,
  atomicModify,
  atomicModify_,
) where

import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Data.Ref.Linear.Unlifted
import Unsafe.Linear qualified as Unsafe

data Ref a = Ref (Ref# a)

type role Ref nominal

new :: a %1 -> Linearly %1 -> Ref a
{-# INLINE new #-}
new a lin = Ref (newRef# a lin)

instance LinearOnly (Ref a) where
  linearOnly = UnsafeLinearOnly

instance Affine (Ref a) where
  aff = Unsafe.toLinear UnsafeAff

atomicModify_ :: (a %1 -> a) %1 -> Ref a %1 -> Ref a
{-# INLINE atomicModify_ #-}
atomicModify_ f (Ref v) = Ref (atomicModify_# f v)

atomicModify :: (a %1 -> (b, a)) %1 -> Ref a %1 -> (b, Ref a)
{-# INLINE atomicModify #-}
atomicModify f (Ref v) = case atomicModify# f v of
  (# b, v' #) -> (b, Ref v')

freeRef :: Ref a %1 -> a
{-# INLINE freeRef #-}
freeRef (Ref v) = freeRef# v

unsafeReadRef :: Ref a %1 -> (a, Ref a)
{-# INLINE unsafeReadRef #-}
unsafeReadRef (Ref v) = case unsafeReadRef# v of
  (# a, v' #) -> (a, Ref v')

unsafeWriteRef :: Ref a %1 -> a %1 -> Ref a
{-# INLINE unsafeWriteRef #-}
unsafeWriteRef (Ref v) a = Ref (unsafeWriteRef# v a)
