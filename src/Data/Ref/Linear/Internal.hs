{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Ref.Linear.Internal (
  module Data.Ref.Linear.Internal,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine
import Control.Monad.Borrow.Pure.Affine.Unsafe (unsafeAff)
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..), unsafeSystemIOToBO)
import Control.Monad.Borrow.Pure.Clone
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
import Control.Monad.Borrow.Pure.Utils (evaluateStored)
import Data.Ref.Linear.Unlifted.Internal
import GHC.TypeError
import Prelude.Linear (Consumable (..), Dupable (..))
import Prelude.Linear qualified as PL
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

-- | Linearly owned mutable reference.
data Ref a = Ref (Ref# a)

type role Ref nominal

{- | Allocate a reference that owns the given value.

The value is evaluated to weak head normal form as the reference itself is evaluated, which borrowing it does.
A placeholder such as @undefined@ therefore raises then, even if nothing reads it, and an expensive value is computed by the thread that evaluates the reference, usually the parent before a 'Control.Monad.Borrow.Pure.parBO' forks, rather than by the branch that reads it.
To keep an expensive GC-owned value lazy, store it in a lazy box, such as t'Prelude.Linear.Ur'.
A lazy field inside the value stays unevaluated; see [Contents that are not evaluated yet]("Control.Monad.Borrow.Pure.Clone#lazy").
-}
new :: a %1 -> Linearly %1 -> Ref a
{-# INLINE new #-}
new a lin = Ref (newRef# a lin)

instance LinearOnly (Ref a) where
  linearOnly = UnsafeLinearOnly

instance (Consumable a) => Consumable (Ref a) where
  consume = consume PL.. free
  {-# INLINE consume #-}

-- | Copies the contents, with 'dup2', into a fresh reference.
instance (PL.Dupable a) => PL.Dupable (Ref a) where
  -- The original reference is read and written back through the opaque primitives, so the reference handed back is a new expression rather than the one that was read.
  -- Returning the reference that was read let a later pure read of it be merged with the read made here, serving the contents from before any write in between:
  -- see Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal".
  dup2 (Ref v) = case withLinearly# v of
    (# l, v #) -> case unsafeReadRef# v of
      (# a, v #) -> case dup2 a of
        (kept, copied) -> (Ref (unsafeWriteRef# v kept), new copied l)
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

{- | Replace the contents, which is unsafe, because the ownership of the previous contents is dropped.

The new contents are evaluated to weak head normal form as the write runs, as 'new' evaluates what it stores; see [Contents that are not evaluated yet]("Control.Monad.Borrow.Pure.Clone#lazy").
-}
unsafeWriteRef :: Ref a %1 -> a %1 -> Ref a
{-# INLINE unsafeWriteRef #-}
unsafeWriteRef (Ref v) a = Ref (unsafeWriteRef# v a)

{- | Read the contents inside 'BO', ordered by the state token after every earlier effect of the computation.

This is unsafe in the same way as 'unsafeReadRef': the ownership of the contents is duplicated.
The caller must either treat the value as a borrow of the contents, of the kind and lifetime of the borrow through which it reached the reference, and keep threading that borrow, or take the contents over and write a replacement back with 'unsafeWriteRefBO' before anything else can read the reference, as the read-modify-writes of "Data.Ref.Linear.Borrow" and the growable vectors' header updates do.
Unlike 'unsafeReadRef', it cannot be shared, floated or evaluated out of order, because it consumes and returns the state token.
-}
unsafeReadRefBO :: Ref a -> BO α a
{-# INLINE unsafeReadRefBO #-}
unsafeReadRefBO (Ref v) = unsafeSystemIOToBO (unsafeReadRefIO# v)

{- | Replace the contents inside 'BO', ordered by the state token after every earlier effect of the computation.

This is unsafe in the same way as 'unsafeWriteRef': the ownership of the previous contents is dropped, so the caller must already have taken it over, as a read-modify-write does.
The new contents are forced to WHNF before storing them.
The BO run hides demand from its caller; evaluation may move within that run, so this does not specify precise exception ordering.
See Note [Demand stays inside a BO run] in "Control.Monad.Borrow.Pure.BO.Internal".
-}
unsafeWriteRefBO :: Ref a -> a %1 -> BO α ()
{-# INLINE unsafeWriteRefBO #-}
unsafeWriteRefBO (Ref v) = Unsafe.toLinear \a -> unsafeSystemIOToBO (evaluateStored a NonLinear.>>= unsafeWriteRefIO# v)

instance
  (Unsatisfiable (ShowType (Ref a) :<>: Text " cannot be copied!" :$$: Text "It is mutable: clone a shared borrow of it inside BO with 'clone' instead.")) =>
  Copyable (Ref a)
  where
  copy = unsatisfiable

{- | The contents are cloned through a shared borrow of them, with their own 'Clone', into a fresh reference.

The original is only read, so any number of 'Control.Monad.Borrow.Pure.parBO' branches may clone the same reference at once, unless the contents hold, in a lazy field, a call that is not evaluated yet and updates memory in place, such as linear-base's @Data.Array.Mutable.Linear.map@ or the owned hash map's @insert@: see [Contents that are not evaluated yet]("Control.Monad.Borrow.Pure.Clone#lazy").
'new' evaluates the contents themselves.
See Note [Cloning the contents of a shared borrow] in @Data.Ref.Linear.Internal@.
-}
instance (Clone a) => Clone (Ref a) where
  clone = Unsafe.toLinear \(UnsafeAlias ref) -> Control.do
    contents <- unsafeReadRefBO ref
    copied <- clone (UnsafeAlias contents)
    lin <- askLinearly
    -- Allocate here rather than leave a thunk to allocate on first use.
    Control.pure PL.$! new copied lin
  {-# INLINE clone #-}

{-
Note [Cloning the contents of a shared borrow]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'clone' of a container that owns its contents, such as 'Ref' or the boxed vectors, clones each piece of the contents through a shared borrow of that piece, with the contents' own 'Clone'.
It never consumes the original, nor writes to it.
A shared borrow of the container may already have handed out shared borrows of the contents, through 'Data.Ref.Linear.Borrow.readShare' or a vector read, and those must go on seeing the original, unchanged, until their lifetime ends.

0.1.0.0 required 'Dupable' of the contents instead, and duplicated each piece with 'dup2', which consumes its argument, so one of the two copies had to take the original's place.
No choice of that copy is sound for every lawful 'Dupable', because linear-base's laws do not say which copy, if either, is the original.
Dropping the first copy, as 0.1.0.0 did, consumed the original twice when 'dup2' returns two fresh copies.
Writing the first copy back hands the original to the clone when 'dup2' returns it second, so that the clone could mutate what a live shared borrow still reads.
That write also raced with readers of the same contents in a 'Control.Monad.Borrow.Pure.parBO' sibling.
-}
