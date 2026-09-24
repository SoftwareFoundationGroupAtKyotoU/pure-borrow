{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

{- | The primitives behind "Data.Ref.Linear.Unlifted", including the ones that run inside 'SystemIO.IO' and are meant only for the library's own state-threaded accessors.

See Note [Pure Ref primitives run their effects at most once].
-}
module Data.Ref.Linear.Unlifted.Internal (
  Ref# (..),
  newRef#,
  freeRef#,
  unsafeReadRef#,
  unsafeWriteRef#,
  unsafeReadRefIO#,
  unsafeWriteRefIO#,
  atomicModify_#,
  atomicModify#,
) where

import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (LinearOnly (..), LinearOnlyWitness (..))
import Control.Monad.Borrow.Pure.Utils (lseq#)
import GHC.Exts
import GHC.Exts qualified as GHC
import GHC.IO qualified as SystemIO
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

newtype Ref# a = Ref# (MutVar# RealWorld a)

type role Ref# nominal

{-
Note [Pure Ref primitives run their effects at most once]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The primitives below are pure functions that open their own 'runRW#'.
Linearity promises that each result is used once, but not that it is evaluated by one thread: a result stored unevaluated where a 'Control.Monad.Borrow.Pure.Share' can reach it, and read by both branches of a 'Control.Monad.Borrow.Pure.parBO', is entered by two threads at once under GHC's lazy blackholing.
A primitive that allocates or writes would then run twice, which doubled an 'atomicModify_' increment in most runs of such a program.
So every primitive that allocates or writes calls 'GHC.noDuplicate#' before its effect, as 'System.IO.Unsafe.unsafePerformIO' does: the thread that reaches it second finds the thunk claimed and waits for the first one's result.
The primitives that only read ('unsafeReadRef#', 'freeRef#') need no such guard, since running a read twice returns the same value.
'GHC.noDuplicate#' is a no-op with a single capability.
It does not help a thunk built in a module compiled with @-feager-blackholing@, where two threads were still observed to run the effect.
-}

{-
Note [Stored contents are evaluated after noDuplicate#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A linearly supplied value can itself be an unevaluated call that writes memory in place, such as linear-base's @Data.Array.Mutable.Linear.map f arr@, whose primitives run their effects under 'GHC.runRW#' without 'GHC.noDuplicate#'.
Stored unevaluated where a 'Control.Monad.Borrow.Pure.Share' can reach it, such a call can be entered by both branches of a 'Control.Monad.Borrow.Pure.parBO' at once, and both run it: @map (+ 1)@ then adds 2 to some elements, and a @map@ that changes the element type crashes the program.
So every primitive and safe constructor that takes a value linearly and stores it evaluates the value to WHNF first, and so does every write through a 'Control.Monad.Borrow.Pure.Mut'.
The value must be evaluated once, where one thread does it, and GHC must not see the demand on it.

Where it is evaluated.
The primitives and constructors that open their own 'GHC.runRW#' or 'System.IO.Unsafe.unsafePerformIO', 'newRef#' and 'unsafeWriteRef#' here and the @fromList@ of the boxed, unboxed and multiplicity vectors, evaluate the value after 'GHC.noDuplicate#', in the same 'GHC.runRW#' as their effect.
Two threads that enter one unevaluated call of such a constructor then evaluate the value once: the one that reaches the guard second finds the call claimed, and waits.
The writes through a 'Control.Monad.Borrow.Pure.Mut' ('set', 'update', 'push' and 'write' of the vectors, and the updates of "Data.Ref.Linear.Borrow") evaluate the value where they run, in a 'Control.Monad.Borrow.Pure.BO' action, which runs after the guard of its run.
'atomicModify#' and 'atomicModify_#' store @f x@ unevaluated and evaluate it right after, under their own guard.
A constructor that takes its contents unrestricted, such as @fromVector@ or @fromMutable@, stores them as they are: such a value cannot hold a linear call that writes in place without an unsafe coercion.

Why the demand must stay hidden.
A bang, or a strict field, on the stored value would let GHC make every function that stores it strict in the value, and through worker/wrapper a caller would then evaluate the value before the call, outside any guard the call runs under.
When the call is itself left unevaluated where two branches enter it, as a component of a pair, both would evaluate the value: a function that ran 'Control.Monad.Borrow.Pure.modifyBO_' over a vector and wrote an argument into it with @set@ ran an @Array.map@ passed to it twice, and summed heap pointers as 'Int's when the map changed the element type.
So the primitives evaluate with 'GHC.seq#', which orders the evaluation after 'GHC.noDuplicate#' by the state token and hides the demand; they are applied through 'GHC.noinline', so that no caller sees their strictness either.
The BO runner instead hides demand on its complete state continuation until after the guard, so writes may use ordinary strict evaluation without forcing an unboxed arithmetic loop to allocate a thunk per element.
See Note [Demand stays inside a BO run] in Control.Monad.Borrow.Pure.BO.Internal.
This includes ordinary unboxed Int values computed by consuming a linear owner, not only boxed representations such as DoNotUnboxLazy.
The fromLists evaluate construction inside GHC.seq#, with each element forced before the strict construction stream yields it (Control.Monad.Borrow.Pure.Utils.evaluatingBundle).
The stream fuses with the vector builder without an intermediate list.

What it does not cover.
The value is evaluated only to WHNF: a lazy field inside it, such as a component of a pair or the payload of a 'Just', is not, and remains the hazard that "Control.Monad.Borrow.Pure.Clone#lazy" describes.
Storing a value evaluates it, so storing a 'Control.Monad.Borrow.Pure.Share' evaluates the value that it points to: a branch that stores a 'Control.Monad.Borrow.Pure.Share' of such a lazy field, in a reference, a vector or through a write, can evaluate the field while its sibling does.
An explicit force outside the BO action, such as a bang on an argument of a user's wrapper function, remains outside the run's guard.
As with Note [Pure Ref primitives run their effects at most once], none of this helps a call left unevaluated in a module compiled with @-feager-blackholing@, where two threads were still observed to evaluate the value.
The evaluation also moves: the thread that first forces the owner computes the contents, usually the parent at 'Control.Monad.Borrow.Pure.borrow', before a 'Control.Monad.Borrow.Pure.parBO' forks, where branches reading different contents used to compute them in parallel.

"Control.Monad.Borrow.Pure.SharedEffectSpec" races both branches of a 'Control.Monad.Borrow.Pure.parBO' over each constructor and each write, stored directly, left unevaluated in a pair, and made by a function that takes the call as an argument, which exposing the run's demand to its caller fails.
-}

newRef# :: a %1 -> Linearly %1 -> Ref# a
{-# NOINLINE newRef# #-}
newRef# = GHC.noinline $ Unsafe.toLinear $ \a lin ->
  lin
    `lseq#` GHC.runRW# \s ->
      -- See Note [Stored contents are evaluated after noDuplicate#].
      case GHC.seq# a (GHC.noDuplicate# s) of
        (# s, a #) -> case GHC.newMutVar# a s of
          (# !_, !v #) -> Ref# v

-- | This is unsafe, because the ownership of 'a' is duplicated.
unsafeReadRef# :: Ref# a %1 -> (# a, Ref# a #)
{-# NOINLINE unsafeReadRef# #-}
unsafeReadRef# = GHC.noinline $ Unsafe.toLinear \(Ref# !mv) ->
  runRW# \s ->
    case GHC.readMutVar# mv s of
      (# !_, !a #) -> (# a, Ref# mv #)

-- | This is unsafe, because the ownership of original 'a' is dropped.
unsafeWriteRef# :: Ref# a %1 -> a %1 -> Ref# a
{-# NOINLINE unsafeWriteRef# #-}
unsafeWriteRef# = GHC.noinline $ Unsafe.toLinear2 \(Ref# mv) a ->
  runRW# \s ->
    -- See Note [Stored contents are evaluated after noDuplicate#].
    case GHC.seq# a (GHC.noDuplicate# s) of
      (# s, a #) -> case GHC.writeMutVar# mv a s of
        _ -> Ref# mv

freeRef# :: Ref# a %1 -> a
{-# NOINLINE freeRef# #-}
freeRef# = Unsafe.toLinear \(Ref# a) ->
  runRW# \s ->
    case GHC.readMutVar# a s of
      (# _, !a #) -> a

-- | Read inside 'SystemIO.IO'. Unsafe for the same reason as 'unsafeReadRef#': the ownership of @a@ is duplicated.
unsafeReadRefIO# :: Ref# a -> SystemIO.IO a
{-# INLINE unsafeReadRefIO# #-}
unsafeReadRefIO# (Ref# mv) = SystemIO.IO (GHC.readMutVar# mv)

-- | Write inside 'SystemIO.IO'. Unsafe for the same reason as 'unsafeWriteRef#': the ownership of the original contents is dropped.
unsafeWriteRefIO# :: Ref# a -> a -> SystemIO.IO ()
{-# INLINE unsafeWriteRefIO# #-}
unsafeWriteRefIO# (Ref# mv) a = SystemIO.IO \s -> (# GHC.writeMutVar# mv a s, () #)

instance LinearOnly (Ref# a) where
  linearOnly = UnsafeLinearOnly

atomicModify_# :: (a %1 -> a) %1 -> Ref# a %1 -> Ref# a
{-# NOINLINE atomicModify_# #-}
atomicModify_# = GHC.noinline $ Unsafe.toLinear2 \f (Ref# mv) ->
  runRW# \s ->
    case GHC.atomicModifyMutVar_# mv (Unsafe.toLinear f) (GHC.noDuplicate# s) of
      (# _, !_, !_ #) -> Ref# mv

atomicModify# :: (a %1 -> (b, a)) %1 -> Ref# a %1 -> (# b, Ref# a #)
{-# NOINLINE atomicModify# #-}
atomicModify# = GHC.noinline $ Unsafe.toLinear2 \f (Ref# mv) ->
  runRW# \s ->
    -- 'GHC.atomicModifyMutVar2#' stores the first field of the function's
    -- result, so the new contents must come first.
    case GHC.atomicModifyMutVar2# mv (\x -> case f x of (b, a) -> (a, b)) (GHC.noDuplicate# s) of
      (# _, !_, (!_, !b) #) -> (# b, Ref# mv #)
