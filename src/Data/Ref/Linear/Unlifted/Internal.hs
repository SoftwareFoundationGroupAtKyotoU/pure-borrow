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

newRef# :: a %1 -> Linearly %1 -> Ref# a
{-# NOINLINE newRef# #-}
newRef# = GHC.noinline $ Unsafe.toLinear $ \a lin ->
  lin
    `lseq#` GHC.runRW# \s ->
      case GHC.newMutVar# a (GHC.noDuplicate# s) of
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
unsafeWriteRef# = GHC.noinline $ Unsafe.toLinear2 \(Ref# mv) !a ->
  runRW# \s ->
    case GHC.writeMutVar# mv a (GHC.noDuplicate# s) of
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
