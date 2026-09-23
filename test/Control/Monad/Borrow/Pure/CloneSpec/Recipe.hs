{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | The hand-written 'Clone' instance that the documentation of 'Clone' gives for linear-base's arrays, verbatim, with two kernels that clone one shared array in a loop.

These tests check that the documented example compiles and keeps the clones of a loop apart.
They do not show that its @noinline@ is needed: GHC shares an unprotected copy among the clones of a loop only in some shapes, and in this module it did not, with the token consumed beside the copy or passed to a function that is only @NOINLINE@.
In other programs of the same kind, those two variants did hand one array to every clone, which is why the documentation requires the copy function to be @NOINLINE@ and applied through @noinline@.
-}
module Control.Monad.Borrow.Pure.CloneSpec.Recipe (loopArrayClones, twoArrayClonesFromLoop) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO (evaluateBO)
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..))
import Data.Array.Mutable.Linear (Array)
import Data.Array.Mutable.Linear qualified as Array
import GHC.Exts (noinline)
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

instance Clone (Array a) where
  clone = Unsafe.toLinear \(UnsafeAlias arr) -> Control.do
    lin <- askLinearly
    evaluateBO (copyArray arr lin)

-- NOINLINE and applied through noinline, so that each copy depends on its own token.
copyArray :: Array a -> Linearly %1 -> Array a
{-# NOINLINE copyArray #-}
copyArray = noinline \arr lin -> lin `lseq` sliceAll arr

sliceAll :: Array a -> Array a
sliceAll arr = case Array.size arr of
  (Ur n, _) -> case Array.slice 0 n arr of
    (_, copied) -> copied

-- | Read the element of a clone, then overwrite it with @k@.
readThenWrite :: Int -> Array Int %1 -> Ur Int
readThenWrite k arr = case Array.get 0 arr of
  (Ur found, arr') -> Array.set 0 k arr' `lseq` Ur found

-- | Clone one shared array of @[0]@ in each of @n@ iterations, reading and then overwriting each clone's element; every read must see 0.
loopClones :: forall t. (Clone t, Consumable t) => (Array Int %1 -> t) -> (t %1 -> Array Int) -> Int -> [Int]
{-# INLINE loopClones #-}
loopClones wrap unwrap n = unur do
  Array.alloc 1 (0 :: Int) \arr -> move do
    linearly \lin -> runBO lin Control.do
      (borrowed, lend) <- borrowM (wrap arr)
      Ur shared <- Control.pure (share borrowed)
      let go k
            | k > n = Control.pure []
            | otherwise = Control.do
                cloned <- clone shared
                Ur found <- Control.pure (readThenWrite k (unwrap cloned))
                rest <- go (k + 1)
                Control.pure (found : rest)
      founds <- go 1
      pureAfter (consume (reclaim lend) `lseq` founds)

-- | Collect two clones from a loop over one shared array of @[0]@, write 999 into the first and read both; expected @(999, 0)@.
twoLive :: forall t. (Clone t, Consumable t) => (Array Int %1 -> t) -> (t %1 -> Array Int) -> (Int, Int)
{-# INLINE twoLive #-}
twoLive wrap unwrap = unur do
  Array.alloc 1 (0 :: Int) \arr -> move do
    linearly \lin -> runBO lin Control.do
      (borrowed, lend) <- borrowM (wrap arr)
      Ur shared <- Control.pure (share borrowed)
      let go k
            | k > (2 :: Int) = Control.pure []
            | otherwise = Control.do
                cloned <- clone shared
                rest <- go (k + 1)
                Control.pure (unwrap cloned : rest)
      clones <- go 1
      pureAfter (consume (reclaim lend) `lseq` firstTwo clones)
  where
    firstTwo :: [Array Int] %1 -> (Int, Int)
    firstTwo [first, second] = case Array.get 0 (Array.set 0 999 first) of
      (Ur x, first') -> case Array.get 0 second of
        (Ur y, second') -> first' `lseq` second' `lseq` (x, y)
    firstTwo others = consume others `lseq` (-1, -1)

loopArrayClones :: Int -> [Int]
{-# NOINLINE loopArrayClones #-}
loopArrayClones = loopClones id id

twoArrayClonesFromLoop :: (Int, Int)
{-# NOINLINE twoArrayClonesFromLoop #-}
twoArrayClonesFromLoop = twoLive id id
