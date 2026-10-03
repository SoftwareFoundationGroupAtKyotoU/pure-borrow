{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Kernels that clone one shared linear-base 'Array' in a loop, through the library's 'Clone' instance, whose code the documentation of 'Clone' gives as the example of a hand-written instance.

The tests of "Control.Monad.Borrow.Pure.CloneSpec" that run them check that the clones of a loop stay apart.
'loopArrayClones' and 'twoArrayClonesFromLoop' give wrong results when the copy function is @NOINLINE@ but not applied through @noinline@, since GHC then hands one array to several clones, which is why the documentation requires both.
Both also give wrong results when the copy keeps the first result of 'dup2' without asking which one is the original, and 'twoArrayClonesFromLoop' does when the clone is the original array itself.
'twoRefOfArrayClonesFromLoop' clones a reference to the array, which 0.1.0.0 cloned with one pure 'dup2' for all the clones of a loop.
The kernels see the missing @noinline@ only while this suite sees the library's unfoldings, which is why its @-O0@ modules pass @-fno-ignore-interface-pragmas@.
-}
module Control.Monad.Borrow.Pure.CloneSpec.ArrayLoops (
  loopArrayClones,
  twoArrayClonesFromLoop,
  twoRefOfArrayClonesFromLoop,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Data.Array.Mutable.Linear (Array)
import Data.Array.Mutable.Linear qualified as LA
import Data.Ref.Linear qualified as Ref
import Prelude.Linear

-- | Read the element of a clone, then overwrite it with @k@.
readThenWrite :: Int -> Array Int %1 -> Ur Int
readThenWrite k arr = case LA.get 0 arr of
  (Ur found, arr') -> LA.set 0 k arr' `lseq` Ur found

-- | Clone one shared array of @[0]@ in each of @n@ iterations, reading and then overwriting each clone's element; every read must see 0.
loopArrayClones :: Int -> [Int]
{-# NOINLINE loopArrayClones #-}
loopArrayClones n = unur do
  LA.alloc 1 (0 :: Int) \arr -> move do
    linearly \lin -> runBO lin Control.do
      (borrowed, lend) <- borrowM arr
      Ur shared <- Control.pure (share borrowed)
      let go k
            | k > n = Control.pure []
            | otherwise = Control.do
                cloned <- clone shared
                Ur found <- Control.pure (readThenWrite k cloned)
                rest <- go (k + 1)
                Control.pure (found : rest)
      founds <- go 1
      pureAfter (consume (reclaim lend) `lseq` founds)

-- | Collect two clones from a loop over one shared array of @[0]@, write 999 into the first and read both; expected @(999, 0)@.
twoArrayClonesFromLoop :: (Int, Int)
{-# NOINLINE twoArrayClonesFromLoop #-}
twoArrayClonesFromLoop = unur do
  LA.alloc 1 (0 :: Int) \arr -> move do
    linearly \lin -> runBO lin Control.do
      (borrowed, lend) <- borrowM arr
      Ur shared <- Control.pure (share borrowed)
      let go k
            | k > (2 :: Int) = Control.pure []
            | otherwise = Control.do
                cloned <- clone shared
                rest <- go (k + 1)
                Control.pure (cloned : rest)
      clones <- go 1
      pureAfter (consume (reclaim lend) `lseq` firstTwo clones)

-- | 'twoArrayClonesFromLoop' through a reference to the array, whose 'Clone' clones its contents with the array's; expected @(999, 0)@.
twoRefOfArrayClonesFromLoop :: (Int, Int)
{-# NOINLINE twoRefOfArrayClonesFromLoop #-}
twoRefOfArrayClonesFromLoop = unur do
  LA.alloc 1 (0 :: Int) \arr -> move do
    linearly \lin -> runBO lin Control.do
      ref <- asksLinearly (Ref.new arr)
      (borrowed, lend) <- borrowM ref
      Ur shared <- Control.pure (share borrowed)
      let go k
            | k > (2 :: Int) = Control.pure []
            | otherwise = Control.do
                cloned <- clone shared
                rest <- go (k + 1)
                Control.pure (Ref.free cloned : rest)
      clones <- go 1
      pureAfter (consume (Ref.free (reclaim lend)) `lseq` firstTwo clones)

-- | Write 999 into the first of two arrays and read both.
firstTwo :: [Array Int] %1 -> (Int, Int)
firstTwo [first, second] = case LA.get 0 (LA.set 0 999 first) of
  (Ur x, first') -> case LA.get 0 second of
    (Ur y, second') -> first' `lseq` second' `lseq` (x, y)
firstTwo others = consume others `lseq` (-1, -1)
