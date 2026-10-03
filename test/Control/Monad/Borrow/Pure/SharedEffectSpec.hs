{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | A pure value that performs an effect, stored unevaluated behind a 'Share' and forced by both branches of a 'parBO', performs it once.

See Note [Pure Ref primitives run their effects at most once] in "Data.Ref.Linear.Unlifted.Internal".
The race is not deterministic: a correct library never doubles an effect, and a regression doubles most runs at @-N2@ and above, which is how this suite runs.
-}
module Control.Monad.Borrow.Pure.SharedEffectSpec (
  module Control.Monad.Borrow.Pure.SharedEffectSpec,
) where

import Control.Exception (evaluate)
import Control.Functor.Linear qualified as Control
import Control.Monad (forM)
import Control.Monad.Borrow.Pure
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefB
import Prelude.Linear qualified as PL
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | A pure loop that GHC cannot fold away.
busy :: Int -> Int
{-# NOINLINE busy #-}
busy n = go 0 (abs n)
  where
    go :: Int -> Int -> Int
    go !acc 0 = acc
    go !acc i = go ((acc + i * i) `rem` 1_000_003) (i - 1)

-- | Some pure work before handing the reference on, which only widens the window of the race.
pause :: Int -> Ref Int %1 -> Ref Int
{-# NOINLINE pause #-}
pause work r = case busy work of !_ -> r

spin :: Int -> Ur ()
{-# NOINLINE spin #-}
spin n = case busy n of !_ -> Ur ()

bump :: Mut α (Ref Int) %1 -> BO α ()
bump m = Control.do
  bumped <- RefB.modify (PL.+ 1) m
  Control.pure (PL.consume bumped)

-- | Read the stored reference through the share, linger, and read what it holds.
branch :: Int -> Share β (Ref (Ref Int)) -> BO β (Ur Int)
branch linger sh = Control.do
  Ur inner <- RefB.readShare sh
  Ur () <- Control.pure (spin linger)
  x <- RefB.copyRef inner
  Control.pure (PL.move x)

type Effect = Ref Int %1 -> Linearly %1 -> Ref Int

{- | Store @effect inner@ unevaluated in a reference, read it through a 'Share' in both branches of a 'parBO', and return what each branch read and what is left in the end.

Every component must be @seed + 1@.
-}
probe :: Effect -> Int -> Int -> (Int, Int, Int)
{-# NOINLINE probe #-}
probe effect work seed = linearly \lin -> case dup3 lin of
  (l1, l2, l34) -> case dup l34 of
    (l3, l4) -> case Ref.new seed l1 of
      !inner ->
        let outer = Ref.new (effect (pause work inner) l4) l2
         in case modifyBO outer l3 body of
              (Ur (x, y), outer') -> case Ref.free (Ref.free outer') of
                final -> (x, y, final)
  where
    body :: forall α. Mut α (Ref (Ref Int)) %1 -> BO α (Ur (Int, Int))
    body mut = Control.do
      (r, restored) <- sharing mut \sh -> Control.do
        (Ur x, Ur y) <- parBO (branch (work + 1) sh) (branch (work + 2) sh)
        Control.pure (Ur (x, y))
      Control.pure (restored `PL.lseq` r)

viaAtomicModify :: Effect
viaAtomicModify r l = l `PL.lseq` Ref.atomicModify_ (PL.+ 1) r

viaModifyBO :: Effect
viaModifyBO r l = modifyBO_ r l bump

-- | The offsets from the seed over a number of runs, which must all be one.
offsets :: Effect -> IO [(Int, Int, Int)]
offsets effect = forM [1 .. 100] \i -> do
  (x, y, final) <- evaluate (probe effect 2_000 (i * 10))
  pure (x - i * 10, y - i * 10, final - i * 10)

test_sharedEffect :: TestTree
test_sharedEffect =
  testGroup
    "an effect stored behind a Share runs once when two branches force it"
    [ testCase "Ref.atomicModify_" do
        results <- offsets viaAtomicModify
        filter (/= (1, 1, 1)) results @?= []
    , testCase "modifyBO_" do
        results <- offsets viaModifyBO
        filter (/= (1, 1, 1)) results @?= []
    ]
