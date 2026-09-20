{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.Borrow.Pure.Experimental.LoopSpec (
  module Control.Monad.Borrow.Pure.Experimental.LoopSpec,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Experimental.Loop qualified as Loop
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Functor.Linear qualified as Data
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import Data.Vector qualified as V
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Growable
import Prelude.Linear (Ur (..), consume, dup, lseq, move, unur, ($), (&), (*), (+))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude (Int, String, sum)
import Prelude qualified as NonLinear

{- | Sum a list into a borrowed counter with the linear combinator, moving each element in.

This is the shape a caller with GC-owned elements is pushed into by
'Loop.forReborrowing_': the element binds linearly, so it has to be moved back
out before the body can read it.
-}
sumThroughLinearLoop :: [Int] -> Int
{-# NOINLINE sumThroughLinearLoop #-}
sumThroughLinearLoop values =
  unur $ linearly \linear -> DataFlow.do
    (runLinear, refLinear) <- dup linear
    runBO runLinear Control.do
      (counter, lend) <- borrowM (Ref.new (0 :: Int) refLinear)
      counter <-
        Loop.forReborrowing_ counter values \scoped value ->
          move value & \(Ur value) ->
            consume Data.<$> RefBorrow.modify (+ value) scoped
      counter `lseq` pureAfter (move (Ref.free (reclaim lend)))

-- | The same sum with the unrestricted-element combinator, which needs no 'move'.
sumThroughUnrestrictedLoop :: [Int] -> Int
{-# NOINLINE sumThroughUnrestrictedLoop #-}
sumThroughUnrestrictedLoop values =
  unur $ linearly \linear -> DataFlow.do
    (runLinear, refLinear) <- dup linear
    runBO runLinear Control.do
      (counter, lend) <- borrowM (Ref.new (0 :: Int) refLinear)
      counter <-
        Loop.forReborrowingUr_ counter values \scoped value ->
          consume Data.<$> RefBorrow.modify (+ value) scoped
      counter `lseq` pureAfter (move (Ref.free (reclaim lend)))

{- | The unrestricted combinator over an element no capability class describes.

'String' has no 'Movable' instance requirement here because the element never
binds linearly, which is the whole point: a caller whose elements are already
GC-owned owes the loop nothing.
-}
lengthsThroughUnrestrictedLoop :: [String] -> Int
{-# NOINLINE lengthsThroughUnrestrictedLoop #-}
lengthsThroughUnrestrictedLoop values =
  unur $ linearly \linear -> DataFlow.do
    (runLinear, refLinear) <- dup linear
    runBO runLinear Control.do
      (counter, lend) <- borrowM (Ref.new (0 :: Int) refLinear)
      counter <-
        Loop.forReborrowingUr_ counter values \scoped value ->
          consume Data.<$> RefBorrow.modify (+ NonLinear.length value) scoped
      counter `lseq` pureAfter (move (Ref.free (reclaim lend)))

-- | The indexed unrestricted combinator, weighting each element by its position.
weightedThroughUnrestrictedLoop :: [Int] -> Int
{-# NOINLINE weightedThroughUnrestrictedLoop #-}
weightedThroughUnrestrictedLoop values =
  unur $ linearly \linear -> DataFlow.do
    (runLinear, refLinear) <- dup linear
    runBO runLinear Control.do
      (counter, lend) <- borrowM (Ref.new (0 :: Int) refLinear)
      counter <-
        Loop.iforReborrowingUr_ counter values \scoped index value ->
          consume Data.<$> RefBorrow.modify (+ (index * value)) scoped
      counter `lseq` pureAfter (move (Ref.free (reclaim lend)))

{- | The unrestricted combinator grows a borrowed vector, once per element.

The growable vector is the instrument rather than the subject, as in
"Control.Monad.Borrow.Pure.BOSpec": each iteration writes the header, so a
combinator that handed back the caller's own occurrence would report a stale
length here.
-}
growThroughUnrestrictedLoop :: [Int] -> (Int, [Int])
{-# NOINLINE growThroughUnrestrictedLoop #-}
growThroughUnrestrictedLoop values =
  unur $ linearly \linear -> DataFlow.do
    (runLinear, vectorLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromVector (V.fromList []) vectorLinear)
      vector <-
        Loop.forReborrowingUr_ vector values \scoped value ->
          consume Data.<$> Growable.push value scoped
      Growable.size vector & \(Ur finalSize, vector) ->
        vector `lseq`
          pureAfter
            ( Growable.toVector (reclaim lend) & \(Ur contents) ->
                Ur (finalSize, V.toList contents)
            )

test_unrestrictedElementLoops :: TestTree
test_unrestrictedElementLoops =
  testGroup
    "loops over GC-owned elements"
    [ testGroup
        "forReborrowingUr_ agrees with forReborrowing_ plus move"
        [ testCase (NonLinear.show values) do
            sumThroughUnrestrictedLoop values @?= sumThroughLinearLoop values
        | values <- [[], [7], [1, 2, 3], [10, 20, 30, 40, 50]]
        ]
    , testGroup
        "and with the ordinary sum"
        [ testCase (NonLinear.show values) do
            sumThroughUnrestrictedLoop values @?= sum values
        | values <- [[], [7], [1, 2, 3], NonLinear.enumFromTo 1 64]
        ]
    , testCase "an element with no capability instances at all" do
        lengthsThroughUnrestrictedLoop ["", "ab", "cde"] @?= 5
    , testGroup
        "iforReborrowingUr_ supplies ascending indices from zero"
        [ testCase (NonLinear.show values) do
            weightedThroughUnrestrictedLoop values
              @?= sum (NonLinear.zipWith (NonLinear.*) (NonLinear.enumFrom 0) values)
        | values <- [[], [5], [1, 2, 3], [10, 20, 30, 40]]
        ]
    , testGroup
        "a write through the borrow each iteration restored is visible to the next"
        [ testCase (NonLinear.show values) do
            growThroughUnrestrictedLoop values
              @?= (NonLinear.length values, values)
        | values <- [[], [1], [1, 2, 3], NonLinear.enumFromTo 1 33]
        ]
    ]
