{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Ref.LinearSpec (
  module Data.Ref.LinearSpec,
) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure (linearly, modifyBO)
import Control.Monad.Borrow.Pure.Affine (aff, pop)
import Data.Ref.Linear qualified as Ref
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- 'GHC.Exts.atomicModifyMutVar2#' stores the first field of the function's
-- result; these cases would read back the wrong value, or crash, if the
-- wrappers handed it a function whose result is not a record with the new
-- contents first.

atomicModifyInt :: Int -> Int
{-# NOINLINE atomicModifyInt #-}
atomicModifyInt start =
  linearly \lin -> Ref.free (Ref.atomicModify_ (+ 1) (Ref.new start lin))

atomicModifyPair :: Int -> (Int, Int)
{-# NOINLINE atomicModifyPair #-}
atomicModifyPair start = linearly \lin ->
  case Ref.atomicModify (\x -> (x, 5)) (Ref.new start lin) of
    (old, ref) -> (old, Ref.free ref)

atomicModifyList :: [Int] -> [Int]
{-# NOINLINE atomicModifyList #-}
atomicModifyList start =
  linearly \lin -> Ref.free (Ref.atomicModify_ (7 :) (Ref.new start lin))

test_atomicModify :: TestTree
test_atomicModify =
  testGroup
    "atomicModify stores the function's new value"
    [ testCase "atomicModify_ on an Int" do
        atomicModifyInt 0 @?= 1
    , testCase "atomicModify returns the old value and stores the new one" do
        atomicModifyPair 0 @?= (0, 5)
    , testCase "atomicModify_ on a list" do
        atomicModifyList [8] @?= [7, 8]
    ]

{- | A reference to a value that raises, borrowed, and dropped with 'aff' without anything reading it.

'Ref.new' evaluates what it stores as the reference is evaluated, which borrowing it does, so this raises; see Note [Stored contents are evaluated after noDuplicate#] in "Data.Ref.Linear.Unlifted.Internal".
Freeing the reference would raise however it was stored.
-}
droppedUnread :: ()
{-# NOINLINE droppedUnread #-}
droppedUnread = linearly \lin -> case dup lin of
  (l1, l2) -> case modifyBO (Ref.new (error "a stored placeholder" :: Int) l1) l2 (\mut -> Control.pure (consume mut)) of
    ((), ref) -> pop (aff ref)

test_new :: TestTree
test_new =
  testGroup
    "Ref.new"
    [ testCase "evaluates what it stores before anything reads it" do
        result <- try (evaluate droppedUnread)
        case result of
          Left (ErrorCall message) -> message @?= "a stored placeholder"
          Right () -> assertFailure "the stored value was not evaluated"
    ]
