{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Vector.Mutable.Linear.BorrowSpec (
  module Data.Vector.Mutable.Linear.BorrowSpec,
) where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias))
import Control.Monad.Borrow.Pure.Copyable
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Bifunctor.Linear qualified as Bi
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Vector qualified as V
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import GHC.IO (unsafePerformIO)
import Prelude.Linear
import Test.Falsify.Generator qualified as G
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Property qualified as F
import Test.Falsify.Range qualified as G
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

data MoveTracked = MoveTracked !(IORef Int) !Int !Bool

instance Consumable MoveTracked where
  consume = Unsafe.toLinear \_ -> ()

instance Dupable MoveTracked where
  dup2 = Unsafe.toLinear \value -> (value, value)

instance Movable MoveTracked where
  move =
    Unsafe.toLinear \(MoveTracked moves value _) ->
      case unsafePerformIO (modifyIORef' moves NonLinear.succ) of
        () -> Ur (MoveTracked moves value True)

data CopyTracked = CopyTracked !(IORef Int) !(IORef Int) !Int

instance Copyable CopyTracked where
  copy =
    Unsafe.toLinear \(UnsafeAlias value@(CopyTracked copies retired _)) ->
      case unsafePerformIO do
        retirementCount <- readIORef retired
        if retirementCount == 0
          then modifyIORef' copies NonLinear.succ
          else NonLinear.error "copy invoked after source retirement" of
        () -> value

instance Consumable CopyTracked where
  consume =
    Unsafe.toLinear \(CopyTracked _ retired _) ->
      unsafePerformIO (modifyIORef' retired NonLinear.succ)

instance Dupable CopyTracked where
  dup2 = Unsafe.toLinear \value -> (value, value)

instance Movable CopyTracked where
  move =
    Unsafe.toLinear \value@(CopyTracked _ retired _) ->
      case unsafePerformIO (modifyIORef' retired NonLinear.succ) of
        () -> Ur value

materializeMoveTracked :: IORef Int -> [(Int, Bool)]
materializeMoveTracked moves =
  linearly \linear ->
    case VL.toVector
      ( VL.fromVector
          (V.fromList [MoveTracked moves 10 False, MoveTracked moves 20 False])
          linear
      ) of
      Ur vector ->
        NonLinear.map
          (\(MoveTracked _ value wasMoved) -> (value, wasMoved))
          (V.toList vector)

discardMaterializedMoveTracked :: IORef Int -> ()
discardMaterializedMoveTracked moves =
  linearly \linear ->
    case VL.toVector
      ( VL.fromVector
          (V.fromList [MoveTracked moves 10 False, MoveTracked moves 20 False])
          linear
      ) of
      Ur _ -> ()

test_materialization :: TestTree
test_materialization =
  testGroup
    "materialization"
    [ testCase "invokes move for every owned element" do
        moves <- newIORef 0
        materializeMoveTracked moves @?= [(10, True), (20, True)]
        moveCount <- readIORef moves
        moveCount @?= 2
    , testCase "completes moves even when the result is discarded" do
        moves <- newIORef 0
        _ <- Exception.evaluate (discardMaterializedMoveTracked moves)
        moveCount <- readIORef moves
        moveCount @?= 2
    ]

copyAtMutValue :: Int -> [Int] -> (Int, [Int])
copyAtMutValue i xs = linearly \lin -> DataFlow.do
  (lin, lin') <- dup lin
  vec <- VL.fromList xs lin
  runBO lin' Control.do
    (mvec, lend) <- borrowM vec
    (Ur x, mvec) <- VL.copyAtMut i mvec
    let !() = consume mvec
    pureAfter (x, unur $ VL.toList (reclaim lend))

retireCopiedResult ::
  (Ur CopyTracked, Mut α (VL.Vector CopyTracked)) %1 ->
  VL.Vector CopyTracked %1 ->
  Int
retireCopiedResult =
  Unsafe.toLinear2 \(copiedResult, borrowed) owner ->
    consume borrowed `lseq`
      case VL.toVector owner of
        Ur frozen ->
          case V.length frozen of
            !_ ->
              case copiedResult of
                Ur (CopyTracked _ _ value) -> value

copyAtMutAfterRetirement :: IORef Int -> IORef Int -> Int
copyAtMutAfterRetirement copies retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (VL.fromList [CopyTracked copies retired 10] ownerLinear)
      copiedResult <- VL.copyAtMut 0 vector
      pureAfter (retireCopiedResult copiedResult (reclaim lend))

assertCopyAtMutBoundsError :: Int -> Assertion
assertCopyAtMutBoundsError i = do
  result <- Exception.try @Exception.ErrorCall $ Exception.evaluate $ copyAtMutValue i [10, 20, 30]
  case result of
    Left exception ->
      assertBool
        ("unexpected error: " <> Exception.displayException exception)
        (("get: index " <> show i <> " out of bound: 3") `List.isPrefixOf` Exception.displayException exception)
    Right value -> assertFailure ("expected bounds error, got " <> show value)

test_copyAtMut :: TestTree
test_copyAtMut =
  testGroup
    "copyAtMut"
    [ testCase "copies the selected element and preserves the vector" do
        copyAtMutValue 1 [10, 20, 30] @?= (20, [10, 20, 30])
    , testCase "completes copying before mutable recovery" do
        copies <- newIORef 0
        retired <- newIORef 0
        copyAtMutAfterRetirement copies retired @?= 10
        copyCount <- readIORef copies
        copyCount @?= 1
        retirementCount <- readIORef retired
        retirementCount @?= 1
    , testCase "rejects a negative index" do
        assertCopyAtMutBoundsError (-1)
    , testCase "rejects an index at the upper bound" do
        assertCopyAtMutBoundsError 3
    ]

qsortVec :: (Ord a, Copyable a, Movable a) => V.Vector a -> V.Vector a
qsortVec v = unur $ linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  runBO l1 Control.do
    (v, lend) <- borrowM (VL.fromVector v l2)
    VL.qsort 8 v
    pureAfter $ VL.toVector (reclaim lend)

divideList :: [Int] -> (Int, [Int])
divideList [] = (0, [])
divideList xs =
  let v0 = (V.fromList xs)
      pivot = v0 V.! (V.length v0 `quot` 2)
   in Bi.second unur $ linearly \lin -> DataFlow.do
        (l1, l2) <- dup lin
        runBO l1 Control.do
          (v, lend) <- borrowM (VL.fromList xs l2)
          VL.size v & \(Ur len, v) -> Control.do
            (lo, hi) <- VL.divide pivot v 0 len
            VL.size lo & \(Ur n, lo) -> DataFlow.do
              consume lo
              consume hi
              pureAfter (n, VL.toList $ reclaim lend)

test_divideList :: TestTree
test_divideList =
  testGroup
    "divideList"
    [ testCase "empty" do
        divideList [] @?= (0, [])
    , testProperty "singleton" do
        x <- F.gen $ G.int $ G.between (-100, 100)
        F.assert $
          P.expect (0, [x])
            P..$ ("answer", divideList [x])
    , testProperty "non-empty" do
        xs <- F.gen $ G.list (G.between (1, 100)) $ G.int $ G.between (0, 100)
        let v = V.fromList xs
            pivot = v V.! (V.length v `quot` 2)
            (off, vs) = divideList xs
            (lo, hi) = V.splitAt off $ V.fromList vs

        F.collect "length" [ceiling @_ @Int (fromIntegral @_ @Double (V.length v) / 10) * 10]
        F.collect "min" [NonLinear.minimum v `quot` 10 * 10]
        F.collect "max" [NonLinear.maximum v `quot` 10 * 10]
        F.info $ "pivot: " <> show pivot
        F.assert $
          P.satisfies ("lo <= " <> show pivot, V.all (NonLinear.<= pivot))
            P..$ ("lo", lo)
        F.assert $
          P.satisfies ("hi >= " <> show pivot, V.all (NonLinear.>= pivot))
            P..$ ("hi", hi)
    ]

test_qsort :: TestTree
test_qsort =
  testGroup
    "qsort"
    [ testCase "empty" do
        qsortVec (V.empty @Int) @?= V.empty
    , testProperty "coincides with Data.List.sort on Ints" do
        xs <- F.gen $ G.list (G.between (1, 100)) $ G.int $ G.between (-100, 100)
        let v = V.fromList xs
            sorted = qsortVec v
        F.collect "length" [ceiling @_ @Int (fromIntegral @_ @Double (V.length v) / 10) * 10]
        F.collect "min" [NonLinear.minimum v `quot` 10 * 10]
        F.collect "max" [NonLinear.maximum v `quot` 10 * 10]
        F.collect "sorted" [V.and $ V.zipWith (NonLinear.<=) v (V.tail v)]
        F.info $ "input: " <> show xs
        F.assert $
          P.expect (V.fromList $ List.sort xs)
            P..$ ("output", sorted)
    ]

example1 :: (Int, [Int])
example1 = linearly \lin -> DataFlow.do
  (lin, lin') <- dup lin
  vec <- VL.fromList [0, 1, 2] lin
  runBO lin' Control.do
    (mvec, lend) <- borrowM vec
    mvec <- VL.modify 0 (+ 3) mvec
    mvec <- VL.modify 2 (+ 5) mvec
    mvec <- VL.modify 0 (* 4) mvec
    let !(Ur svec) = share mvec
    Ur n <- VL.copyAt 0 svec
    pureAfter $ (n, unur $ VL.toList (reclaim lend))

test_example1 :: TestTree
test_example1 =
  testCase "example1" do
    example1 @?= (12, [12, 1, 7])

example2 :: (Int, [Int])
example2 = linearly \lin -> DataFlow.do
  (lin, lin') <- dup lin
  vec <- VL.fromList [0, 1, 2] lin
  runBO lin' Control.do
    (mvec, lend) <- borrowM vec
    let !(mvec1, mvec2) = VL.splitAt 1 mvec
    (mvec, ()) <-
      parBO
        ( Control.do
            mvec1 <- VL.modify 0 (+ 3) mvec1
            VL.modify 0 (* 4) mvec1
        )
        (consume Control.<$> VL.modify 1 (+ 5) mvec2)
    let !(Ur svec) = share mvec
    Ur n <- VL.copyAt 0 svec
    pureAfter $ (n, unur $ VL.toList (reclaim lend))

test_example2 :: TestTree
test_example2 =
  testCase "example2" do
    example2 @?= (12, [12, 1, 7])

example3 :: (Int, [Int])
example3 = linearly \lin -> DataFlow.do
  (lin, lin') <- dup lin
  vec <- VL.fromList [0, 1, 2] lin
  runBO lin' Control.do
    (mvec, lend) <- borrowM vec
    mvec <- reborrowing_ mvec \mvec -> Control.do
      let !(mvec1, mvec2) = VL.splitAt 1 mvec
      consume
        Control.<$> parBO
          ( Control.do
              mvec1 <- VL.modify 0 (+ 3) mvec1
              VL.modify 0 (* 4) mvec1
          )
          (VL.modify 1 (+ 5) mvec2)
    let !(Ur svec) = share mvec
    Ur n <- VL.copyAt 0 svec
    pureAfter $ (n, unur $ VL.toList (reclaim lend))

test_example3 :: TestTree
test_example3 =
  testCase "example3" do
    example3 @?= (12, [12, 1, 7])

discardingScopes :: (Int, [Int])
discardingScopes = linearly \lin -> DataFlow.do
  (lin, lin') <- dup lin
  vec <- VL.fromList [10, 20, 30] lin
  runBO lin' Control.do
    (mvec, lend) <- borrowM vec
    mvec <- reborrowing_ mvec \mvec -> Control.do
      mvec <- sharing_ mvec \shared ->
        consume Control.<$> parBO (VL.copyAt 0 shared) (VL.copyAt 1 shared)
      mvec <- VL.modify 0 (+ 1) mvec
      Control.pure $ consume mvec
    mvec <- VL.modify 2 (+ 2) mvec
    let !(Ur svec) = share mvec
    Ur n <- VL.copyAt 0 svec
    pureAfter $ (n, unur $ VL.toList (reclaim lend))

test_discardingScopes :: TestTree
test_discardingScopes =
  testCase "result-discarding scopes restore the outer mutable borrow" do
    discardingScopes @?= (11, [11, 20, 32])
