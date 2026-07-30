{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Vector.Mutable.Growable.Linear.BorrowSpec (
  module Data.Vector.Mutable.Growable.Linear.BorrowSpec,
) where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..))
import Control.Monad.Borrow.Pure.Copyable (Copyable (copy), copyMut)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Growable
import Data.Vector.Mutable.Growable.Linear.TypingCases
import Data.Vector.Mutable.Linear.Borrow qualified as Fixed
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

data Operation
  = Push !Int
  | Extend !Int !Int
  | Reserve !Int
  | ReserveAdditional !Int
  | Replace !Int !Int
  deriving (Show)

newtype Tracked = Tracked (IORef Int)

instance Consumable Tracked where
  consume =
    Unsafe.toLinear \(Tracked counter) ->
      unsafePerformIO (modifyIORef' counter NonLinear.succ)

newtype NestedRef = NestedRef (Ref.Ref Int)

instance Consumable NestedRef where
  consume =
    Unsafe.toLinear \(NestedRef ref) ->
      consume ref

asBorrowedRef ::
  Mut α NestedRef %1 ->
  Mut α (Ref.Ref Int)
asBorrowedRef = upcast

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

materializeMoveTracked :: IORef Int -> [(Int, Bool)]
materializeMoveTracked moves =
  NonLinear.map
    (\(MoveTracked _ value wasMoved) -> (value, wasMoved))
    ( V.toList $
        unur $
          linearly \linear -> DataFlow.do
            (ownerLinear, runLinear) <- dup linear
            runBO runLinear Control.do
              (vector, lend) <- borrowM (Growable.empty ownerLinear)
              vector <- Growable.push (MoveTracked moves 10 False) vector
              vector <- Growable.push (MoveTracked moves 20 False) vector
              let !() = consume vector
              pureAfter (Growable.toVector (reclaim lend))
    )

discardMaterializedMoveTracked :: IORef Int -> ()
discardMaterializedMoveTracked moves =
  linearly \linear ->
    case Growable.toVector
      ( Growable.fromVector
          (V.fromList [MoveTracked moves 10 False, MoveTracked moves 20 False])
          linear
      ) of
      Ur _ -> ()

decodeOperation :: Int -> Operation
decodeOperation seed =
  case seed `NonLinear.mod` 5 of
    0 -> Push seed
    1 -> Extend seed (seed + 1)
    2 -> Reserve (NonLinear.abs seed `NonLinear.mod` 32)
    3 -> ReserveAdditional (NonLinear.abs seed `NonLinear.mod` 16)
    _ -> Replace seed (-seed)

applyOperations ::
  [Operation] ->
  Mut α (Growable.GrowableVector Int) %1 ->
  BO α (Mut α (Growable.GrowableVector Int))
applyOperations [] vector = Control.pure vector
applyOperations (operation : operations) vector =
  case operation of
    Push value -> Control.do
      vector <- Growable.push value vector
      applyOperations operations vector
    Extend first second -> Control.do
      vector <- Growable.extend (V.fromList [first, second]) vector
      applyOperations operations vector
    Reserve requested -> Control.do
      vector <- Growable.reserve requested vector
      applyOperations operations vector
    ReserveAdditional additional -> Control.do
      vector <- Growable.reserveAdditional additional vector
      applyOperations operations vector
    Replace rawIndex value ->
      case Growable.size vector of
        (Ur 0, vector) -> applyOperations operations vector
        (Ur logicalSize, vector) -> Control.do
          let !index = NonLinear.abs rawIndex `NonLinear.mod` logicalSize
          (old, vector) <- Growable.set index value vector
          applyOperations operations (consume old `lseq` vector)

applyModel :: [Operation] -> [Int] -> [Int]
applyModel operations initial = NonLinear.foldl step initial operations
  where
    step values = \case
      Push value -> values <> [value]
      Extend first second -> values <> [first, second]
      Reserve _ -> values
      ReserveAdditional _ -> values
      Replace _ _ | NonLinear.null values -> values
      Replace rawIndex value ->
        let !index = NonLinear.abs rawIndex `NonLinear.mod` NonLinear.length values
         in case NonLinear.splitAt index values of
              (prefix, _ : suffix) -> prefix <> (value : suffix)
              (_, []) -> values

freezeList :: Growable.GrowableVector Int %1 -> [Int]
freezeList vector =
  case Growable.toVector vector of
    Ur frozen -> V.toList frozen

freezeLength :: Growable.GrowableVector Int %1 -> Int
freezeLength vector =
  case Growable.toVector vector of
    Ur frozen -> V.length frozen

freezeListUr :: Growable.GrowableVector Int %1 -> Ur [Int]
freezeListUr vector =
  case Growable.toVector vector of
    Ur frozen -> Ur (V.toList frozen)

runOperations :: Int -> [Operation] -> ([Int], Int, Int)
runOperations initialCapacity operations =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.withCapacity initialCapacity ownerLinear)
      vector <- applyOperations operations vector
      Growable.size vector & \(Ur logicalSize, vector) ->
        Growable.capacity vector & \(Ur finalCapacity, vector) -> DataFlow.do
          consume vector
          pureAfter $
            case Growable.toVector (reclaim lend) of
              Ur frozen ->
                Ur (V.toList frozen, logicalSize, finalCapacity)

test_model :: TestTree
test_model =
  testProperty "matches a list model across reserve, push, extend, and replace" do
    initialCapacity <- F.gen $ G.int $ G.between (0, 16)
    seeds <- F.gen $ G.list (G.between (0, 100)) $ G.int $ G.between (-100, 100)
    let !operations = NonLinear.map decodeOperation seeds
        !(actual, logicalSize, finalCapacity) =
          runOperations initialCapacity operations
        !expected = applyModel operations []
    F.collect "operations" [NonLinear.length operations `NonLinear.quot` 10 * 10]
    F.assert $
      P.expect expected P..$ ("contents", actual)
    F.assert $
      P.expect (NonLinear.length expected) P..$ ("logical size", logicalSize)
    F.assert $
      P.satisfies
        ("capacity >= logical size", (NonLinear.>= logicalSize))
        P..$ ("capacity", finalCapacity)

contentRoundTrip :: ((Int, Int), [Int])
contentRoundTrip =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList [10, 20, 30]) ownerLinear)
      ((logicalSize, first), vector) <-
        Growable.withContent vector \contents -> Control.do
          Fixed.size contents & \(Ur logicalSize, contents) -> Control.do
            (Ur first, contents) <- Fixed.copyAtMut 0 contents
            contents <- Fixed.modify 1 (+ 1) contents
            Control.pure (consume contents `lseq` (logicalSize, first))
      vector <- Growable.push 40 vector
      let !() = consume vector
      pureAfter $
        ( (logicalSize, first)
        , freezeList (reclaim lend)
        )

test_withContent :: TestTree
test_withContent =
  testCase "opens only the initialized prefix and restores growth access" do
    contentRoundTrip @?= ((3, 10), [10, 21, 30, 40])

directProjection :: (Int, [Int])
directProjection =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.withCapacity 8 ownerLinear)
      vector <- Growable.extend (V.fromList [3, 4, 5]) vector
      Fixed.size (Growable.getContents vector) & \(Ur logicalSize, contents) -> Control.do
        (Ur value, contents) <- Fixed.copyAtMut 2 contents
        let !() = consume contents
        pureAfter $
          ( logicalSize + value
          , freezeList (reclaim lend)
          )

test_getContents :: TestTree
test_getContents =
  testCase "direct projection captures logical length, not spare capacity" do
    directProjection @?= (8, [3, 4, 5])

discardingContentScope :: [Int]
discardingContentScope =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList [1, 2, 3]) ownerLinear)
      vector <- Growable.withContent_ vector \contents -> Control.do
        contents <- Fixed.modify 0 (+ 10) contents
        Control.pure (consume contents)
      vector <- Growable.push 4 vector
      let !() = consume vector
      pureAfter $ freezeListUr (reclaim lend)

countedContentScope :: IORef Int -> [Int]
countedContentScope counter =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList [1, 2, 3]) ownerLinear)
      vector <- Growable.withContent_ vector \contents ->
        case unsafePerformIO (modifyIORef' counter NonLinear.succ) of
          () -> Control.pure (consume contents)
      vector <- Growable.push 4 vector
      let !() = consume vector
      pureAfter $ freezeListUr (reclaim lend)

test_withContent_ :: TestTree
test_withContent_ =
  testGroup
    "discarding content scopes"
    [ testCase "restores the growable borrow" do
        discardingContentScope @?= [11, 2, 3, 4]
    , testCase "runs the callback exactly once" do
        counter <- newIORef 0
        countedContentScope counter @?= [1, 2, 3, 4]
        count <- readIORef counter
        count @?= 1
    ]

sharedContentProjection :: ((Int, Int, Int), [Int])
sharedContentProjection =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList [5, 6, 7]) ownerLinear)
      share vector & \(Ur sharedVector) -> Control.do
        ((first, second), returnedSharedVector) <-
          Growable.withContent sharedVector \linearContents ->
            move linearContents & \(Ur contents) -> Control.do
              Ur first <- Fixed.copyAt 0 contents
              Ur second <- Fixed.copyAt 1 contents
              Control.pure (first, second)
        move returnedSharedVector & \(Ur sharedVector) -> Control.do
          Ur third <- Fixed.copyAt 2 (Growable.getContents sharedVector)
          let !() = consume sharedVector
          pureAfter
            ( (first, second, third)
            , freezeList (reclaim lend)
            )

test_sharedContentProjection :: TestTree
test_sharedContentProjection =
  testCase "preserves Share and permits repeated reads in a content scope" do
    sharedContentProjection @?= ((5, 6, 7), [5, 6, 7])

sharedDiscardingContentScope :: (Int, [Int])
sharedDiscardingContentScope =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList [8, 9, 10]) ownerLinear)
      share vector & \(Ur sharedVector) -> Control.do
        returnedSharedVector <-
          Growable.withContent_ sharedVector \linearContents ->
            move linearContents & \(Ur contents) -> Control.do
              Ur first <- Fixed.copyAt 0 contents
              Ur second <- Fixed.copyAt 1 contents
              Control.pure (first + second)
        move returnedSharedVector & \(Ur sharedVector) -> Control.do
          Ur last <- Fixed.copyAt 2 (Growable.getContents sharedVector)
          pureAfter
            ( last
            , freezeList (reclaim lend)
            )

test_sharedDiscardingContentScope :: TestTree
test_sharedDiscardingContentScope =
  testCase "withContent_ preserves a shared growable borrow" do
    sharedDiscardingContentScope @?= (10, [8, 9, 10])

replacement :: (Int, [Int])
replacement =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList [7, 8, 9]) ownerLinear)
      (old, vector) <- Growable.set 1 80 vector
      let !() = consume vector
      pureAfter
        ( old
        , freezeList (reclaim lend)
        )

test_set :: TestTree
test_set =
  testCase "returns the displaced value without changing logical size" do
    replacement @?= (8, [7, 80, 9])

mirroredSurface :: ((Int, Int, Int), [Int])
mirroredSurface =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [1, 2, 3] ownerLinear)
      (Ur middle, vector) <-
        reborrowing vector \short -> Control.do
          element <- Growable.get 1 short
          Control.pure (copyMut element)
      (Ur first, vector) <-
        reborrowing vector \short -> Control.do
          element <- Growable.head short
          Control.pure (copyMut element)
      (Ur final, vector) <-
        reborrowing vector \short -> Control.do
          element <- Growable.last short
          Control.pure (copyMut element)
      ((), vector) <-
        Growable.update
          1
          (\ !value -> Control.pure ((), value + 10))
          vector
      vector <- Growable.modify 0 (* 2) vector
      vector <- Growable.swap vector 0 2
      let !() = consume vector
      pureAfter
        ( (middle, first, final)
        , freezeList (reclaim lend)
        )

test_mirroredSurface :: TestTree
test_mirroredSurface =
  testCase "mirrors fixed-vector get, head, last, update, modify, and swap" do
    mirroredSurface @?= ((2, 1, 3), [3, 12, 2])

test_constructionSurface :: TestTree
test_constructionSurface =
  testGroup
    "construction"
    [ testCase "constant initializes the complete logical prefix" do
        linearly
          (\linear -> unur $ Growable.toList (Growable.constant 3 (7 :: Int) linear))
          @?= [7, 7, 7]
    , testCase "fromList and toList round-trip" do
        linearly
          (\linear -> unur $ Growable.toList (Growable.fromList [4, 5, 6 :: Int] linear))
          @?= [4, 5, 6]
    , testCase "unsafeFromMutable takes the complete initialized source" do
        linearly
          ( \linear ->
              unur $
                Growable.toList
                  ( Growable.unsafeFromMutable
                      (unsafePerformIO (MV.replicate 2 (3 :: Int)))
                      linear
                  )
          )
          @?= [3, 3]
    , testCase "unsafeFromVector takes the complete initialized source" do
        linearly
          ( \linear ->
              unur $
                Growable.toList
                  (Growable.unsafeFromVector (V.fromList [2, 4 :: Int]) linear)
          )
          @?= [2, 4]
    ]

assertErrorPrefix :: NonLinear.String -> a -> Assertion
assertErrorPrefix expectedPrefix value = do
  result <- Exception.try @Exception.ErrorCall $ Exception.evaluate value
  case result of
    Left exception ->
      assertBool
        ("unexpected error: " <> Exception.displayException exception)
        (expectedPrefix `List.isPrefixOf` Exception.displayException exception)
    Right _ -> assertFailure ("expected error beginning with " <> expectedPrefix)

copyOutOfBounds :: Int -> Int
copyOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList [10, 20, 30]) ownerLinear)
      (Ur value, vector) <- Growable.copyAtMut index vector
      let !() = consume vector
      pureAfter (value + freezeLength (reclaim lend))

setOutOfBounds :: Int -> Int
setOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (V.fromList [10, 20, 30]) ownerLinear)
      (old, vector) <- Growable.set index 0 vector
      let !() = consume vector
      pureAfter (old + freezeLength (reclaim lend))

emptyHead :: Int
emptyHead =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.empty ownerLinear)
      element <- Growable.head vector
      let !(Ur value) = copyMut element
      pureAfter (value + freezeLength (reclaim lend))

emptyLast :: Int
emptyLast =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.empty ownerLinear)
      element <- Growable.last vector
      let !(Ur value) = copyMut element
      pureAfter (value + freezeLength (reclaim lend))

test_bounds :: TestTree
test_bounds =
  testGroup
    "bounds"
    [ testCase "copy rejects a negative index" do
        assertErrorPrefix
          "copyAtMut: index -1 out of bounds for length 3"
          (copyOutOfBounds (-1))
    , testCase "copy rejects the upper bound" do
        assertErrorPrefix
          "copyAtMut: index 3 out of bounds for length 3"
          (copyOutOfBounds 3)
    , testCase "set rejects a negative index" do
        assertErrorPrefix
          "set: index -1 out of bounds for length 3"
          (setOutOfBounds (-1))
    , testCase "set rejects the upper bound" do
        assertErrorPrefix
          "set: index 3 out of bounds for length 3"
          (setOutOfBounds 3)
    , testCase "head rejects an empty vector" do
        assertErrorPrefix
          "get: index 0 out of bounds for length 0"
          emptyHead
    , testCase "last rejects an empty vector" do
        assertErrorPrefix
          "last: empty vector"
          emptyLast
    , testCase "construction rejects negative capacity" do
        assertErrorPrefix
          "withCapacity: negative capacity -1"
          (linearly \linear -> consume (Growable.withCapacity @Int (-1) linear))
    ]

test_capacity :: TestTree
test_capacity =
  testGroup
    "capacity"
    [ testCase "empty starts with zero logical size" do
        runOperations 0 [] @?= ([], 0, 0)
    , testCase "preallocation does not initialize spare capacity" do
        runOperations 8 [] @?= ([], 0, 8)
    , testCase "reserve preserves contents and does not change size" do
        let !(values, logicalSize, finalCapacity) =
              runOperations 1 [Push 1, Push 2, Reserve 12]
        values @?= [1, 2]
        logicalSize @?= 2
        assertBool "reserve did not reserve enough" (finalCapacity >= 12)
    ]

trackedConsumption :: IORef Int -> ()
trackedConsumption counter =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.withCapacity 0 ownerLinear)
      vector <- Growable.push (Tracked counter) vector
      vector <- Growable.push (Tracked counter) vector
      vector <- Growable.push (Tracked counter) vector
      let !() = consume vector
      pureAfter (consume (reclaim lend))

boxedRefAcrossGrowth :: Int
boxedRefAcrossGrowth =
  linearly \linear -> DataFlow.do
    (refLinear, remainingLinear) <- dup linear
    (ownerLinear, runLinear) <- dup remainingLinear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.empty ownerLinear)
      vector <- Growable.push (NestedRef (Ref.new 1 refLinear)) vector
      vector <- Growable.reserve 64 vector
      ((), vector) <-
        reborrowing vector \short -> Control.do
          element <- Growable.get 0 short
          ref <- RefBorrow.modify (+ 41) (asBorrowedRef element)
          Control.pure (consume ref)
      (observed, vector) <-
        reborrowing vector \short -> Control.do
          element <- Growable.get 0 short
          RefBorrow.copyRef (asBorrowedRef element)
      let !() = consume vector
      pureAfter (consume (reclaim lend) `lseq` observed)

immutableCopyLifecycle :: IORef Int -> IORef Int -> ()
immutableCopyLifecycle copies retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromVector
              ( V.fromList
                  [ CopyTracked copies retired 10
                  , CopyTracked copies retired 20
                  ]
              )
              ownerLinear
          )
      vector <- Growable.reserve 64 vector
      vector <-
        Growable.extend
          (V.singleton (CopyTracked copies retired 30))
          vector
      vector <- Growable.reserveAdditional 64 vector
      let !() = consume vector
      pureAfter (consume (reclaim lend))

gcOwnedImmutableLifecycle :: IORef Int -> ()
gcOwnedImmutableLifecycle retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromVector
              (V.singleton (Tracked retired))
              ownerLinear
          )
      vector <-
        Growable.extend
          (V.fromList [Tracked retired, Tracked retired])
          vector
      let !() = consume vector
      pureAfter (consume (reclaim lend))

test_consumable :: TestTree
test_consumable =
  testGroup
    "destructive growth"
    [ testCase "growth moves capabilities and final consumption retires each once" do
        counter <- newIORef 0
        _ <- Exception.evaluate (trackedConsumption counter)
        count <- readIORef counter
        count @?= 3
    , testCase "preserves a nested Ref identity across reallocation" do
        boxedRefAcrossGrowth @?= 42
    , testCase "ordinary constructors need no Copyable instance" do
        counter <- newIORef 0
        _ <-
          Exception.evaluate $
            linearly \linear ->
              dup linear & \(constantLinear, listLinear) ->
                consume
                  (Growable.constant 2 (Tracked counter) constantLinear)
                  `lseq` consume
                    (Growable.fromList [Tracked counter] listLinear)
        retired <- readIORef counter
        retired @?= 3
    , testCase "ordinary immutable sources need no Copyable instance" do
        counter <- newIORef 0
        _ <- Exception.evaluate (gcOwnedImmutableLifecycle counter)
        retired <- readIORef counter
        retired @?= 3
    , testCase "ordinary immutable copies do not invoke Copyable" do
        copies <- newIORef 0
        retired <- newIORef 0
        _ <- Exception.evaluate (immutableCopyLifecycle copies retired)
        copyCount <- readIORef copies
        retiredCount <- readIORef retired
        copyCount @?= 0
        retiredCount @?= 3
    , testCase "materialization invokes move for every owned element" do
        moves <- newIORef 0
        materializeMoveTracked moves @?= [(10, True), (20, True)]
        moveCount <- readIORef moves
        moveCount @?= 2
    , testCase "discarding materialization still invokes every move" do
        moves <- newIORef 0
        _ <- Exception.evaluate (discardMaterializedMoveTracked moves)
        moveCount <- readIORef moves
        moveCount @?= 2
    ]

sharedCopy :: (Int, [Int])
sharedCopy =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [4, 5, 6] ownerLinear)
      let !(Ur shared) = share vector
      Ur value <- Growable.copyAt 1 shared
      pureAfter (value, freezeList (reclaim lend))

retireCopiedResult ::
  (Ur CopyTracked, Mut α (Growable.GrowableVector CopyTracked)) %1 ->
  Growable.GrowableVector CopyTracked %1 ->
  Int
retireCopiedResult =
  Unsafe.toLinear2 \(copiedResult, borrowed) owner ->
    consume borrowed `lseq`
      consume owner `lseq`
        case copiedResult of
          Ur (CopyTracked _ _ value) -> value

retireSharedCopiedResult ::
  Ur CopyTracked %1 ->
  Growable.GrowableVector CopyTracked %1 ->
  Int
retireSharedCopiedResult =
  Unsafe.toLinear2 \copiedResult owner ->
    consume owner `lseq`
      case copiedResult of
        Ur (CopyTracked _ _ value) -> value

copyAtAfterRetirement :: IORef Int -> IORef Int -> Int
copyAtAfterRetirement copies retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromList
              [CopyTracked copies retired 10]
              ownerLinear
          )
      let !(Ur shared) = share vector
      copiedResult <- Growable.copyAt 0 shared
      pureAfter (retireSharedCopiedResult copiedResult (reclaim lend))

unsafeCopyAtAfterRetirement :: IORef Int -> IORef Int -> Int
unsafeCopyAtAfterRetirement copies retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromList
              [CopyTracked copies retired 10]
              ownerLinear
          )
      let !(Ur shared) = share vector
      copiedResult <- Growable.unsafeCopyAt 0 shared
      pureAfter (retireSharedCopiedResult copiedResult (reclaim lend))

copyAtMutAfterRetirement :: IORef Int -> IORef Int -> Int
copyAtMutAfterRetirement copies retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromList
              [CopyTracked copies retired 10]
              ownerLinear
          )
      copiedResult <- Growable.copyAtMut 0 vector
      pureAfter (retireCopiedResult copiedResult (reclaim lend))

selectedValues :: ((Int, Int), [Int])
selectedValues =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [4, 5, 6] ownerLinear)
      selected <- Growable.indicesMut vector [0, 2]
      let !(Ur firstValue, Ur finalValue) = copySelected selected
      pureAfter
        ((firstValue, finalValue), freezeList (reclaim lend))

copySelected :: [Mut α Int] %1 -> (Ur Int, Ur Int)
copySelected =
  Unsafe.toLinear \case
    [first, final] -> (copyMut first, copyMut final)
    selected -> error "indicesMut returned the wrong number of borrows" selected

duplicateIndices :: Int
duplicateIndices =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [4, 5, 6] ownerLinear)
      selected <- Growable.indicesMut vector [0, 0]
      let !() = consume selected
      pureAfter (freezeLength (reclaim lend))

parallelSplitContent :: [Int]
parallelSplitContent =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [1, 2, 3, 4] ownerLinear)
      vector <- Growable.withContent_ vector \contents -> Control.do
        let !(left, right) = Fixed.splitAt 2 contents
        consume
          Control.<$> parBO
            (Fixed.modify 0 (+ 10) left)
            (Fixed.modify 0 (+ 20) right)
      vector <- Growable.push 5 vector
      let !() = consume vector
      pureAfter (freezeList (reclaim lend))

test_additionalSurface :: TestTree
test_additionalSurface =
  testGroup
    "additional mirrored surface"
    [ testCase "shared copyAt reads without mutable recovery plumbing" do
        sharedCopy @?= (5, [4, 5, 6])
    , testCase "copyAt completes copying before the shared borrow ends" do
        copies <- newIORef 0
        retired <- newIORef 0
        copyAtAfterRetirement copies retired @?= 10
        copyCount <- readIORef copies
        copyCount @?= 1
        retirementCount <- readIORef retired
        retirementCount @?= 1
    , testCase "unsafeCopyAt completes copying before the shared borrow ends" do
        copies <- newIORef 0
        retired <- newIORef 0
        unsafeCopyAtAfterRetirement copies retired @?= 10
        copyCount <- readIORef copies
        copyCount @?= 1
        retirementCount <- readIORef retired
        retirementCount @?= 1
    , testCase "copyAtMut completes copying before mutable recovery" do
        copies <- newIORef 0
        retired <- newIORef 0
        copyAtMutAfterRetirement copies retired @?= 10
        copyCount <- readIORef copies
        copyCount @?= 1
        retirementCount <- readIORef retired
        retirementCount @?= 1
    , testCase "indicesMut returns the requested distinct elements" do
        selectedValues @?= ((4, 6), [4, 5, 6])
    , testCase "indicesMut rejects duplicate indices" do
        assertErrorPrefix
          "indicesMut: duplicate indices: [0,0]"
          duplicateIndices
    , testCase "fixed content can split safely in parallel before growth resumes" do
        parallelSplitContent @?= [11, 2, 23, 4, 5]
    ]

test_typingBoundaries :: TestTree
test_typingBoundaries =
  testGroup
    "typing boundaries"
    [ expectDeferredTypeError
        "GrowableVector element role is nominal"
        "Couldn't match type"
        badElementCoercion
    , expectDeferredTypeError
        "GrowableVector cannot be coerced to a fixed Vector"
        "Couldn't match representation of type"
        badGrowableToFixed
    , expectDeferredTypeError
        "a fixed Vector cannot be coerced to GrowableVector"
        "Couldn't match representation of type"
        badFixedToGrowable
    , expectDeferredTypeError
        "GrowableVector cannot be upcast to a fixed Vector"
        "Couldn't match representation of type"
        badGrowableToFixedUpcast
    , expectDeferredTypeError
        "a fixed Vector cannot be upcast to GrowableVector"
        "Couldn't match representation of type"
        badFixedToGrowableUpcast
    , expectDeferredTypeError
        "a growable borrow cannot swap lifetime indices"
        "Couldn't match type"
        badLifetimeSwapCase
    , expectDeferredTypeError
        "GrowableVector has no generic split"
        "DistributesAlias Growable.GrowableVector"
        badSplit
    , expectDeferredTypeError
        "GrowableVector cannot be copied"
        "cannot be copied!"
        badDuplicate
    , expectDeferredTypeError
        "fixed content cannot escape withContent"
        "Couldn't match type"
        badContentEscapeCase
    , expectDeferredTypeError
        "shared fixed content cannot escape withContent"
        "Couldn't match type"
        badSharedContentEscapeCase
    , expectDeferredTypeError
        "Copyable alone does not permit growable materialization"
        "Movable CopyOnly"
        badGrowableCopyableOnlyToVectorCase
    , expectDeferredTypeError
        "Copyable alone does not permit fixed materialization"
        "Movable CopyOnly"
        badFixedCopyableOnlyToVectorCase
    , expectDeferredTypeError
        "Movable alone does not permit copying through a shared borrow"
        "Copyable NonCopyable"
        badNonCopyableCopyAtCase
    , expectDeferredTypeError
        "Movable alone does not permit copying through a mutable borrow"
        "Copyable NonCopyable"
        badNonCopyableCopyAtMutCase
    ]
  where
    expectDeferredTypeError description expectedFragment value =
      testCase description do
        result <- Exception.try @Exception.SomeException (Exception.evaluate value)
        case result of
          Left exception ->
            assertBool
              ("unexpected deferred type error: " <> Exception.displayException exception)
              (expectedFragment `List.isInfixOf` Exception.displayException exception)
          Right _ ->
            assertFailure
              ("expected deferred type error containing " <> expectedFragment)
