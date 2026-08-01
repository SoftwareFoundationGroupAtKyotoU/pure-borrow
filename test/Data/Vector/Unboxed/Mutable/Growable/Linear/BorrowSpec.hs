{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Vector.Unboxed.Mutable.Growable.Linear.BorrowSpec (
  module Data.Vector.Unboxed.Mutable.Growable.Linear.BorrowSpec,
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
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow qualified as Growable
import Data.Vector.Unboxed.Mutable.Growable.Linear.TypingCases
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as Fixed
import GHC.IO (unsafePerformIO)
import Prelude.Linear
import PureBorrow.Internal.Bench.Unboxed qualified as UnboxedBench
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
      vector <- Growable.extend (U.fromList [first, second]) vector
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
    Ur frozen -> U.toList frozen

freezeLength :: Growable.GrowableVector Int %1 -> Int
freezeLength vector =
  case Growable.toVector vector of
    Ur frozen -> U.length frozen

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
                Ur (U.toList frozen, logicalSize, finalCapacity)

test_model :: TestTree
test_model =
  testProperty "matches a list model across growth and mutation" do
    initialCapacity <- F.gen $ G.int $ G.between (0, 16)
    seeds <- F.gen $ G.list (G.between (0, 100)) $ G.int $ G.between (-100, 100)
    let !operations = NonLinear.map decodeOperation seeds
        !(actual, logicalSize, finalCapacity) =
          runOperations initialCapacity operations
        !expected = applyModel operations []
    F.assert $ P.expect expected P..$ ("contents", actual)
    F.assert $
      P.expect (NonLinear.length expected) P..$ ("logical size", logicalSize)
    F.assert $
      P.satisfies
        ("capacity >= logical size", (NonLinear.>= logicalSize))
        P..$ ("capacity", finalCapacity)

test_construction :: TestTree
test_construction =
  testGroup
    "construction"
    [ testCase "empty has no initialized elements" do
        linearly (\linear -> freezeList (Growable.empty linear)) @?= []
    , testCase "constant initializes the complete logical prefix" do
        linearly
          (\linear -> freezeList (Growable.constant 3 (7 :: Int) linear))
          @?= [7, 7, 7]
    , testCase "fromList moves every element" do
        linearly
          (\linear -> freezeList (Growable.fromList [4, 5, 6 :: Int] linear))
          @?= [4, 5, 6]
    , testCase "fromVector copies every element" do
        linearly
          ( \linear ->
              freezeList
                (Growable.fromVector (U.fromList [8, 9 :: Int]) linear)
          )
          @?= [8, 9]
    , testCase "unsafe mutable adoption preserves a nonzero slice offset across growth" do
        linearly
          ( \linear ->
              let source =
                    unsafePerformIO do
                      whole <- U.thaw (U.fromList [99, 1, 2, 88 :: Int])
                      NonLinear.pure (UM.unsafeSlice 1 2 whole)
               in growAdopted source linear
          )
          @?= [1, 2, 3]
    ]

growAdopted ::
  UM.IOVector Int %1 ->
  Linearly %1 ->
  [Int]
growAdopted =
  Unsafe.toLinear2 \source linear ->
    unur $ DataFlow.do
      (ownerLinear, runLinear) <- dup linear
      runBO runLinear Control.do
        (vector, lend) <-
          borrowM (Growable.unsafeFromMutable source ownerLinear)
        vector <- Growable.push 3 vector
        let !() = consume vector
        pureAfter $
          case Growable.toVector (reclaim lend) of
            Ur frozen -> Ur (U.toList frozen)

mirroredSurface :: ((Int, Int, Int, Int), [Int])
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
      (Ur copied, vector) <- Growable.copyAtMut 1 vector
      (old, vector) <- Growable.set 1 20 vector
      let !() = consume old
      ((), vector) <-
        Growable.update
          1
          (\ !value -> Control.pure ((), value + 1))
          vector
      vector <- Growable.modify 0 (+ 10) vector
      vector <- Growable.swap vector 0 2
      let !() = consume vector
      pureAfter
        ( (middle, first, final, copied)
        , freezeList (reclaim lend)
        )

test_mirroredSurface :: TestTree
test_mirroredSurface =
  testCase "mirrors fixed unboxed reads and mutation" do
    mirroredSurface @?= ((2, 1, 3, 2), [3, 21, 11])

contentRoundTrip :: ((Int, Int), [Int])
contentRoundTrip =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromVector (U.fromList [10, 20, 30]) ownerLinear)
      ((logicalSize, first), vector) <-
        Growable.withContent vector \contents -> Control.do
          Fixed.size contents & \(Ur logicalSize, contents) -> Control.do
            (Ur first, contents) <- Fixed.copyAtMut 0 contents
            contents <- Fixed.modify 1 (+ 1) contents
            Control.pure (consume contents `lseq` (logicalSize, first))
      vector <- Growable.push 40 vector
      let !() = consume vector
      pureAfter
        ( (logicalSize, first)
        , freezeList (reclaim lend)
        )

directMutableProjection :: (Int, [Int])
directMutableProjection =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.withCapacity 8 ownerLinear)
      vector <- Growable.extend (U.fromList [3, 4, 5]) vector
      Fixed.size (Growable.getContents vector) & \(Ur logicalSize, contents) -> Control.do
        contents <- Fixed.modify 0 (+ 10) contents
        let !() = consume contents
        pureAfter
          ( logicalSize
          , freezeList (reclaim lend)
          )

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

sharedContentProjection :: ((Int, Int), [Int])
sharedContentProjection =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [5, 6, 7] ownerLinear)
      share vector & \(Ur sharedVector) -> Control.do
        (first, returnedSharedVector) <-
          Growable.withContent sharedVector \linearContents ->
            move linearContents & \(Ur contents) -> Control.do
              Ur first <- Fixed.copyAt 0 contents
              Control.pure first
        move returnedSharedVector & \(Ur sharedVector) -> Control.do
          Ur final <- Fixed.copyAt 2 (Growable.getContents sharedVector)
          let !() = consume sharedVector
          pureAfter ((first, final), freezeList (reclaim lend))

countedContentScope :: IORef Int -> [Int]
countedContentScope counter =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [1, 2, 3] ownerLinear)
      vector <- Growable.withContent_ vector \contents ->
        case unsafePerformIO (modifyIORef' counter NonLinear.succ) of
          () -> Control.pure (consume contents)
      vector <- Growable.push 4 vector
      let !() = consume vector
      pureAfter (freezeList (reclaim lend))

test_contentProjection :: TestTree
test_contentProjection =
  testGroup
    "content projection"
    [ testCase "exposes only initialized content and restores growth" do
        contentRoundTrip @?= ((3, 10), [10, 21, 30, 40])
    , testCase "direct mutable projection exposes only the initialized prefix" do
        directMutableProjection @?= (3, [13, 4, 5])
    , testCase "permits fixed content to split before growth resumes" do
        parallelSplitContent @?= [11, 2, 23, 4, 5]
    , testCase "preserves shared content access" do
        sharedContentProjection @?= ((5, 7), [5, 6, 7])
    , testCase "runs a content callback exactly once" do
        counter <- newIORef 0
        countedContentScope counter @?= [1, 2, 3, 4]
        count <- readIORef counter
        count @?= 1
    ]

data Tracked = Tracked !(IORef Int) !Int

instance Consumable (U.DoNotUnboxLazy Tracked) where
  consume =
    Unsafe.toLinear \(U.DoNotUnboxLazy (Tracked counter _)) ->
      unsafePerformIO (modifyIORef' counter NonLinear.succ)

instance Consumable (U.DoNotUnboxStrict Tracked) where
  consume =
    Unsafe.toLinear \(U.DoNotUnboxStrict (Tracked counter _)) ->
      unsafePerformIO (modifyIORef' counter NonLinear.succ)

trackedGrowth :: IORef Int -> Int
trackedGrowth counter =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.withCapacity 0 ownerLinear)
      vector <- Growable.push (U.DoNotUnboxLazy (Tracked counter 10)) vector
      vector <- Growable.push (U.DoNotUnboxLazy (Tracked counter 20)) vector
      vector <- Growable.reserve 32 vector
      vector <- Growable.push (U.DoNotUnboxLazy (Tracked counter 30)) vector
      vector <- Growable.reserveAdditional 64 vector
      (oldLabel, vector) <-
        Growable.update
          1
          ( \(U.DoNotUnboxLazy (Tracked elementCounter label)) ->
              case dup label of
                (oldLabel, replacementLabel) ->
                  Control.pure
                    ( oldLabel
                    , U.DoNotUnboxLazy
                        (Tracked elementCounter (replacementLabel + 1))
                    )
          )
          vector
      let !() = consume vector
      pureAfter (consume (reclaim lend) `lseq` oldLabel)

strictTrackedGrowth :: IORef Int -> ()
strictTrackedGrowth counter =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.withCapacity 0 ownerLinear)
      vector <- Growable.push (U.DoNotUnboxStrict (Tracked counter 10)) vector
      vector <- Growable.push (U.DoNotUnboxStrict (Tracked counter 20)) vector
      vector <- Growable.reserve 32 vector
      vector <- Growable.push (U.DoNotUnboxStrict (Tracked counter 30)) vector
      vector <- Growable.reserveAdditional 64 vector
      let !() = consume vector
      pureAfter (consume (reclaim lend))

data CopyTracked = CopyTracked !(IORef Int) !(IORef Int) !Int

type UnboxedCopyTracked = U.DoNotUnboxLazy CopyTracked

instance Copyable UnboxedCopyTracked where
  copy =
    Unsafe.toLinear
      \(UnsafeAlias value@(U.DoNotUnboxLazy (CopyTracked copies retired _))) ->
        case unsafePerformIO do
          retirementCount <- readIORef retired
          if retirementCount == 0
            then modifyIORef' copies NonLinear.succ
            else NonLinear.error "copy invoked after source retirement" of
          () -> value

instance Consumable UnboxedCopyTracked where
  consume =
    Unsafe.toLinear
      \(U.DoNotUnboxLazy (CopyTracked _ retired _)) ->
        unsafePerformIO (modifyIORef' retired NonLinear.succ)

data MoveTracked = MoveTracked !(IORef Int) !Int !Bool

type UnboxedMoveTracked = U.DoNotUnboxLazy MoveTracked

instance Consumable UnboxedMoveTracked where
  consume = Unsafe.toLinear \_ -> ()

instance Dupable UnboxedMoveTracked where
  dup2 = Unsafe.toLinear \value -> (value, value)

instance Movable UnboxedMoveTracked where
  move =
    Unsafe.toLinear
      \(U.DoNotUnboxLazy (MoveTracked moves value _)) ->
        case unsafePerformIO (modifyIORef' moves NonLinear.succ) of
          () -> Ur (U.DoNotUnboxLazy (MoveTracked moves value True))

materializeMoveTracked :: IORef Int -> [(Int, Bool)]
materializeMoveTracked moves =
  NonLinear.map
    ( \(U.DoNotUnboxLazy (MoveTracked _ value wasMoved)) ->
        (value, wasMoved)
    )
    ( U.toList $
        unur $
          linearly \linear -> DataFlow.do
            (ownerLinear, runLinear) <- dup linear
            runBO runLinear Control.do
              (vector, lend) <- borrowM (Growable.empty ownerLinear)
              vector <-
                Growable.push
                  (U.DoNotUnboxLazy (MoveTracked moves 10 False))
                  vector
              vector <-
                Growable.push
                  (U.DoNotUnboxLazy (MoveTracked moves 20 False))
                  vector
              let !() = consume vector
              pureAfter (Growable.toVector (reclaim lend))
    )

discardMaterializedMoveTracked :: IORef Int -> ()
discardMaterializedMoveTracked moves =
  linearly \linear ->
    case Growable.toVector
      ( Growable.fromVector
          ( U.fromList
              [ U.DoNotUnboxLazy (MoveTracked moves 10 False)
              , U.DoNotUnboxLazy (MoveTracked moves 20 False)
              ]
          )
          linear
      ) of
      Ur _ -> ()

immutableCopyLifecycle :: IORef Int -> IORef Int -> ()
immutableCopyLifecycle copies retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromVector
              ( U.fromList
                  [ U.DoNotUnboxLazy (CopyTracked copies retired 10)
                  , U.DoNotUnboxLazy (CopyTracked copies retired 20)
                  ]
              )
              ownerLinear
          )
      vector <- Growable.reserve 64 vector
      vector <-
        Growable.extend
          (U.singleton (U.DoNotUnboxLazy (CopyTracked copies retired 30)))
          vector
      vector <- Growable.reserveAdditional 64 vector
      let !() = consume vector
      pureAfter (consume (reclaim lend))

retireCopiedResult ::
  (Ur UnboxedCopyTracked, Mut α (Growable.GrowableVector UnboxedCopyTracked)) %1 ->
  Growable.GrowableVector UnboxedCopyTracked %1 ->
  Int
retireCopiedResult =
  Unsafe.toLinear2 \(copiedResult, borrowed) owner ->
    consume borrowed `lseq`
      consume owner `lseq`
        case copiedResult of
          Ur (U.DoNotUnboxLazy (CopyTracked _ _ value)) -> value

copyAtMutAfterRetirement :: IORef Int -> IORef Int -> Int
copyAtMutAfterRetirement copies retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromVector
              (U.singleton (U.DoNotUnboxLazy (CopyTracked copies retired 10)))
              ownerLinear
          )
      copiedResult <- Growable.copyAtMut 0 vector
      pureAfter (retireCopiedResult copiedResult (reclaim lend))

test_copyAtMutStrictness :: TestTree
test_copyAtMutStrictness =
  testCase "copyAtMut completes copying before mutable recovery" do
    copies <- newIORef 0
    retired <- newIORef 0
    copyAtMutAfterRetirement copies retired @?= 10
    copyCount <- readIORef copies
    copyCount @?= 1
    retirementCount <- readIORef retired
    retirementCount @?= 1

gcOwnedImmutableLifecycle :: IORef Int -> ()
gcOwnedImmutableLifecycle retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromVector
              (U.singleton (U.DoNotUnboxLazy (Tracked retired 10)))
              ownerLinear
          )
      vector <-
        Growable.extend
          ( U.fromList
              [ U.DoNotUnboxLazy (Tracked retired 20)
              , U.DoNotUnboxLazy (Tracked retired 30)
              ]
          )
          vector
      let !() = consume vector
      pureAfter (consume (reclaim lend))

newtype LinearElement = LinearElement (Ref.Ref Int)

type BoxedLinearElement = U.DoNotUnboxLazy LinearElement

instance Consumable BoxedLinearElement where
  consume =
    Unsafe.toLinear \(U.DoNotUnboxLazy (LinearElement ref)) ->
      consume ref

asBorrowedRef ::
  Mut α BoxedLinearElement %1 ->
  Mut α (Ref.Ref Int)
asBorrowedRef = upcast

borrowedRefAcrossGrowth :: Int
borrowedRefAcrossGrowth =
  linearly \linear -> DataFlow.do
    (refLinear, remainingLinear) <- dup linear
    (ownerLinear, runLinear) <- dup remainingLinear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.empty ownerLinear)
      vector <-
        Growable.push
          (U.DoNotUnboxLazy (LinearElement (Ref.new 1 refLinear)))
          vector
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

test_destructiveGrowth :: TestTree
test_destructiveGrowth =
  testGroup
    "destructive growth"
    [ testCase "moves non-Copyable capabilities and retires each exactly once" do
        counter <- newIORef 0
        oldLabel <- Exception.evaluate (trackedGrowth counter)
        oldLabel @?= 20
        retired <- readIORef counter
        retired @?= 3
    , testCase "moves strict boxed-backed capabilities and retires each exactly once" do
        counter <- newIORef 0
        _ <- Exception.evaluate (strictTrackedGrowth counter)
        retired <- readIORef counter
        retired @?= 3
    , testCase "preserves a nested Ref identity across reallocation" do
        borrowedRefAcrossGrowth @?= 42
    , testCase "ordinary constructors need no Copyable instance" do
        counter <- newIORef 0
        _ <-
          Exception.evaluate $
            linearly \linear ->
              consume
                ( Growable.constant
                    2
                    (U.DoNotUnboxLazy (Tracked counter 10))
                    linear
                )
        retired <- readIORef counter
        retired @?= 2
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
        borrowM (Growable.fromVector (U.fromList [10, 20, 30]) ownerLinear)
      (Ur value, vector) <- Growable.copyAtMut index vector
      let !() = consume vector
      pureAfter (value + freezeLength (reclaim lend))

getOutOfBounds :: Int -> Int
getOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [10, 20, 30] ownerLinear)
      (Ur value, vector) <-
        reborrowing vector \short -> Control.do
          element <- Growable.get index short
          Control.pure (copyMut element)
      let !() = consume vector
      pureAfter (value + freezeLength (reclaim lend))

setOutOfBounds :: Int -> Int
setOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [10, 20, 30] ownerLinear)
      (old, vector) <- Growable.set index 0 vector
      let !() = consume vector
      pureAfter (old + freezeLength (reclaim lend))

updateOutOfBounds :: Int -> Int
updateOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [10, 20, 30] ownerLinear)
      ((), vector) <-
        Growable.update
          index
          (\value -> Control.pure ((), value))
          vector
      let !() = consume vector
      pureAfter (freezeLength (reclaim lend))

swapOutOfBounds :: Int -> Int -> Int
swapOutOfBounds first second =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [10, 20, 30] ownerLinear)
      vector <- Growable.swap vector first second
      let !() = consume vector
      pureAfter (freezeLength (reclaim lend))

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

reserveOutOfBounds :: Int -> Int
reserveOutOfBounds requested =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.empty ownerLinear)
      vector <- Growable.reserve requested vector
      let !() = consume vector
      pureAfter (freezeLength (reclaim lend))

reserveAdditionalOutOfBounds :: Int -> Int
reserveAdditionalOutOfBounds additional =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.empty ownerLinear)
      vector <- Growable.reserveAdditional additional vector
      let !() = consume vector
      pureAfter (freezeLength (reclaim lend))

capacityTransitions :: (Int, Int, Int, Int)
capacityTransitions =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.withCapacity @Int 2 ownerLinear)
      Growable.capacity vector & \(Ur initial, vector) -> Control.do
        vector <- Growable.push 1 vector
        Growable.capacity vector & \(Ur afterFirst, vector) -> Control.do
          vector <- Growable.push 2 vector
          Growable.capacity vector & \(Ur afterSecond, vector) -> Control.do
            vector <- Growable.push 3 vector
            Growable.capacity vector & \(Ur afterGrowth, vector) -> DataFlow.do
              consume vector
              pureAfter
                ( consume (reclaim lend) `lseq`
                    (initial, afterFirst, afterSecond, afterGrowth)
                )

test_bounds :: TestTree
test_bounds =
  testGroup
    "bounds"
    [ testCase "get rejects a negative index" do
        assertErrorPrefix
          "get: index -1 out of bounds for length 3"
          (getOutOfBounds (-1))
    , testCase "get rejects the upper bound" do
        assertErrorPrefix
          "get: index 3 out of bounds for length 3"
          (getOutOfBounds 3)
    , testCase "copyAtMut rejects a negative index" do
        assertErrorPrefix
          "copyAtMut: index -1 out of bounds for length 3"
          (copyOutOfBounds (-1))
    , testCase "copyAtMut rejects the upper bound" do
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
    , testCase "update rejects an invalid index" do
        assertErrorPrefix
          "update: index 3 out of bounds for length 3"
          (updateOutOfBounds 3)
    , testCase "swap rejects an invalid index" do
        assertErrorPrefix
          "swap: indices (0,3) out of bounds for length 3"
          (swapOutOfBounds 0 3)
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
    , testCase "reserve rejects negative capacity" do
        assertErrorPrefix
          "reserve: negative capacity -1"
          (reserveOutOfBounds (-1))
    , testCase "reserveAdditional rejects a negative amount" do
        assertErrorPrefix
          "reserveAdditional: negative additional capacity -1"
          (reserveAdditionalOutOfBounds (-1))
    , testCase "push preserves exact capacity until one element exceeds it" do
        capacityTransitions @?= (2, 2, 2, 4)
    ]

test_typingBoundaries :: TestTree
test_typingBoundaries =
  testGroup
    "typing boundaries"
    [ expectDeferredTypeError
        "element role is nominal"
        "Couldn't match type"
        badElementCoercion
    , expectDeferredTypeError
        "growable and fixed vectors are representation-distinct"
        "Couldn't match representation of type"
        badGrowableToFixed
    , expectDeferredTypeError
        "fixed and growable vectors are representation-distinct"
        "Couldn't match representation of type"
        badFixedToGrowable
    , expectDeferredTypeError
        "growable cannot be upcast to fixed"
        "Couldn't match representation of type"
        badGrowableToFixedUpcast
    , expectDeferredTypeError
        "fixed cannot be upcast to growable"
        "Couldn't match representation of type"
        badFixedToGrowableUpcast
    , expectDeferredTypeError
        "unboxed and boxed growable vectors are representation-distinct"
        "Couldn't match representation of type"
        badUnboxedGrowableToBoxedGrowable
    , expectDeferredTypeError
        "boxed and unboxed growable vectors are representation-distinct"
        "Couldn't match representation of type"
        badBoxedGrowableToUnboxedGrowable
    , expectDeferredTypeError
        "unboxed growable cannot be upcast to boxed growable"
        "Couldn't match representation of type"
        badUnboxedGrowableToBoxedGrowableUpcast
    , expectDeferredTypeError
        "boxed growable cannot be upcast to unboxed growable"
        "Couldn't match representation of type"
        badBoxedGrowableToUnboxedGrowableUpcast
    , expectDeferredTypeError
        "borrow lifetime cannot be swapped"
        "Couldn't match type"
        badLifetimeSwapCase
    , expectDeferredTypeError
        "growable vector has no generic split"
        "DistributesAlias Growable.GrowableVector"
        badSplit
    , expectDeferredTypeError
        "growable vector cannot be copied"
        "cannot be copied!"
        badDuplicate
    , expectDeferredTypeError
        "mutable content cannot escape its scope"
        "Couldn't match type"
        badContentEscapeCase
    , expectDeferredTypeError
        "shared content cannot escape its scope"
        "Couldn't match type"
        badSharedContentEscapeCase
    , expectDeferredTypeError
        "Copyable alone does not permit growable materialization"
        "Movable (U.DoNotUnboxLazy CopyOnly)"
        badGrowableCopyableOnlyToVectorCase
    , expectDeferredTypeError
        "Copyable alone does not permit fixed materialization"
        "Movable (U.DoNotUnboxLazy CopyOnly)"
        badFixedCopyableOnlyToVectorCase
    , expectDeferredTypeError
        "Movable alone does not permit copying through a shared borrow"
        "Copyable (U.DoNotUnboxLazy NonCopyable)"
        badNonCopyableCopyAtCase
    , expectDeferredTypeError
        "Movable alone does not permit copying through a mutable borrow"
        "Copyable (U.DoNotUnboxLazy NonCopyable)"
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

test_benchmarkRoots :: TestTree
test_benchmarkRoots =
  testGroup
    "benchmark roots"
    [ testGroup
        ("length " <> show length_)
        [ testCase "no-growth direct and Pure Borrow roots agree" do
            let input =
                  U.generate length_ (\index -> index `NonLinear.rem` 17)
            UnboxedBench.pureBorrowGrowableUnboxedNoGrowthKernel input
              @?= UnboxedBench.directGrowableUnboxedNoGrowthKernel input
        , testCase "forced-growth direct and Pure Borrow roots agree" do
            let input =
                  U.generate length_ (\index -> index `NonLinear.rem` 17)
                direct@(_, finalCapacity) =
                  UnboxedBench.directGrowableUnboxedGrowthKernel input
            UnboxedBench.pureBorrowGrowableUnboxedGrowthKernel input
              @?= direct
            finalCapacity @?= expectedGrowthCapacity length_
        , testCase "public-materialization roots agree" do
            let input =
                  U.generate length_ (\index -> index `NonLinear.rem` 17)
            UnboxedBench.pureBorrowGrowableUnboxedMaterialization input
              @?= UnboxedBench.directGrowableUnboxedMaterialization input
        ]
    | length_ <- [0, 1, 257, 1024 * 1024]
    ]

expectedGrowthCapacity :: Int -> Int
expectedGrowthCapacity logicalSize = go 0
  where
    go capacity
      | capacity >= logicalSize = capacity
      | capacity <= 0 = go 1
      | otherwise = go (capacity * 2)
