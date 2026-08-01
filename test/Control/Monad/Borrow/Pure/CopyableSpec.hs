{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Borrow.Pure.CopyableSpec (
  module Control.Monad.Borrow.Pure.CopyableSpec,
) where

import Control.Monad.Borrow.Pure.BO (Mut, Share, linearly, runBO_)
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias))
import Control.Monad.Borrow.Pure.Clone (AsCopyable (AsCopyable), Clone (clone))
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Data.Complex (Complex ((:+)))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import GHC.IO (unsafePerformIO)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

data Tracked = Tracked !(IORef Int) !(IORef Bool) !Int

instance Copyable Tracked where
  copy =
    Unsafe.toLinear
      \(UnsafeAlias value@(Tracked copies retired _)) ->
        case unsafePerformIO do
          wasRetired <- readIORef retired
          if wasRetired
            then NonLinear.error "copy evaluated after retirement"
            else modifyIORef' copies NonLinear.succ of
          () -> value

instance Consumable Tracked where
  consume = Unsafe.toLinear \_ -> ()

instance Dupable Tracked where
  dup2 = Unsafe.toLinear \value -> (value, value)

instance Movable Tracked where
  move = Unsafe.toLinear Ur

instance Copyable1 [] where
  liftCopy = genericLiftCopy

trackedValue :: Tracked -> Int
trackedValue (Tracked _ _ value) = value

cloneTracked :: IORef Int -> IORef Bool -> Tracked
cloneTracked copies retired =
  linearly \linear ->
    case runBO_
      linear
      (clone (UnsafeAlias (AsCopyable (Tracked copies retired 10)))) of
      AsCopyable copied -> copied

test_copyStrictness :: TestTree
test_copyStrictness =
  testGroup
    "copy strictness"
    [ testCase "copyMut completes the copy before returning Ur" do
        copies <- newIORef 0
        retired <- newIORef False
        let source = Tracked copies retired 10
            !(Ur copied) =
              copyMut
                (UnsafeAlias source :: Mut Static Tracked)
        copyCount <- readIORef copies
        copyCount @?= 1
        writeIORef retired True
        trackedValue copied @?= 10
    , testCase "generic Copyable forces every copied field" do
        copies <- newIORef 0
        retired <- newIORef False
        let first = Tracked copies retired 10
            second = Tracked copies retired 20
            !(firstCopy, secondCopy) =
              copy
                ( UnsafeAlias (first, second) ::
                    Share Static (Tracked, Tracked)
                )
        copyCount <- readIORef copies
        copyCount @?= 2
        writeIORef retired True
        (trackedValue firstCopy, trackedValue secondCopy) @?= (10, 20)
    , testCase "generic Copyable traverses recursive values before returning" do
        copies <- newIORef 0
        retired <- newIORef False
        let first = Tracked copies retired 10
            second = Tracked copies retired 20
            !copied =
              copy
                ( UnsafeAlias [first, second] ::
                    Share Static [Tracked]
                )
        copyCount <- readIORef copies
        copyCount @?= 2
        writeIORef retired True
        NonLinear.map trackedValue copied @?= [10, 20]
    , testCase "Complex Copyable copies both components" do
        copies <- newIORef 0
        retired <- newIORef False
        let real = Tracked copies retired 10
            imaginary = Tracked copies retired 20
            !(realCopy :+ imaginaryCopy) =
              copy
                ( UnsafeAlias (real :+ imaginary) ::
                    Share Static (Complex Tracked)
                )
        copyCount <- readIORef copies
        copyCount @?= 2
        writeIORef retired True
        (trackedValue realCopy, trackedValue imaginaryCopy) @?= (10, 20)
    , testCase "Complex Double moves without changing its numeric value" do
        case move (10 :+ 20 :: Complex Double) of
          Ur moved -> moved @?= (10 :+ 20)
    , testCase "Copyable1 forces every copied field" do
        copies <- newIORef 0
        retired <- newIORef False
        let real = Tracked copies retired 10
            imaginary = Tracked copies retired 20
            !(realCopy :+ imaginaryCopy) =
              copy1
                ( UnsafeAlias (real :+ imaginary) ::
                    Share Static (Complex Tracked)
                )
        copyCount <- readIORef copies
        copyCount @?= 2
        writeIORef retired True
        (trackedValue realCopy, trackedValue imaginaryCopy) @?= (10, 20)
    , testCase "Copyable1 traverses recursive values before returning" do
        copies <- newIORef 0
        retired <- newIORef False
        let first = Tracked copies retired 10
            second = Tracked copies retired 20
            !copied =
              copy1
                ( UnsafeAlias [first, second] ::
                    Share Static [Tracked]
                )
        copyCount <- readIORef copies
        copyCount @?= 2
        writeIORef retired True
        NonLinear.map trackedValue copied @?= [10, 20]
    , testCase "Clone through AsCopyable completes copying inside BO" do
        copies <- newIORef 0
        retired <- newIORef False
        let !copied = cloneTracked copies retired
        copyCount <- readIORef copies
        copyCount @?= 1
        writeIORef retired True
        trackedValue copied @?= 10
    ]
