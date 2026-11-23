{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
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
import GHC.Exts qualified as GHC
import GHC.IO (unsafePerformIO)
import Generics.Linear
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

-- A nonlinear function beside a linear field under a hand-written 'MP1 'One'.
data Mixed where
  Mixed :: (Int -> Int) -> Tracked %1 -> Mixed

instance Generic Mixed where
  type
    Rep Mixed =
      D1 GHC.Any (C1 GHC.Any (S1 GHC.Any (MP1 'GHC.Many (K1 R (Int -> Int))) :*: S1 GHC.Any (MP1 'GHC.One (K1 R Tracked))))
  from mixed = fromMixed mixed
    where
      fromMixed :: Mixed %1 -> Rep Mixed x
      fromMixed (Mixed f t) = M1 (M1 (M1 (MP1 (K1 f)) :*: M1 (MP1 (K1 t))))
  to rep = toMixed rep
    where
      toMixed :: Rep Mixed x %1 -> Mixed
      toMixed (M1 (M1 (M1 (MP1 (K1 f)) :*: M1 (MP1 (K1 t))))) = Mixed f t

deriving via Generically Mixed instance Copyable Mixed

-- A field whose multiplicity is a variable, which the derived Copyable of 0.1.0.0 accepted.
data Poly m where
  Poly :: Tracked %m -> Poly m

instance Generic (Poly m) where
  type Rep (Poly m) = D1 GHC.Any (C1 GHC.Any (S1 GHC.Any (MP1 m (K1 R Tracked))))
  from poly = fromPoly poly
    where
      fromPoly :: Poly m %1 -> Rep (Poly m) x
      fromPoly (Poly t) = M1 (M1 (M1 (MP1 (K1 t))))
  to rep = toPoly rep
    where
      toPoly :: Rep (Poly m) x %1 -> Poly m
      toPoly (M1 (M1 (M1 (MP1 (K1 t))))) = Poly t

deriving via Generically (Poly m) instance Copyable (Poly m)

test_copyNonlinearFields :: TestTree
test_copyNonlinearFields =
  testGroup
    "generic Copyable over nonlinear fields"
    [ testCase "shares the nonlinear field and copies the linear one" do
        copies <- newIORef 0
        retired <- newIORef False
        let !(Mixed f copied) =
              copy (UnsafeAlias (Mixed (NonLinear.+ 1) (Tracked copies retired 10)) :: Share Static Mixed)
        copyCount <- readIORef copies
        copyCount @?= 1
        writeIORef retired True
        (f 41, trackedValue copied) @?= (42, 10)
    , testCase "does not force the nonlinear field" do
        copies <- newIORef 0
        retired <- newIORef False
        let !(Mixed _ copied) =
              copy (UnsafeAlias (Mixed (NonLinear.error "copy forced the function") (Tracked copies retired 7)) :: Mut Static Mixed)
        copyCount <- readIORef copies
        copyCount @?= 1
        trackedValue copied @?= 7
    , testCase "copies a field whose multiplicity is a variable" do
        copies <- newIORef 0
        retired <- newIORef False
        let !(Poly copied) =
              copy (UnsafeAlias (Poly (Tracked copies retired 5)) :: Share Static (Poly 'GHC.One))
        copyCount <- readIORef copies
        copyCount @?= 1
        trackedValue copied @?= 5
    ]
