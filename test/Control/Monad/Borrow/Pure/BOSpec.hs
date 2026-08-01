{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Borrow.Pure.BOSpec (
  module Control.Monad.Borrow.Pure.BOSpec,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Data.Functor.Linear qualified as Data
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Unsafe.Linear qualified as Unsafe
import Prelude (Int, ($), (+))

shortenShare :: (α >= β) => Share α a -> Share β a
shortenShare = subShare

addLinear :: Int %1 -> Int %1 -> Int
addLinear = Unsafe.toLinear2 (+)

test_instanceMethods :: TestTree
test_instanceMethods =
  testGroup
    "BO instance methods"
    [ testCase "linear liftA2" $
        linearly (\lin -> runBO_ lin (Control.liftA2 addLinear (Control.pure 20) (Control.pure 22))) @?= (42 :: Int)
    , testCase "non-linear liftA2" $
        linearly (\lin -> runBO_ lin (Data.liftA2 addLinear (Data.pure 20) (Data.pure 22))) @?= (42 :: Int)
    , testCase "linear sequencing" $
        linearly (\lin -> runBO_ lin (Control.pure () Control.>> Control.pure 42)) @?= (42 :: Int)
    ]
