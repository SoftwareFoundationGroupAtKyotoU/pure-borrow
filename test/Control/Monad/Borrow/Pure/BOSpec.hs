{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Borrow.Pure.BOSpec (
  module Control.Monad.Borrow.Pure.BOSpec,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO qualified as BO
import Data.Functor.Linear qualified as Data
import Data.Type.Equality ((:~:))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Unsafe.Linear qualified as Unsafe
import Prelude (Int, ($), (+))

assocBorrowEqTypingCase ::
  forall (bk :: BO.BorrowKind) α β γ a.
  BO.Borrow bk ((α /\ β) /\ γ) a :~: BO.Borrow bk (α /\ (β /\ γ)) a
assocBorrowEqTypingCase = BO.assocBorrowEq @bk @α @β @γ @a

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
