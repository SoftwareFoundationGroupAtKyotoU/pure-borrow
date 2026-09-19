{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Control.Monad.Borrow.IO.TypingCases (
  module Control.Monad.Borrow.IO.TypingCases,
) where

import Control.Exception (evaluate)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Affine (aff, pop)
import Control.Monad.Borrow.BO
import Control.Monad.Borrow.IO
import Control.Monad.Borrow.Unsafe (unsafeBOToLinIO, unsafeLiftBIO)
import Data.Coerce (coerce)
import Data.Vector qualified as Vector
import Data.Vector.Mutable.Linear.Borrow qualified as BorrowVector
import Prelude.Linear qualified as Linear
import System.IO.Linear qualified as LinearIO

data CustomWorld

badPureLift :: IO Int
badPureLift =
  evaluate $
    Linear.unur
      ( linearly \lin ->
          runBO_ lin (liftSystemIOU (pure 7))
      )

launderWorld :: BIO α a -> BO α a
launderWorld = coerce

badWorldCoercion :: IO Int
badWorldCoercion =
  evaluate $
    Linear.unur
      ( linearly \lin ->
          runBO_ lin (launderWorld (Control.pure (Linear.Ur 7)))
      )

badPureUnsafeLift :: IO Int
badPureUnsafeLift =
  evaluate $
    Linear.unur
      ( linearly \lin ->
          runBO_ lin (unsafeLiftBIO (Control.pure (Linear.Ur 7)))
      )

badCustomRunner :: IO Int
badCustomRunner =
  LinearIO.withLinearIO $
    runBIO_ (Control.pure (Linear.Ur 7) :: forall α. BO' CustomWorld α (Linear.Ur Int))

badCustomExecSource :: IO Int
badCustomExecSource = LinearIO.withLinearIO Control.do
  (now, result) <- execBIO (Control.pure (Linear.Ur 7) :: BO' CustomWorld Static (Linear.Ur Int)) nowStatic
  pop (aff now) `Linear.lseq` Control.pure result

badCustomExecTarget :: IO Int
badCustomExecTarget = LinearIO.withLinearIO (unsafeBOToLinIO customTarget)
  where
    customTarget :: BO' CustomWorld Static (Linear.Ur Int)
    customTarget = Control.do
      (now, result) <- execBIO (Control.pure (Linear.Ur 7)) nowStatic
      pop (aff now) `Linear.lseq` Control.pure result

effectfulMutation :: Mut α (BorrowVector.Vector Int) %1 -> BIO α ()
effectfulMutation mut =
  Linear.consume mut `Linear.lseq` Control.do
    Linear.Ur () <- liftSystemIOU (pure ())
    Control.pure ()

badBoxedSTAdapter :: IO (Vector.Vector Int)
badBoxedSTAdapter = evaluate $ BorrowVector.modifyBoxedVector effectfulMutation (Vector.singleton 1)
