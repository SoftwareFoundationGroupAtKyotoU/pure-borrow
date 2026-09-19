{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}

module SchedulerWithoutForkable where

import Control.Concurrent.DivideConquer.Linear qualified as DivideConquer
import Control.Monad.Borrow.BO
import Data.Vector qualified as Vector
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as BorrowVector
import System.Random (mkStdGen)

data ThreadBound

bad :: Mut α (BorrowVector.Vector Vector.Vector Int) %1 -> BO' ThreadBound α (Mut α (BorrowVector.Vector Vector.Vector Int))
bad = DivideConquer.qsortDC (mkStdGen 42) 2 2
