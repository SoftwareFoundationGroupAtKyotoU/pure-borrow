{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
The FFT butterfly, kept in its own module solely so that
@-fno-spec-constr@ can apply to it and nothing else.

SpecConstr on this loop defeats the specialization that
@pure-borrow-inspection@ requires: at @-O2@ the @combine loop has no
type-class dictionaries@ assertion fails without the flag. Setting the flag
on "Control.Concurrent.DivideConquer.Linear" instead would also disable
SpecConstr for the scheduler, the Chase-Lev queue and the worker loop, which
measures up to 32% slower on @fft.worksteal@. The flag is module-scoped and
has no per-binding form, so a one-binding module is the only way to scope it
correctly.
-}
module Control.Concurrent.DivideConquer.Linear.Internal (
  combineLoop,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Data.Complex (Complex)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Vector
import Prelude.Linear
import Prelude qualified as P

combineLoop ::
  (G.Vector v (Complex Double)) =>
  Int ->
  Complex Double ->
  Int ->
  Complex Double ->
  Mut α (Vector.Vector v (Complex Double)) %1 ->
  BO α ()
{-# INLINEABLE combineLoop #-}
combineLoop !half !root !index !weight vector
  | index >= half =
      Control.pure (consume vector)
  | otherwise = Control.do
      (Ur evenValue, vector) <-
        Vector.unsafeGet index vector
      (Ur oddValue, vector) <-
        Vector.unsafeGet (half + index) vector
      let !weightedOdd = weight P.* oddValue
      vector <-
        Vector.unsafeWrite
          index
          (evenValue P.+ weightedOdd)
          vector
      vector <-
        Vector.unsafeWrite
          (half + index)
          (evenValue P.- weightedOdd)
          vector
      combineLoop
        half
        root
        (index + 1)
        (weight P.* root)
        vector
