{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-spec-constr -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Concurrent.DivideConquer.Linear.Unrestricted.Internal (
  module Control.Concurrent.DivideConquer.Linear.Unrestricted.Internal,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine (Affine, GenericallyAffine (..))
import Control.Monad.Borrow.Pure.BO
import Data.Complex (Complex)
import Data.Functor.Linear qualified as Data
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Vector
import GHC.Generics qualified as GHC
import Generics.Linear.TH (deriveGenericAnd1)
import Prelude.Linear
import Prelude.Linear.Generically (Generically, Generically1)
import Prelude qualified as NonLinear

data Pair a where
  Pair :: !a %1 -> !a %1 -> Pair a
  deriving (GHC.Generic, GHC.Generic1)

deriveGenericAnd1 ''Pair

deriving via Generically1 Pair instance Data.Functor Pair

deriving via
  Generically (Pair a)
  instance
    (Consumable a) => Consumable (Pair a)

deriving via
  Generically (Pair a)
  instance
    (Dupable a) => Dupable (Pair a)

deriving via
  GenericallyAffine (Pair a)
  instance
    (Affine a) => Affine (Pair a)

deriving via
  Generically (Pair a)
  instance
    (Movable a) => Movable (Pair a)

instance Data.Traversable Pair where
  traverse = Data.genericTraverse
  {-# INLINE traverse #-}

data FftCoe = FftCoe
  { cosθ :: {-# UNPACK #-} !Double
  , sinθ :: {-# UNPACK #-} !Double
  , size :: {-# UNPACK #-} !Int
  }
  deriving (Show)

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
      let !weightedOdd = weight NonLinear.* oddValue
      vector <-
        Vector.unsafeWrite
          index
          (evenValue NonLinear.+ weightedOdd)
          vector
      vector <-
        Vector.unsafeWrite
          (half + index)
          (evenValue NonLinear.- weightedOdd)
          vector
      combineLoop
        half
        root
        (index + 1)
        (weight NonLinear.* root)
        vector
