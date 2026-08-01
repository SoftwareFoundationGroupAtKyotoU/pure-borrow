{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PureBorrow.Inspection.Fft (
  tests,
  unboxedFftCombineLoop,
) where

import Control.Concurrent.DivideConquer.Linear (combineLoop)
import Control.Monad.Borrow.Pure.BO (BO, Mut)
import Data.Complex (Complex)
import Data.Vector.Generic.Mutable qualified as Generic
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Vector
import Data.Vector.Mutable qualified as Boxed
import Data.Vector.Unboxed qualified as Unboxed
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

{-# SPECIALIZE combineLoop ::
  Int ->
  Complex Double ->
  Int ->
  Complex Double ->
  Mut α (Vector.Vector Unboxed.Vector (Complex Double)) %1 ->
  BO α ()
  #-}

unboxedFftCombineLoop ::
  Int ->
  Complex Double ->
  Int ->
  Complex Double ->
  Mut α (Vector.Vector Unboxed.Vector (Complex Double)) %1 ->
  BO α ()
{-# NOINLINE unboxedFftCombineLoop #-}
unboxedFftCombineLoop = combineLoop

tests :: TestTree
tests =
  testGroup
    "FFT"
    [ $( inspectTest
           ( (hasNoTypeClasses 'unboxedFftCombineLoop)
               { testName =
                   Just "combine loop has no type-class dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'unboxedFftCombineLoop ''Boxed.MVector)
               { testName =
                   Just "combine loop has no boxed-vector backing"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'unboxedFftCombineLoop
                 [ 'combineLoop
                 , 'Vector.unsafeGet
                 , 'Vector.unsafeWrite
                 , 'Generic.unsafeRead
                 , 'Generic.unsafeWrite
                 , 'Generic.basicUnsafeRead
                 , 'Generic.basicUnsafeWrite
                 ]
             )
               { testName =
                   Just
                     "combine loop contains no listed generic-vector operations"
               }
           )
       )
    ]
