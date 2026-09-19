{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PureBorrow.Inspection.QSort (
  tests,
  unboxedQsort,
  unboxedBioQsort,
) where

import Control.Concurrent.DivideConquer.Linear (qsort)
import Control.Monad.Borrow.BO (BIO, BO, Mut)
import Data.Vector.Generic qualified as GenericVector
import Data.Vector.Generic.Mutable qualified as Generic
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Vector
import Data.Vector.Mutable qualified as Boxed
import Data.Vector.Unboxed qualified as Unboxed
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

{-# SPECIALIZE qsort ::
  Word ->
  Mut α (Vector.Vector Unboxed.Vector Int) %1 ->
  BO α ()
  #-}

unboxedQsort ::
  Word ->
  Mut α (Vector.Vector Unboxed.Vector Int) %1 ->
  BO α ()
{-# NOINLINE unboxedQsort #-}
unboxedQsort = qsort

{-# SPECIALIZE qsort ::
  Word ->
  Mut α (Vector.Vector Unboxed.Vector Int) %1 ->
  BIO α ()
  #-}

unboxedBioQsort ::
  Word ->
  Mut α (Vector.Vector Unboxed.Vector Int) %1 ->
  BIO α ()
{-# NOINLINE unboxedBioQsort #-}
unboxedBioQsort = qsort

tests :: TestTree
tests =
  testGroup
    "qsort"
    [ $( inspectTest
           ( ( hasNoTypeClassesExcept
                 'unboxedQsort
                 [''GenericVector.Vector, ''Ord]
             )
               { testName =
                   Just
                     "specialization retains only Vector and Ord dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'unboxedQsort ''Boxed.MVector)
               { testName =
                   Just "root has no boxed-vector backing"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'unboxedQsort
                 [ 'qsort
                 , 'Vector.unsafeGet
                 , 'Vector.unsafeSwap
                 , 'Generic.unsafeRead
                 , 'Generic.unsafeSwap
                 , 'Generic.unsafeWrite
                 , 'Generic.basicUnsafeRead
                 , 'Generic.basicUnsafeWrite
                 ]
             )
               { testName =
                   Just "root contains no listed generic-vector operations"
               }
           )
       )
    , $( inspectTest
           ( ( hasNoTypeClassesExcept
                 'unboxedBioQsort
                 [''GenericVector.Vector, ''Ord]
             )
               { testName =
                   Just
                     "BIO specialization retains only Vector and Ord dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'unboxedBioQsort ''Boxed.MVector)
               { testName =
                   Just "BIO root has no boxed-vector backing"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'unboxedBioQsort
                 [ 'qsort
                 , 'Vector.unsafeGet
                 , 'Vector.unsafeSwap
                 , 'Generic.unsafeRead
                 , 'Generic.unsafeSwap
                 , 'Generic.unsafeWrite
                 , 'Generic.basicUnsafeRead
                 , 'Generic.basicUnsafeWrite
                 ]
             )
               { testName =
                   Just "BIO root contains no listed generic-vector operations"
               }
           )
       )
    ]
