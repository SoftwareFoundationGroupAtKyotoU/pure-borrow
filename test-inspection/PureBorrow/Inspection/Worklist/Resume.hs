{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PureBorrow.Inspection.Worklist.Resume (
  tests,
  resumeEdgeWorker,
  openOnceEdgeWorker,
) where

import Control.Monad.Borrow.Pure.BO (BO, Borrow, Mut)
import Control.Monad.Borrow.Pure.Experimental.Borrows (Aliases, reborrowings)
import Control.Monad.Borrow.Pure.Lifetime (type (>=))
import Data.Int (Int64)
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as Growable
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Fixed
import Data.Vector.Unboxed qualified as U
import Prelude.Linear
import PureBorrow.Internal.Bench.Worklist.Resume
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

resumeEdgeWorker ::
  (α >= δ, β >= δ, γ >= δ) =>
  Int ->
  Int ->
  Int ->
  Int ->
  [Int] ->
  Int64 ->
  Borrow bk1 α (Fixed.Vector U.Vector Int) %1 ->
  Borrow bk2 β (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut γ (Fixed.Vector U.Vector Int) %1 ->
  BO
    δ
    ( Ur (Int, Int, [Int], Int64)
    , Borrow bk1 α (Fixed.Vector U.Vector Int)
    , Borrow bk2 β (Fixed.Vector V.Vector (Int, Int))
    , Mut γ (Fixed.Vector U.Vector Int)
    )
{-# NOINLINE resumeEdgeWorker #-}
resumeEdgeWorker = worklistPureBorrowResumeEdgeWorker

openOnceEdgeWorker ::
  Int ->
  Int ->
  Int ->
  Int ->
  Int64 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO
    α
    ( Ur (Int, Int, Int64)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector V.Vector (Int, Int))
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    )
{-# NOINLINE openOnceEdgeWorker #-}
openOnceEdgeWorker = worklistPureBorrowOpenOnceEdgeWorker

tests :: TestTree
tests =
  testGroup
    "worklist resume"
    [ $( inspectTest
           ( (hasNoTypeClasses 'resumeEdgeWorker)
               { testName =
                   Just "resume edge worker has no type-class dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClasses 'openOnceEdgeWorker)
               { testName =
                   Just "open-once edge worker has no type-class dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'resumeEdgeWorker ''Growable.GrowableVector)
               { testName =
                   Just "resume edge worker has no growable header"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'resumeEdgeWorker ''Aliases)
               { testName =
                   Just "resume edge worker has no plural-borrow bundle"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'resumeEdgeWorker
                 [ 'worklistPureBorrowResumeEdgeWorker
                 , 'Fixed.unsafeGet
                 , 'Fixed.unsafeWrite
                 , 'Growable.getContents
                 , 'Growable.extend
                 , 'reborrowings
                 ]
             )
               { testName =
                   Just "resume edge worker contains only specialized backing access"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'openOnceEdgeWorker
                 [ 'worklistPureBorrowOpenOnceEdgeWorker
                 , 'Fixed.unsafeGet
                 , 'Fixed.unsafeWrite
                 , 'Growable.getContents
                 , 'Growable.extend
                 , 'reborrowings
                 ]
             )
               { testName =
                   Just "open-once edge worker contains only specialized backing access"
               }
           )
       )
    ]
