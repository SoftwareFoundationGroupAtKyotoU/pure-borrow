{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PureBorrow.Inspection.MultiStoreScan (tests) where

import Control.Monad.Borrow.Pure.BO (BO, Mut)
import Control.Monad.Borrow.Pure.Experimental.Borrows (
  Aliases,
 )
import Data.Int (Int64)
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as Growable
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Fixed
import Data.Vector.Unboxed qualified as U
import Prelude.Linear
import PureBorrow.Internal.Bench.MultiStoreScan
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

hotWorker ::
  Int ->
  Int ->
  Int ->
  Int64 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO
    α
    ( Ur Int64
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector V.Vector (Int, Int))
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    )
{-# NOINLINE hotWorker #-}
hotWorker = multiStoreScanPureBorrowWorker

boxedContentProjection ::
  Mut α (Growable.GrowableVector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int))
{-# NOINLINE boxedContentProjection #-}
boxedContentProjection = multiStoreScanBoxedContentProjection

unboxedContentProjection ::
  Mut α (Growable.GrowableVector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int)
{-# NOINLINE unboxedContentProjection #-}
unboxedContentProjection = multiStoreScanUnboxedContentProjection

tests :: TestTree
tests =
  testGroup
    "multi-store scan"
    [ $( inspectTest
           ( (hasNoTypeClasses 'hotWorker)
               { testName =
                   Just "hot worker has no type-class dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'hotWorker ''Growable.GrowableVector)
               { testName =
                   Just "hot worker has no growable header"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'hotWorker ''Aliases)
               { testName =
                   Just "hot worker has no plural-borrow bundle"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'hotWorker
                 [ 'multiStoreScanPureBorrowWorker
                 , 'Fixed.unsafeGet
                 , 'Fixed.unsafeWrite
                 , 'Growable.getContents
                 ]
             )
               { testName =
                   Just "hot worker contains no generic access or projection calls"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'boxedContentProjection
                 [ 'multiStoreScanBoxedContentProjection
                 , 'Growable.getContents
                 ]
             )
               { testName =
                   Just "boxed projection inlines getContents"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'unboxedContentProjection
                 [ 'multiStoreScanUnboxedContentProjection
                 , 'Growable.getContents
                 ]
             )
               { testName =
                   Just "unboxed projection inlines getContents"
               }
           )
       )
    ]
