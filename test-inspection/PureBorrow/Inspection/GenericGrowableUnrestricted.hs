{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 #-}

module PureBorrow.Inspection.GenericGrowableUnrestricted (
  tests,
  boxedPush,
  unboxedPush,
) where

import Control.Monad.Borrow.Pure.BO (BO, Mut)
import Data.Vector qualified as Boxed
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as Growable
import Data.Vector.Mutable qualified as BoxedMutable
import Data.Vector.Unboxed qualified as Unboxed
import GHC.Base (IP)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

boxedPush ::
  Int ->
  Mut α (Growable.GrowableVector Boxed.Vector Int) %1 ->
  BO α (Mut α (Growable.GrowableVector Boxed.Vector Int))
{-# NOINLINE boxedPush #-}
boxedPush = Growable.push

unboxedPush ::
  Int ->
  Mut α (Growable.GrowableVector Unboxed.Vector Int) %1 ->
  BO α (Mut α (Growable.GrowableVector Unboxed.Vector Int))
{-# NOINLINE unboxedPush #-}
unboxedPush = Growable.push

tests :: TestTree
tests =
  testGroup
    "generic growable unrestricted vector"
    [ $( inspectTest
           ( (hasNoTypeClassesExcept 'boxedPush [''IP])
               { testName =
                   Just "boxed push retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClassesExcept 'unboxedPush [''IP])
               { testName =
                   Just "unboxed push retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'boxedPush ''Unboxed.MVector)
               { testName =
                   Just "boxed push has no unboxed backing"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'unboxedPush ''BoxedMutable.MVector)
               { testName =
                   Just "unboxed push has no boxed backing"
               }
           )
       )
    ]
