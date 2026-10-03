{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 #-}

{- | Header access of the growable vectors, at a concrete element type.

Every header read and write goes through the state-threaded primitives of "Data.Ref.Linear.Internal", which must inline away completely, and so must the pure, 'GHC.noinline'-wrapped read they replaced.
-}
module PureBorrow.Inspection.GrowableAccess (
  tests,
  boxedCopyAt,
  boxedSet,
  boxedPush,
  unboxedCopyAt,
  unboxedSet,
  unboxedPush,
  genericCopyAt,
  genericSet,
  genericPush,
) where

import Control.Monad.Borrow.Pure.BO (BO, Mut, Share)
import Data.Ref.Linear.Internal qualified as Ref
import Data.Ref.Linear.Unlifted.Internal (freeRef#, unsafeReadRef#, unsafeReadRefIO#, unsafeWriteRef#, unsafeWriteRefIO#)
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as GenericGrowable
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted.Internal qualified as GenericGrowableInternal
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Growable
import Data.Vector.Mutable.Growable.Linear.Borrow.Internal qualified as GrowableInternal
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow qualified as UnboxedGrowable
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow.Internal qualified as UnboxedGrowableInternal
import GHC.Base (IP)
import GHC.Exts qualified as GHC
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

boxedCopyAt :: Int -> Share α (Growable.GrowableVector Int) -> BO α (Ur Int)
{-# NOINLINE boxedCopyAt #-}
boxedCopyAt = Growable.copyAt

boxedSet ::
  Int ->
  Int %1 ->
  Mut α (Growable.GrowableVector Int) %1 ->
  BO α (Int, Mut α (Growable.GrowableVector Int))
{-# NOINLINE boxedSet #-}
boxedSet = Growable.set

boxedPush ::
  Int %1 ->
  Mut α (Growable.GrowableVector Int) %1 ->
  BO α (Mut α (Growable.GrowableVector Int))
{-# NOINLINE boxedPush #-}
boxedPush = Growable.push

unboxedCopyAt :: Int -> Share α (UnboxedGrowable.GrowableVector Int) -> BO α (Ur Int)
{-# NOINLINE unboxedCopyAt #-}
unboxedCopyAt = UnboxedGrowable.copyAt

unboxedSet ::
  Int ->
  Int %1 ->
  Mut α (UnboxedGrowable.GrowableVector Int) %1 ->
  BO α (Int, Mut α (UnboxedGrowable.GrowableVector Int))
{-# NOINLINE unboxedSet #-}
unboxedSet = UnboxedGrowable.set

unboxedPush ::
  Int %1 ->
  Mut α (UnboxedGrowable.GrowableVector Int) %1 ->
  BO α (Mut α (UnboxedGrowable.GrowableVector Int))
{-# NOINLINE unboxedPush #-}
unboxedPush = UnboxedGrowable.push

genericCopyAt :: Int -> Share α (GenericGrowable.GrowableVector U.Vector Int) -> BO α (Ur Int)
{-# NOINLINE genericCopyAt #-}
genericCopyAt = GenericGrowable.copyAt

genericSet ::
  Int ->
  Int ->
  Mut α (GenericGrowable.GrowableVector U.Vector Int) %1 ->
  BO α (Ur Int, Mut α (GenericGrowable.GrowableVector U.Vector Int))
{-# NOINLINE genericSet #-}
genericSet = GenericGrowable.set

genericPush ::
  Int ->
  Mut α (GenericGrowable.GrowableVector U.Vector Int) %1 ->
  BO α (Mut α (GenericGrowable.GrowableVector U.Vector Int))
{-# NOINLINE genericPush #-}
genericPush = GenericGrowable.push

tests :: TestTree
tests =
  testGroup
    "growable header access"
    [ $( inspectTest
           ( (hasNoTypeClassesExcept 'boxedCopyAt [''IP])
               { testName = Just "boxed copyAt retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClassesExcept 'boxedSet [''IP])
               { testName = Just "boxed set retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClassesExcept 'boxedPush [''IP])
               { testName = Just "boxed push retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClassesExcept 'unboxedCopyAt [''IP])
               { testName = Just "unboxed copyAt retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClassesExcept 'unboxedSet [''IP])
               { testName = Just "unboxed set retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClassesExcept 'unboxedPush [''IP])
               { testName = Just "unboxed push retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'boxedCopyAt
                 [ 'Growable.copyAt
                 , 'GrowableInternal.readHeader
                 , 'Ref.unsafeReadRefBO
                 , 'unsafeReadRefIO#
                 , 'unsafeReadRef#
                 , 'freeRef#
                 , 'GHC.runRW#
                 , 'GHC.noinline
                 ]
             )
               { testName = Just "boxed copyAt inlines its header read"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'boxedSet
                 [ 'Growable.set
                 , 'GrowableInternal.readHeader
                 , 'Ref.unsafeReadRefBO
                 , 'unsafeReadRefIO#
                 , 'unsafeReadRef#
                 , 'freeRef#
                 , 'GHC.runRW#
                 , 'unsafeWriteRef#
                 , 'GHC.noinline
                 ]
             )
               { testName = Just "boxed set inlines its header read"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'boxedPush
                 [ 'Growable.push
                 , 'GrowableInternal.withHeader
                 , 'Ref.unsafeReadRefBO
                 , 'Ref.unsafeWriteRefBO
                 , 'unsafeReadRefIO#
                 , 'unsafeWriteRefIO#
                 , 'unsafeReadRef#
                 , 'freeRef#
                 , 'GHC.runRW#
                 , 'unsafeWriteRef#
                 , 'GHC.noinline
                 ]
             )
               { testName = Just "boxed push inlines its header read and write"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'unboxedCopyAt
                 [ 'UnboxedGrowable.copyAt
                 , 'UnboxedGrowableInternal.readHeader
                 , 'Ref.unsafeReadRefBO
                 , 'unsafeReadRefIO#
                 , 'unsafeReadRef#
                 , 'freeRef#
                 , 'GHC.runRW#
                 , 'GHC.noinline
                 ]
             )
               { testName = Just "unboxed copyAt inlines its header read"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'unboxedSet
                 [ 'UnboxedGrowable.set
                 , 'UnboxedGrowableInternal.readHeader
                 , 'Ref.unsafeReadRefBO
                 , 'unsafeReadRefIO#
                 , 'unsafeReadRef#
                 , 'freeRef#
                 , 'GHC.runRW#
                 , 'unsafeWriteRef#
                 , 'GHC.noinline
                 ]
             )
               { testName = Just "unboxed set inlines its header read"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'unboxedPush
                 [ 'UnboxedGrowable.push
                 , 'UnboxedGrowableInternal.withHeader
                 , 'Ref.unsafeReadRefBO
                 , 'Ref.unsafeWriteRefBO
                 , 'unsafeReadRefIO#
                 , 'unsafeWriteRefIO#
                 , 'unsafeReadRef#
                 , 'freeRef#
                 , 'GHC.runRW#
                 , 'unsafeWriteRef#
                 , 'GHC.noinline
                 ]
             )
               { testName = Just "unboxed push inlines its header read and write"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClassesExcept 'genericCopyAt [''IP])
               { testName = Just "generic copyAt retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClassesExcept 'genericSet [''IP])
               { testName = Just "generic set retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClassesExcept 'genericPush [''IP])
               { testName = Just "generic push retains only CallStack dictionaries"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'genericCopyAt
                 [ 'GenericGrowable.copyAt
                 , 'GenericGrowableInternal.readHeader
                 , 'Ref.unsafeReadRefBO
                 , 'unsafeReadRefIO#
                 , 'unsafeReadRef#
                 , 'freeRef#
                 , 'GHC.runRW#
                 , 'GHC.noinline
                 ]
             )
               { testName = Just "generic copyAt inlines its header read"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'genericSet
                 [ 'GenericGrowable.set
                 , 'GenericGrowableInternal.readHeader
                 , 'Ref.unsafeReadRefBO
                 , 'unsafeReadRefIO#
                 , 'unsafeReadRef#
                 , 'unsafeWriteRef#
                 , 'freeRef#
                 , 'GHC.runRW#
                 , 'GHC.noinline
                 ]
             )
               { testName = Just "generic set inlines its header read"
               }
           )
       )
    , $( inspectTest
           ( ( doesNotUseAnyOf
                 'genericPush
                 [ 'GenericGrowable.push
                 , 'GenericGrowableInternal.withHeader
                 , 'Ref.unsafeReadRefBO
                 , 'Ref.unsafeWriteRefBO
                 , 'unsafeReadRefIO#
                 , 'unsafeWriteRefIO#
                 , 'unsafeReadRef#
                 , 'unsafeWriteRef#
                 , 'freeRef#
                 , 'GHC.runRW#
                 , 'GHC.noinline
                 ]
             )
               { testName = Just "generic push inlines its header read and write"
               }
           )
       )
    ]
