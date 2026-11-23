{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 #-}

module PureBorrow.Inspection.CopyNonlinearField (
  test_copyNonlinearField,
  Counted (..),
  copyCounted,
  WideCounted (..),
  copyWideCounted,
) where

import Control.Monad.Borrow.Pure.BO (Share)
import Control.Monad.Borrow.Pure.Copyable (Copyable (copy))
import GHC.Exts qualified as GHC
import Generics.Linear
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

-- The obligations are about the instances derived here, so the types share this module.
-- A copy of a nonlinear String would keep the dictionary of Copyable String.
data Counted where
  Counted :: String -> Int %1 -> Counted

instance Generic Counted where
  type
    Rep Counted =
      D1 GHC.Any (C1 GHC.Any (S1 GHC.Any (MP1 'GHC.Many (K1 R String)) :*: S1 GHC.Any (K1 R Int)))
  {-# INLINE [1] from #-}
  from counted = fromCounted counted
    where
      fromCounted :: Counted %1 -> Rep Counted x
      fromCounted (Counted label n) = M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (K1 n)))
  {-# INLINE [1] to #-}
  to rep = toCounted rep
    where
      toCounted :: Rep Counted x %1 -> Counted
      toCounted (M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (K1 n)))) = Counted label n

deriving via Generically Counted instance Copyable Counted

copyCounted :: Share α Counted %1 -> Counted
{-# NOINLINE copyCounted #-}
copyCounted = copy

-- Twelve fields, six of them nonlinear: wide enough for the generic representation to survive unless everything inlines.
data WideCounted where
  WideCounted ::
    String ->
    String ->
    String ->
    String ->
    String ->
    String ->
    Int %1 ->
    Int %1 ->
    Int %1 ->
    Int %1 ->
    Int %1 ->
    Int %1 ->
    WideCounted

type Shared = S1 GHC.Any (MP1 'GHC.Many (K1 R String))

type Counter = S1 GHC.Any (K1 R Int)

instance Generic WideCounted where
  type
    Rep WideCounted =
      D1
        GHC.Any
        ( C1
            GHC.Any
            ( Shared
                :*: Shared
                :*: Shared
                :*: Shared
                :*: Shared
                :*: Shared
                :*: Counter
                :*: Counter
                :*: Counter
                :*: Counter
                :*: Counter
                :*: Counter
            )
        )
  {-# INLINE [1] from #-}
  from wide = fromWide wide
    where
      fromWide :: WideCounted %1 -> Rep WideCounted x
      fromWide (WideCounted s1 s2 s3 s4 s5 s6 n1 n2 n3 n4 n5 n6) =
        M1
          ( M1
              ( M1 (MP1 (K1 s1))
                  :*: M1 (MP1 (K1 s2))
                  :*: M1 (MP1 (K1 s3))
                  :*: M1 (MP1 (K1 s4))
                  :*: M1 (MP1 (K1 s5))
                  :*: M1 (MP1 (K1 s6))
                  :*: M1 (K1 n1)
                  :*: M1 (K1 n2)
                  :*: M1 (K1 n3)
                  :*: M1 (K1 n4)
                  :*: M1 (K1 n5)
                  :*: M1 (K1 n6)
              )
          )
  {-# INLINE [1] to #-}
  to rep = toWide rep
    where
      toWide :: Rep WideCounted x %1 -> WideCounted
      toWide
        ( M1
            ( M1
                ( M1 (MP1 (K1 s1))
                    :*: M1 (MP1 (K1 s2))
                    :*: M1 (MP1 (K1 s3))
                    :*: M1 (MP1 (K1 s4))
                    :*: M1 (MP1 (K1 s5))
                    :*: M1 (MP1 (K1 s6))
                    :*: M1 (K1 n1)
                    :*: M1 (K1 n2)
                    :*: M1 (K1 n3)
                    :*: M1 (K1 n4)
                    :*: M1 (K1 n5)
                    :*: M1 (K1 n6)
                  )
              )
          ) = WideCounted s1 s2 s3 s4 s5 s6 n1 n2 n3 n4 n5 n6

deriving via Generically WideCounted instance Copyable WideCounted

copyWideCounted :: Share α WideCounted %1 -> WideCounted
{-# NOINLINE copyWideCounted #-}
copyWideCounted = copy

test_copyNonlinearField :: TestTree
test_copyNonlinearField =
  testGroup
    "copy over a nonlinear field"
    [ $(inspectTest ((hasNoGenerics 'copyCounted) {testName = Just "copy leaves no generic representation"}))
    , $(inspectTest ((hasNoType 'copyCounted ''MP1) {testName = Just "copy leaves no multiplicity wrapper"}))
    , $(inspectTest ((hasNoTypeClasses 'copyCounted) {testName = Just "copy leaves no dictionary"}))
    , $(inspectTest ((hasNoGenerics 'copyWideCounted) {testName = Just "copy of twelve fields leaves no generic representation"}))
    , $(inspectTest ((hasNoType 'copyWideCounted ''MP1) {testName = Just "copy of twelve fields leaves no multiplicity wrapper"}))
    , $(inspectTest ((hasNoTypeClasses 'copyWideCounted) {testName = Just "copy of twelve fields leaves no dictionary"}))
    ]
