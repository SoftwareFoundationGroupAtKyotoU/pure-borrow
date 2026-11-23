{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 #-}

module PureBorrow.Inspection.SplitNonlinearField (
  test_splitNonlinearField,
  Labelled (..),
  splitLabelled,
  Wide (..),
  splitWide,
) where

import Control.Monad.Borrow.Pure.BO (DistributesAlias, Mut, split)
import GHC.Exts qualified as GHC
import Generics.Linear
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

-- The type, its instances and the probe share this module: the obligations
-- below are about the instance that is derived here.
data Labelled a where
  Labelled :: Int -> a %1 -> Labelled a

instance Generic1 Labelled where
  type
    Rep1 Labelled =
      D1 GHC.Any (C1 GHC.Any (S1 GHC.Any (MP1 'GHC.Many (K1 R Int)) :*: S1 GHC.Any Par1))
  {-# INLINE [1] from1 #-}
  from1 labelled = fromLabelled labelled
    where
      fromLabelled :: Labelled a %1 -> Rep1 Labelled a
      fromLabelled (Labelled label a) = M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (Par1 a)))
  {-# INLINE [1] to1 #-}
  to1 rep = toLabelled rep
    where
      toLabelled :: Rep1 Labelled a %1 -> Labelled a
      toLabelled (M1 (M1 (M1 (MP1 (K1 label)) :*: M1 (Par1 a)))) = Labelled label a

deriving anyclass instance DistributesAlias Labelled

splitLabelled :: Mut α (Labelled a) %1 -> Labelled (Mut α a)
{-# NOINLINE splitLabelled #-}
splitLabelled = split

-- Twelve fields, six of them nonlinear and lazy: wide enough for the generic
-- representation to survive unless the instance methods, 'from1' and 'to1' all
-- inline.
data Wide a where
  Wide ::
    Int ->
    Int ->
    Int ->
    Int ->
    Int ->
    Int ->
    a %1 ->
    a %1 ->
    a %1 ->
    a %1 ->
    a %1 ->
    a %1 ->
    Wide a

type Kept = S1 GHC.Any (MP1 'GHC.Many (K1 R Int))

type Element = S1 GHC.Any Par1

instance Generic1 Wide where
  type
    Rep1 Wide =
      D1
        GHC.Any
        ( C1
            GHC.Any
            ( Kept
                :*: Kept
                :*: Kept
                :*: Kept
                :*: Kept
                :*: Kept
                :*: Element
                :*: Element
                :*: Element
                :*: Element
                :*: Element
                :*: Element
            )
        )
  {-# INLINE [1] from1 #-}
  from1 wide = fromWide wide
    where
      fromWide :: Wide a %1 -> Rep1 Wide a
      fromWide (Wide n1 n2 n3 n4 n5 n6 a1 a2 a3 a4 a5 a6) =
        M1
          ( M1
              ( M1 (MP1 (K1 n1))
                  :*: M1 (MP1 (K1 n2))
                  :*: M1 (MP1 (K1 n3))
                  :*: M1 (MP1 (K1 n4))
                  :*: M1 (MP1 (K1 n5))
                  :*: M1 (MP1 (K1 n6))
                  :*: M1 (Par1 a1)
                  :*: M1 (Par1 a2)
                  :*: M1 (Par1 a3)
                  :*: M1 (Par1 a4)
                  :*: M1 (Par1 a5)
                  :*: M1 (Par1 a6)
              )
          )
  {-# INLINE [1] to1 #-}
  to1 rep = toWide rep
    where
      toWide :: Rep1 Wide a %1 -> Wide a
      toWide
        ( M1
            ( M1
                ( M1 (MP1 (K1 n1))
                    :*: M1 (MP1 (K1 n2))
                    :*: M1 (MP1 (K1 n3))
                    :*: M1 (MP1 (K1 n4))
                    :*: M1 (MP1 (K1 n5))
                    :*: M1 (MP1 (K1 n6))
                    :*: M1 (Par1 a1)
                    :*: M1 (Par1 a2)
                    :*: M1 (Par1 a3)
                    :*: M1 (Par1 a4)
                    :*: M1 (Par1 a5)
                    :*: M1 (Par1 a6)
                  )
              )
          ) = Wide n1 n2 n3 n4 n5 n6 a1 a2 a3 a4 a5 a6

deriving anyclass instance DistributesAlias Wide

splitWide :: Mut α (Wide a) %1 -> Wide (Mut α a)
{-# NOINLINE splitWide #-}
splitWide = split

test_splitNonlinearField :: TestTree
test_splitNonlinearField =
  testGroup
    "split over a nonlinear field"
    [ $( inspectTest
           ( (hasNoGenerics 'splitLabelled)
               { testName =
                   Just "split leaves no generic representation"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'splitLabelled ''MP1)
               { testName =
                   Just "split leaves no multiplicity wrapper"
               }
           )
       )
    , $( inspectTest
           ( (hasNoTypeClasses 'splitLabelled)
               { testName =
                   Just "split leaves no dictionary"
               }
           )
       )
    , $( inspectTest
           ( (hasNoGenerics 'splitWide)
               { testName =
                   Just "split of twelve fields leaves no generic representation"
               }
           )
       )
    , $( inspectTest
           ( (hasNoType 'splitWide ''MP1)
               { testName =
                   Just "split of twelve fields leaves no multiplicity wrapper"
               }
           )
       )
    ]
