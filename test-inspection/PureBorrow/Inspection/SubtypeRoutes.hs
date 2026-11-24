{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -dsuppress-coercions #-}
{-# OPTIONS_GHC -dsuppress-idinfo #-}
{-# OPTIONS_GHC -dsuppress-module-prefixes #-}
{-# OPTIONS_GHC -dsuppress-type-applications #-}
{-# OPTIONS_GHC -dsuppress-type-signatures #-}
{-# OPTIONS_GHC -dsuppress-uniques #-}

-- Each route of upcast and upcast1 compiles to the same Core as a cast, and the witnesses of the instances are constants.
module PureBorrow.Inspection.SubtypeRoutes (
  test_subtypeRoutes,
  witnessShare,
  refWitnessShare,
  witnessNonEmpty,
  refWitnessNonEmpty,
  witness1Share,
  refWitness1Share,
  SV (..),
  Eight (..),
  Eight1 (..),
  MP (..),
  NT (..),
  shareConc,
  refShareConc,
  sharePoly,
  refSharePoly,
  boPoly,
  refBoPoly,
  svParam,
  refSvParam,
  svParamPoly,
  refSvParamPoly,
  neShares1,
  refNeShares1,
  funMult,
  refFunMult,
  funContra1,
  refFunContra1,
  eight,
  refEight,
  eight1,
  refEight1,
  mpPoly,
  refMpPoly,
  ntPoly,
  refNtPoly,
) where

import Control.Monad.Borrow.Pure (BO, Mut, Share, Static, upcast, type (<:), type (>=))
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Data.Coerce.Directed (Subtype1, upcast1)
import Data.Coerce.Directed.Unsafe (SubtypeWitness (..), subtype, subtype1)
import Data.List.NonEmpty (NonEmpty)
import GHC.Exts (FUN, Multiplicity (..))
import Generics.Linear qualified as GL
import Generics.Linear.TH (deriveGeneric, deriveGenericAnd1)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Linear qualified as Unsafe

data SV α a = SV (Share α a) Int

deriveGenericAnd1 ''SV

deriving via GL.Generically1 (SV β) instance (α >= β) => Subtype1 (SV α) (SV β)

deriving via GL.Generically1 (SV β) b instance (α >= β, a <: b) => SV α a <: SV β b

shareConc :: Share Static Int %1 -> Share (Al 0) Int
{-# NOINLINE shareConc #-}
shareConc = upcast

refShareConc :: Share Static Int %1 -> Share (Al 0) Int
{-# NOINLINE refShareConc #-}
refShareConc = Unsafe.toLinear unsafeCoerce

sharePoly :: (α >= β) => Share α a %1 -> Share β a
{-# NOINLINE sharePoly #-}
sharePoly = upcast

refSharePoly :: (α >= β) => Share α a %1 -> Share β a
{-# NOINLINE refSharePoly #-}
refSharePoly = Unsafe.toLinear unsafeCoerce

boPoly :: (α >= β) => BO α (Share α a) %1 -> BO β (Share β a)
{-# NOINLINE boPoly #-}
boPoly = upcast

refBoPoly :: (α >= β) => BO α (Share α a) %1 -> BO β (Share β a)
{-# NOINLINE refBoPoly #-}
refBoPoly = Unsafe.toLinear unsafeCoerce

svParam :: SV Static (Share Static Int) %1 -> SV Static (Share (Al 0) Int)
{-# NOINLINE svParam #-}
svParam = upcast

refSvParam :: SV Static (Share Static Int) %1 -> SV Static (Share (Al 0) Int)
{-# NOINLINE refSvParam #-}
refSvParam = Unsafe.toLinear unsafeCoerce

svParamPoly :: (α >= β) => SV γ (Share α a) %1 -> SV γ (Share β a)
{-# NOINLINE svParamPoly #-}
svParamPoly = upcast

refSvParamPoly :: (α >= β) => SV γ (Share α a) %1 -> SV γ (Share β a)
{-# NOINLINE refSvParamPoly #-}
refSvParamPoly = Unsafe.toLinear unsafeCoerce

neShares1 :: (α >= β) => NonEmpty [Share α a] %1 -> NonEmpty [Share β a]
{-# NOINLINE neShares1 #-}
neShares1 = upcast1

refNeShares1 :: (α >= β) => NonEmpty [Share α a] %1 -> NonEmpty [Share β a]
{-# NOINLINE refNeShares1 #-}
refNeShares1 = Unsafe.toLinear unsafeCoerce

data Eight a1 a2 a3 a4 a5 a6 a7 x = Eight a1 a2 a3 a4 a5 a6 a7 x

deriveGeneric ''Eight

deriving via GL.Generically (Eight b1 a2 a3 a4 a5 a6 a7 y) instance (a1 <: b1, x <: y) => Eight a1 a2 a3 a4 a5 a6 a7 x <: Eight b1 a2 a3 a4 a5 a6 a7 y

data Eight1 a1 a2 a3 a4 a5 a6 a7 a8 x = Eight1 a1 a2 a3 a4 a5 a6 a7 a8 x

deriveGenericAnd1 ''Eight1

deriving via GL.Generically1 (Eight1 b1 a2 a3 a4 a5 a6 a7 a8) instance (a1 <: b1) => Subtype1 (Eight1 a1 a2 a3 a4 a5 a6 a7 a8) (Eight1 b1 a2 a3 a4 a5 a6 a7 a8)

data MP α a = MP (Mut α a) (Share α a) | MPNil

deriveGeneric ''MP

deriving via GL.Generically (MP β b) instance (α >= β, a <: b, b <: a) => MP α a <: MP β b

data NT α a = NTLeaf (Share α a) | NTNode (NonEmpty (NT α a))

deriveGenericAnd1 ''NT

deriving via GL.Generically1 (NT β) instance (α >= β) => Subtype1 (NT α) (NT β)

deriving via GL.Generically1 (NT β) b instance (α >= β, a <: b) => NT α a <: NT β b

funMult :: (Int %1 -> Int) %1 -> (Int -> Int)
{-# NOINLINE funMult #-}
funMult = upcast

refFunMult :: (Int %1 -> Int) %1 -> (Int -> Int)
{-# NOINLINE refFunMult #-}
refFunMult = Unsafe.toLinear unsafeCoerce

funContra1 :: (α >= β) => FUN 'One (Share β Int) Int %1 -> FUN 'Many (Share α Int) Int
{-# NOINLINE funContra1 #-}
funContra1 = upcast1

refFunContra1 :: (α >= β) => FUN 'One (Share β Int) Int %1 -> FUN 'Many (Share α Int) Int
{-# NOINLINE refFunContra1 #-}
refFunContra1 = Unsafe.toLinear unsafeCoerce

eight :: Eight (Int %1 -> Int) Int Int Int Int Int Int (Share Static Int) %1 -> Eight (Int -> Int) Int Int Int Int Int Int (Share (Al 0) Int)
{-# NOINLINE eight #-}
eight = upcast

refEight :: Eight (Int %1 -> Int) Int Int Int Int Int Int (Share Static Int) %1 -> Eight (Int -> Int) Int Int Int Int Int Int (Share (Al 0) Int)
{-# NOINLINE refEight #-}
refEight = Unsafe.toLinear unsafeCoerce

eight1 :: Eight1 (Int %1 -> Int) Int Int Int Int Int Int Int (Share Static Int) %1 -> Eight1 (Int -> Int) Int Int Int Int Int Int Int (Share (Al 0) Int)
{-# NOINLINE eight1 #-}
eight1 = upcast1

refEight1 :: Eight1 (Int %1 -> Int) Int Int Int Int Int Int Int (Share Static Int) %1 -> Eight1 (Int -> Int) Int Int Int Int Int Int Int (Share (Al 0) Int)
{-# NOINLINE refEight1 #-}
refEight1 = Unsafe.toLinear unsafeCoerce

mpPoly :: (α >= β) => MP α Int %1 -> MP β Int
{-# NOINLINE mpPoly #-}
mpPoly = upcast

refMpPoly :: (α >= β) => MP α Int %1 -> MP β Int
{-# NOINLINE refMpPoly #-}
refMpPoly = Unsafe.toLinear unsafeCoerce

ntPoly :: (α >= β) => NT α (Share α Int) %1 -> NT β (Share β Int)
{-# NOINLINE ntPoly #-}
ntPoly = upcast

refNtPoly :: (α >= β) => NT α (Share α Int) %1 -> NT β (Share β Int)
{-# NOINLINE refNtPoly #-}
refNtPoly = Unsafe.toLinear unsafeCoerce

witnessShare :: (α >= β) => SubtypeWitness (Share α a) (Share β a)
{-# NOINLINE witnessShare #-}
witnessShare = subtype

refWitnessShare :: (α >= β) => SubtypeWitness (Share α a) (Share β a)
{-# NOINLINE refWitnessShare #-}
refWitnessShare = UnsafeSubtype

witnessNonEmpty :: (α >= β) => SubtypeWitness (NonEmpty [Share α a]) (NonEmpty [Share β a])
{-# NOINLINE witnessNonEmpty #-}
witnessNonEmpty = subtype

refWitnessNonEmpty :: (α >= β) => SubtypeWitness (NonEmpty [Share α a]) (NonEmpty [Share β a])
{-# NOINLINE refWitnessNonEmpty #-}
refWitnessNonEmpty = UnsafeSubtype

witness1Share :: (α >= β) => SubtypeWitness a b -> SubtypeWitness (Share α a) (Share β b)
{-# NOINLINE witness1Share #-}
witness1Share = subtype1

refWitness1Share :: (α >= β) => SubtypeWitness a b -> SubtypeWitness (Share α a) (Share β b)
{-# NOINLINE refWitness1Share #-}
refWitness1Share _ = UnsafeSubtype

test_subtypeRoutes :: TestTree
test_subtypeRoutes =
  testGroup
    "every route of upcast is a cast"
    [ $(inspectTest (('shareConc ==- 'refShareConc) {testName = Just "upcast Share Static to Share (Al 0) is a pure cast"}))
    , $(inspectTest (('sharePoly ==- 'refSharePoly) {testName = Just "polymorphic upcast Share is a pure cast"}))
    , $(inspectTest (('boPoly ==- 'refBoPoly) {testName = Just "polymorphic upcast BO of Share is a pure cast"}))
    , $(inspectTest (('svParam ==- 'refSvParam) {testName = Just "upcast SV over a Share parameter is a pure cast"}))
    , $(inspectTest (('svParamPoly ==- 'refSvParamPoly) {testName = Just "polymorphic upcast SV over a Share parameter is a pure cast"}))
    , $(inspectTest (('neShares1 ==- 'refNeShares1) {testName = Just "polymorphic upcast1 NonEmpty [Share] is a pure cast"}))
    , $(inspectTest (('funMult ==- 'refFunMult) {testName = Just "upcast FUN multiplicity is a pure cast"}))
    , $(inspectTest (('funContra1 ==- 'refFunContra1) {testName = Just "polymorphic upcast1 FUN contravariant argument is a pure cast"}))
    , $(inspectTest (('eight ==- 'refEight) {testName = Just "upcast through the arity-8 Generically head is a pure cast"}))
    , $(inspectTest (('eight1 ==- 'refEight1) {testName = Just "upcast1 through the arity-8 Generically1 head is a pure cast"}))
    , $(inspectTest (('mpPoly ==- 'refMpPoly) {testName = Just "polymorphic upcast via Generically with Mut is a pure cast"}))
    , $(inspectTest (('ntPoly ==- 'refNtPoly) {testName = Just "polymorphic upcast of a NonEmpty-recursive type via Generically1 is a pure cast"}))
    , $(inspectTest (('witnessShare ==- 'refWitnessShare) {testName = Just "the witness of a trusted instance is a constant"}))
    , $(inspectTest (('witnessNonEmpty ==- 'refWitnessNonEmpty) {testName = Just "the witness of a type recursive through a list is a constant"}))
    , $(inspectTest (('witness1Share ==- 'refWitness1Share) {testName = Just "the witness of a trusted Subtype1 instance ignores its argument"}))
    ]
