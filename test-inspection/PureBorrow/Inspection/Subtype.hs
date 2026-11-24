{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
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

module PureBorrow.Inspection.Subtype (
  test_subtype,
  MutPair (..),
  ShareView (..),
  upcastMutPair,
  refUpcastMutPair,
  upcast1ShareView,
  refUpcast1ShareView,
  upcastShareView,
  refUpcastShareView,
  upcast1NestedList,
  refUpcast1NestedList,
  upcast1PolyShareView,
  refUpcast1PolyShareView,
  upcast1PolyFunctor,
  refUpcast1PolyFunctor,
) where

import Control.Monad.Borrow.Pure (Mut, Share, Static, upcast, type (<:), type (>=))
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Data.Coerce.Directed (Subtype1, upcast1)
import Generics.Linear qualified as GL
import Generics.Linear.TH (deriveGeneric, deriveGenericAnd1)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Linear qualified as Unsafe

-- Concrete and polymorphic upcast and upcast1 compile to casts, with no generic representation, dictionary, or call to from or to.

data MutPair α a = MutPair (Mut α a) (Share α a)

deriveGeneric ''MutPair

deriving via
  GL.Generically (MutPair β b)
  instance
    (α >= β, a <: b, b <: a) => MutPair α a <: MutPair β b

data ShareView α a = ShareView (Share α a) Int

deriveGenericAnd1 ''ShareView

deriving via
  GL.Generically1 (ShareView β)
  instance
    (α >= β) => Subtype1 (ShareView α) (ShareView β)

deriving via
  GL.Generically1 (ShareView β) b
  instance
    (α >= β, a <: b) => ShareView α a <: ShareView β b

upcastMutPair :: MutPair Static Int %1 -> MutPair (Al 0) Int
{-# NOINLINE upcastMutPair #-}
upcastMutPair = upcast

refUpcastMutPair :: MutPair Static Int %1 -> MutPair (Al 0) Int
{-# NOINLINE refUpcastMutPair #-}
refUpcastMutPair = Unsafe.toLinear unsafeCoerce

upcast1ShareView :: ShareView Static Int %1 -> ShareView (Al 0) Int
{-# NOINLINE upcast1ShareView #-}
upcast1ShareView = upcast1

refUpcast1ShareView :: ShareView Static Int %1 -> ShareView (Al 0) Int
{-# NOINLINE refUpcast1ShareView #-}
refUpcast1ShareView = Unsafe.toLinear unsafeCoerce

upcastShareView :: ShareView Static Int %1 -> ShareView (Al 0) Int
{-# NOINLINE upcastShareView #-}
upcastShareView = upcast

refUpcastShareView :: ShareView Static Int %1 -> ShareView (Al 0) Int
{-# NOINLINE refUpcastShareView #-}
refUpcastShareView = Unsafe.toLinear unsafeCoerce

upcast1NestedList :: [ShareView Static Int] %1 -> [ShareView (Al 0) Int]
{-# NOINLINE upcast1NestedList #-}
upcast1NestedList = upcast1

refUpcast1NestedList :: [ShareView Static Int] %1 -> [ShareView (Al 0) Int]
{-# NOINLINE refUpcast1NestedList #-}
refUpcast1NestedList = Unsafe.toLinear unsafeCoerce

upcast1PolyShareView :: forall α β a. (α >= β) => ShareView α a %1 -> ShareView β a
{-# NOINLINE upcast1PolyShareView #-}
upcast1PolyShareView = upcast1

refUpcast1PolyShareView :: forall α β a. (α >= β) => ShareView α a %1 -> ShareView β a
{-# NOINLINE refUpcast1PolyShareView #-}
refUpcast1PolyShareView = Unsafe.toLinear unsafeCoerce

upcast1PolyFunctor :: forall f g α β a. (Subtype1 f g, α >= β) => f (Share α a) %1 -> g (Share β a)
{-# NOINLINE upcast1PolyFunctor #-}
upcast1PolyFunctor = upcast1

refUpcast1PolyFunctor :: forall f g α β a. (Subtype1 f g, α >= β) => f (Share α a) %1 -> g (Share β a)
{-# NOINLINE refUpcast1PolyFunctor #-}
refUpcast1PolyFunctor = Unsafe.toLinear unsafeCoerce

test_subtype :: TestTree
test_subtype =
  testGroup
    "Directed subtyping via Generically and Generically1"
    [ $(inspectTest ((hasNoGenerics 'upcastMutPair) {testName = Just "upcast MutPair leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'upcastMutPair) {testName = Just "upcast MutPair leaves no dictionary"}))
    , $(inspectTest (('upcastMutPair ==- 'refUpcastMutPair) {testName = Just "upcast MutPair is a pure cast"}))
    , $(inspectTest (('upcastMutPair `doesNotUse` 'GL.from) {testName = Just "upcast MutPair does not call from"}))
    , $(inspectTest (('upcastMutPair `doesNotUse` 'GL.to) {testName = Just "upcast MutPair does not call to"}))
    , $(inspectTest ((hasNoGenerics 'upcast1ShareView) {testName = Just "upcast1 ShareView leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'upcast1ShareView) {testName = Just "upcast1 ShareView leaves no dictionary"}))
    , $(inspectTest (('upcast1ShareView ==- 'refUpcast1ShareView) {testName = Just "upcast1 ShareView is a pure cast"}))
    , $(inspectTest (('upcast1ShareView `doesNotUse` 'GL.from1) {testName = Just "upcast1 ShareView does not call from1"}))
    , $(inspectTest (('upcast1ShareView `doesNotUse` 'GL.to1) {testName = Just "upcast1 ShareView does not call to1"}))
    , $(inspectTest ((hasNoGenerics 'upcastShareView) {testName = Just "upcast ShareView leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'upcastShareView) {testName = Just "upcast ShareView leaves no dictionary"}))
    , $(inspectTest (('upcastShareView ==- 'refUpcastShareView) {testName = Just "upcast ShareView is a pure cast"}))
    , $(inspectTest (('upcastShareView `doesNotUse` 'GL.from1) {testName = Just "upcast ShareView does not call from1"}))
    , $(inspectTest ((hasNoGenerics 'upcast1NestedList) {testName = Just "upcast1 [ShareView] leaves no generic representation"}))
    , $(inspectTest ((hasNoTypeClasses 'upcast1NestedList) {testName = Just "upcast1 [ShareView] leaves no dictionary"}))
    , $(inspectTest (('upcast1NestedList ==- 'refUpcast1NestedList) {testName = Just "upcast1 [ShareView] is a pure cast"}))
    , $(inspectTest (('upcast1NestedList `doesNotUse` 'GL.from1) {testName = Just "upcast1 [ShareView] does not call from1"}))
    , $(inspectTest ((hasNoGenerics 'upcast1PolyShareView) {testName = Just "polymorphic upcast1 ShareView leaves no generic representation"}))
    , $(inspectTest (('upcast1PolyShareView ==- 'refUpcast1PolyShareView) {testName = Just "polymorphic upcast1 ShareView matches pure cast"}))
    , $(inspectTest (('upcast1PolyShareView `doesNotUse` 'GL.from1) {testName = Just "polymorphic upcast1 ShareView does not call from1"}))
    , $(inspectTest ((hasNoGenerics 'upcast1PolyFunctor) {testName = Just "polymorphic upcast1 functor leaves no generic representation"}))
    , $(inspectTest (('upcast1PolyFunctor ==- 'refUpcast1PolyFunctor) {testName = Just "polymorphic upcast1 functor matches pure cast"}))
    , $(inspectTest (('upcast1PolyFunctor `doesNotUse` 'GL.from1) {testName = Just "polymorphic upcast1 functor does not call from1"}))
    ]
