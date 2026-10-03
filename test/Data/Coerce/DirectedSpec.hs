{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Coerce.DirectedSpec (
  module Data.Coerce.DirectedSpec,
) where

import Control.Exception qualified as Exception
import Control.Monad.Borrow.Pure (Share, type (>=))
import Data.Coerce.Directed (upcast)
import Data.Coerce.Directed.DeriveSubtype.Types
import Data.Coerce.Directed.TypingCases
import Data.List qualified as List
import GHC.Exts (Multiplicity (..))
import Prelude.Linear qualified as L
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- These must keep compiling: this module is not built with deferred type errors.

-- A linear function is usable as an unrestricted one.
linearAsUnrestricted :: (Int %1 -> Int) -> (Int -> Int)
linearAsUnrestricted f = upcast f

-- Reflexive upcasts stay derivable at a multiplicity variable.
multiplicityPolymorphicRefl :: (Int %p -> Int) -> (Int %p -> Int)
multiplicityPolymorphicRefl f = upcast f

-- A function of any multiplicity is usable as an unrestricted one.
anyAsUnrestricted :: (Int %m -> Int) -> (Int -> Int)
anyAsUnrestricted f = upcast f

-- A linear function is usable at any multiplicity.
linearAsAny :: (Int %1 -> Int) -> (Int %m -> Int)
linearAsAny f = upcast f

-- The instances from 'Data.Coerce.Directed.Unsafe.deriveSubtype' relate what their fields allow.

shortenEnv :: (α >= β) => Env α Int %1 -> Env β Int
shortenEnv = upcast

shortenList :: (α >= β) => List (Share α Int) %1 -> List (Share β Int)
shortenList = upcast

shortenRose :: (α >= β) => Rose (Share α Int) %1 -> Rose (Share β Int)
shortenRose = upcast

shortenPhantom :: (α >= β) => Ph x (Share α Int) %1 -> Ph x (Share β Int)
shortenPhantom = upcast

shortenPolyKinded :: (α >= β) => PK x (Share α Int) %1 -> PK x (Share β Int)
shortenPolyKinded = upcast

linearList :: List (Int %1 -> Int) -> List (Int -> Int)
linearList xs = upcast xs

linearRose :: Rose (Int %1 -> Int) -> Rose (Int -> Int)
linearRose r = upcast r

-- A consumer of unrestricted functions is a consumer of linear ones.
contravariantFn :: Fn (Int -> Int) -> Fn (Int %1 -> Int)
contravariantFn f = upcast f

-- A field stored unrestricted may be read linearly.
manyToOne :: M 'Many Int %1 -> M 'One Int
manyToOne = upcast

applyList :: List (Int -> Int) -> [Int]
applyList Nil = []
applyList (Cons f fs) = f 10 : applyList fs

applyRose :: Rose (Int -> Int) -> [Int]
applyRose (Rose f children) = f 10 : concatMap applyRose children

test_multiplicityOrder :: TestTree
test_multiplicityOrder =
  testGroup
    "multiplicity order used by upcast"
    [ testCase "One is below Many" do
        _ <- Exception.evaluate oneBelowMany
        pure ()
    , testCase "Many is below Many" do
        _ <- Exception.evaluate manyBelowMany
        pure ()
    , expectDeferredTypeError
        "Many is not below One"
        "Couldn't match type"
        badManyBelowOne
    , testCase "a linear function upcasts to an unrestricted one" do
        linearAsUnrestricted (\x -> x) 42 @?= 42
    , testCase "a multiplicity-polymorphic reflexive upcast" do
        multiplicityPolymorphicRefl (\x -> x) 42 @?= 42
    , testCase "a function of any multiplicity upcasts to an unrestricted one" do
        anyAsUnrestricted (\x -> x) 42 @?= 42
    , testCase "a linear function upcasts to any multiplicity" do
        linearAsAny (\x -> x) 42 @?= 42
    ]

test_deriveSubtype :: TestTree
test_deriveSubtype =
  testGroup
    "deriveSubtype"
    [ testCase "a recursive type upcasts its elements" do
        applyList (linearList (Cons (\x -> x L.+ 1) (Cons (\x -> x L.* 2) Nil))) @?= [11, 20]
    , testCase "a type recursive through a list upcasts its elements" do
        applyRose (linearRose (Rose (\x -> x L.+ 1) [Rose (\x -> x L.* 3) []])) @?= [11, 30]
    , testCase "a parameter under a function's argument is contravariant" do
        case contravariantFn (Fn \f -> f 3) of
          Fn g -> g (\x -> x L.+ 1) @?= 4
    , testCase "a field stored unrestricted is read linearly" do
        case manyToOne (M 7) of
          M n -> n @?= 7
    ]

expectDeferredTypeError :: String -> String -> a -> TestTree
expectDeferredTypeError description expectedFragment value =
  testCase description do
    result <- Exception.try @Exception.SomeException (Exception.evaluate value)
    case result of
      Left exception ->
        assertBool
          ("unexpected deferred type error: " <> Exception.displayException exception)
          (expectedFragment `List.isInfixOf` Exception.displayException exception)
      Right _ ->
        assertFailure
          ("expected deferred type error containing " <> expectedFragment)
