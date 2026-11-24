{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Coerce.DirectedSpec (
  module Data.Coerce.DirectedSpec,
) where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure (
  After (..),
  BO,
  Lend,
  Mut,
  Share,
  Static,
  borrowM,
  linearly,
  pureAfter,
  reclaim',
  runBO,
  share,
  srunBO,
  srunBO_,
  type (<=),
  type (>=),
 )
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce.Directed (Subtype1, upcast, upcast1, type (<:))
import Data.Coerce.Directed.TypingCases
import Data.Coerce.Directed.Unsafe (SubtypeWitness (..), subtype, subtype1)
import Data.Functor.Identity (Identity (..))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefB
import GHC.Exts (FUN, Multiplicity (..))
import Generics.Linear (Generically (..), Generically1 (..))
import Generics.Linear.TH (deriveGeneric, deriveGenericAnd1)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude qualified as NonLinear

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

-- The library's instance for Identity.
identityUpcast :: Identity (Share Static Int) %1 -> Identity (Share (Al 0) Int)
identityUpcast = upcast

-- A linear function is usable at any multiplicity.
linearAsAny :: (Int %1 -> Int) -> (Int %m -> Int)
linearAsAny f = upcast f

-- User type with a 'Share' field: both 'Subtype1' and '(<:)' via 'Generically1'.
data ShareView α a = ShareView (Share α a) Int

deriveGenericAnd1 ''ShareView

deriving via
  Generically1 (ShareView β)
  instance
    (α >= β) => Subtype1 (ShareView α) (ShareView β)

deriving via
  Generically1 (ShareView β) b
  instance
    (α >= β, a <: b) => ShareView α a <: ShareView β b

-- User type with a 'Lend' field: both 'Subtype1' and '(<:)' via 'Generically1'.
data LendBox α a = LendBox (Lend α a)

deriveGenericAnd1 ''LendBox

deriving via
  Generically1 (LendBox β)
  instance
    (α <= β) => Subtype1 (LendBox α) (LendBox β)

deriving via
  Generically1 (LendBox β) b
  instance
    (α <= β, a <: b) => LendBox α a <: LendBox β b

-- User type with a 'BO' field: both 'Subtype1' and '(<:)' via 'Generically1'.
data Action α a = Action (BO α a)

deriveGenericAnd1 ''Action

deriving via
  Generically1 (Action β)
  instance
    (α >= β) => Subtype1 (Action α) (Action β)

deriving via
  Generically1 (Action β) b
  instance
    (α >= β, a <: b) => Action α a <: Action β b

-- User type with an 'After' field: both 'Subtype1' and '(<:)' via 'Generically1'.
data Finalizer α a = Finalizer (After α a)

deriveGenericAnd1 ''Finalizer

deriving via
  Generically1 (Finalizer β)
  instance
    (α <= β) => Subtype1 (Finalizer α) (Finalizer β)

deriving via
  Generically1 (Finalizer β) b
  instance
    (α <= β, a <: b) => Finalizer α a <: Finalizer β b

-- User type with a 'Mut' field: '(<:)' via 'Generically'.
data MutBox α a = MutBox (Mut α a) Int

deriveGeneric ''MutBox

deriving via
  Generically (MutBox β b)
  instance
    (α >= β, a <: b, b <: a) => MutBox α a <: MutBox β b

-- Nested functors exercising ':.:' in 'Rep1'.
data NestedView α a = NestedView (Maybe [Share α a])

deriveGenericAnd1 ''NestedView

deriving via
  Generically1 (NestedView β)
  instance
    (α >= β) => Subtype1 (NestedView α) (NestedView β)

deriving via
  Generically1 (NestedView β) b
  instance
    (α >= β, a <: b) => NestedView α a <: NestedView β b

-- Arity 8 boundary for 'Generically' (eight arguments, differing from the first to the last).
data Eight a1 a2 a3 a4 a5 a6 a7 x = Eight a1 a2 a3 a4 a5 a6 a7 x

deriveGeneric ''Eight

deriving via
  Generically (Eight b1 a2 a3 a4 a5 a6 a7 y)
  instance
    (a1 <: b1, x <: y) => Eight a1 a2 a3 a4 a5 a6 a7 x <: Eight b1 a2 a3 a4 a5 a6 a7 y

-- Arity 8 boundary for 'Generically1' (eight functor arguments, differing from the first to the eighth).
data Eight1 a1 a2 a3 a4 a5 a6 a7 a8 x = Eight1 a1 a2 a3 a4 a5 a6 a7 a8 x

deriveGenericAnd1 ''Eight1

deriving via
  Generically1 (Eight1 b1 a2 a3 a4 a5 a6 a7 a8)
  instance
    (a1 <: b1) => Subtype1 (Eight1 a1 a2 a3 a4 a5 a6 a7 a8) (Eight1 b1 a2 a3 a4 a5 a6 a7 a8)

-- Recursive user functor exercising both 'Subtype1' and '(<:)' via 'Generically1'.
data Tree a = Leaf a | Branch (Tree a) (Tree a)

deriveGenericAnd1 ''Tree

deriving via Generically1 Tree instance Subtype1 Tree Tree

deriving via Generically1 Tree b instance (a <: b) => Tree a <: Tree b

-- Left-recursive type whose differing field appears after the recursive spine.
data AfterList α a = NilAL | ConsAL (AfterList α a) (Share α a)

deriveGeneric ''AfterList

deriving via
  Generically (AfterList β b)
  instance
    (α >= β, a <: b) => AfterList α a <: AfterList β b

-- A self-recursive type whose Subtype1 goes through an arity head of Generically1.
data RList α a = RNil | RCons (Share α a) (RList α a)

deriveGenericAnd1 ''RList

deriving via Generically1 (RList β) instance (α >= β) => Subtype1 (RList α) (RList β)

expectDeferredTypeError :: String -> [String] -> a -> TestTree
expectDeferredTypeError description expectedFragments value =
  testCase description do
    result <- Exception.try @Exception.SomeException (Exception.evaluate value)
    case result of
      Left exception ->
        let rendered = Exception.displayException exception
         in assertBool
              ("unexpected deferred type error: " <> rendered)
              (NonLinear.all (`List.isInfixOf` rendered) expectedFragments)
      Right _ ->
        assertFailure
          ("expected deferred type error containing " <> List.intercalate ", " expectedFragments)

test_multiplicityOrder :: TestTree
test_multiplicityOrder =
  testGroup
    "multiplicity order used by upcast"
    [ testCase "One is below Many" do
        _ <- Exception.evaluate oneBelowMany
        NonLinear.pure ()
    , testCase "Many is below Many" do
        _ <- Exception.evaluate manyBelowMany
        NonLinear.pure ()
    , expectDeferredTypeError
        "Many is not below One"
        ["Couldn't match type"]
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

test_subtype1AndSingleGeneric :: TestTree
test_subtype1AndSingleGeneric =
  testGroup
    "Subtype1 and single-Generic upcasts"
    [ testCase "ShareView derives Subtype1 and (<:) and preserves values" do
        let useShareView :: forall γ δ. (γ >= δ) => ShareView γ (Ref Int) -> BO δ Int
            useShareView view = Control.do
              let view1 :: ShareView δ (Ref Int)
                  view1 = upcast view
                  view2 :: ShareView δ (Ref Int)
                  view2 = upcast1 view
                  !(ShareView shr1 tag1) = view1
                  !(ShareView shr2 tag2) = view2
              n1 <- RefB.copyRef shr1
              n2 <- RefB.copyRef shr2
              Control.pure (n1 + tag1 + n2 + tag2)
            result = linearly \lin -> DataFlow.do
              (lin1, lin2) <- dup lin
              ref <- Ref.new (41 :: Int) lin1
              runBO lin2 Control.do
                (mut, lend) <- borrowM ref
                mut <- RefB.modify (+ 1) mut
                let !(Ur shr) = share mut
                    view = ShareView shr 7
                summed <- srunBO_ (useShareView view)
                Control.pure ((\r -> Ref.free r + summed) Control.<$> reclaim' lend)
        result @?= 140
    , testCase "LendBox derives Subtype1 and (<:) and reclaims values" do
        let widenLendUpcast :: forall β γ. (γ <= β) => LendBox γ (Ref Int) %1 -> After β Int
            widenLendUpcast (LendBox l) =
              let %1 !(LendBox l') = upcast (LendBox l) :: LendBox β (Ref Int)
               in Ref.free Control.<$> reclaim' l'
            widenLendUpcast1 :: forall β γ. (γ <= β) => LendBox γ (Ref Int) %1 -> After β Int
            widenLendUpcast1 (LendBox l) =
              let %1 !(LendBox l') = upcast1 (LendBox l) :: LendBox β (Ref Int)
               in Ref.free Control.<$> reclaim' l'
            result1 = linearly \lin -> DataFlow.do
              (lin1, lin2) <- dup lin
              ref <- Ref.new (10 :: Int) lin1
              runBO lin2 Control.do
                r <- srunBO Control.do
                  (mut, lend) <- borrowM ref
                  mut <- RefB.modify (+ 5) mut
                  let !() = consume mut
                  Control.pure (widenLendUpcast (LendBox lend))
                pureAfter r
            result2 = linearly \lin -> DataFlow.do
              (lin1, lin2) <- dup lin
              ref <- Ref.new (20 :: Int) lin1
              runBO lin2 Control.do
                r <- srunBO Control.do
                  (mut, lend) <- borrowM ref
                  mut <- RefB.modify (+ 3) mut
                  let !() = consume mut
                  Control.pure (widenLendUpcast1 (LendBox lend))
                pureAfter r
        (result1, result2) @?= (15, 23)
    , testCase "Action (BO) derives Subtype1 and (<:) and runs in sublifetime" do
        let mkAction :: Share γ (Ref Int) -> Action γ Int
            mkAction shr = Action (RefB.copyRef shr)
            runActionSub :: forall γ δ. (γ >= δ) => Action γ Int -> BO δ (Int, Int)
            runActionSub act = Control.do
              let Action bo1 = upcast act :: Action δ Int
                  Action bo2 = upcast1 act :: Action δ Int
              a <- bo1
              b <- bo2
              Control.pure (a, b)
            result = linearly \lin -> DataFlow.do
              (lin1, lin2) <- dup lin
              ref <- Ref.new (100 :: Int) lin1
              runBO lin2 Control.do
                (mut, lend) <- borrowM ref
                let !(Ur shr) = share mut
                (n1, n2) <- srunBO_ (runActionSub (mkAction shr))
                Control.pure ((\r -> Ref.free r + n1 + n2) Control.<$> reclaim' lend)
        result @?= 300
    , testCase "Finalizer (After) derives Subtype1 and (<:) and discharges in parent" do
        let widenFinUpcast :: forall γ δ. (γ <= δ) => Finalizer γ (Ref Int) %1 -> After δ Int
            widenFinUpcast (Finalizer fin) =
              let %1 !(Finalizer fin') = upcast (Finalizer fin) :: Finalizer δ (Ref Int)
               in Ref.free Control.<$> fin'
            widenFinUpcast1 :: forall γ δ. (γ <= δ) => Finalizer γ (Ref Int) %1 -> After δ Int
            widenFinUpcast1 (Finalizer fin) =
              let %1 !(Finalizer fin') = upcast1 (Finalizer fin) :: Finalizer δ (Ref Int)
               in Ref.free Control.<$> fin'
            result1 = linearly \lin -> DataFlow.do
              (lin1, lin2) <- dup lin
              ref <- Ref.new (12 :: Int) lin1
              runBO lin2 Control.do
                x <- srunBO Control.do
                  (mut, lend) <- borrowM ref
                  mut <- RefB.modify (* 2) mut
                  let !() = consume mut
                  Control.pure (widenFinUpcast (Finalizer (reclaim' lend)))
                pureAfter x
            result2 = linearly \lin -> DataFlow.do
              (lin1, lin2) <- dup lin
              ref <- Ref.new (13 :: Int) lin1
              runBO lin2 Control.do
                x <- srunBO Control.do
                  (mut, lend) <- borrowM ref
                  mut <- RefB.modify (+ 1) mut
                  let !() = consume mut
                  Control.pure (widenFinUpcast1 (Finalizer (reclaim' lend)))
                pureAfter x
        (result1, result2) @?= (24, 14)
    , testCase "MutBox derives (<:) via Generically and preserves mutation" do
        let useMutBox :: forall γ δ. (γ >= δ) => MutBox γ (Ref Int) %1 -> BO δ Int
            useMutBox box = Control.do
              let %1 !(MutBox mut' tag) = upcast box :: MutBox δ (Ref Int)
              mut' <- RefB.modify (+ 4) mut'
              Control.pure (consume mut' `lseq` tag)
            result = linearly \lin -> DataFlow.do
              (lin1, lin2) <- dup lin
              ref <- Ref.new (5 :: Int) lin1
              runBO lin2 Control.do
                (mut, lend) <- borrowM ref
                tag <- srunBO_ (useMutBox (MutBox mut 9))
                Control.pure ((\r -> Ref.free r + tag) Control.<$> reclaim' lend)
        result @?= 18
    , testCase "NestedView exercises (:.:) in Rep1" do
        let useNestedView :: forall γ δ. (γ >= δ) => NestedView γ (Ref Int) -> BO δ Int
            useNestedView nv = Control.do
              let NestedView mshrs1 = upcast nv :: NestedView δ (Ref Int)
                  NestedView mshrs2 = upcast1 nv :: NestedView δ (Ref Int)
              case (mshrs1, mshrs2) of
                (Just [s1], Just [s2]) -> Control.do
                  x1 <- RefB.copyRef s1
                  x2 <- RefB.copyRef s2
                  Control.pure (x1 + x2)
                _ -> Control.pure 0
            result = linearly \lin -> DataFlow.do
              (lin1, lin2) <- dup lin
              ref <- Ref.new (33 :: Int) lin1
              runBO lin2 Control.do
                (mut, lend) <- borrowM ref
                let !(Ur shr) = share mut
                n <- srunBO_ (useNestedView (NestedView (Just [shr])))
                Control.pure ((\r -> Ref.free r + n) Control.<$> reclaim' lend)
        result @?= 99
    , testCase "arity 8 works for both Generically and Generically1" do
        let e :: Eight (Int %1 -> Int) Int Int Int Int Int Int Int
            e = Eight (+ 1) 2 3 4 5 6 7 8
            e' :: Eight (Int -> Int) Int Int Int Int Int Int Int
            e' = upcast e
            evalEight (Eight f a2 a3 a4 a5 a6 a7 x) = f x + a2 + a3 + a4 + a5 + a6 + a7
            f1 :: Eight1 (Int %1 -> Int) Int Int Int Int Int Int Int Int
            f1 = Eight1 (+ 1) 2 3 4 5 6 7 8 9
            f1' :: Eight1 (Int -> Int) Int Int Int Int Int Int Int Int
            f1' = upcast1 f1
            evalEight1 (Eight1 f a2 a3 a4 a5 a6 a7 a8 x) = f x + a2 + a3 + a4 + a5 + a6 + a7 + a8
        (evalEight e', evalEight1 f1') @?= (36, 45)
    , testCase "library Subtype1 instances work with upcast1" do
        let xs = upcast1 [(\x -> x + 1) :: Int %1 -> Int] :: [Int -> Int]
            mx = upcast1 (Just ((\x -> x + 2) :: Int %1 -> Int)) :: Maybe (Int -> Int)
            nx = upcast1 ((\x -> x + 3) :| [] :: NonEmpty (Int %1 -> Int)) :: NonEmpty (Int -> Int)
            ix = upcast1 (Identity ((\x -> x + 4) :: Int %1 -> Int)) :: Identity (Int -> Int)
            px = upcast1 ((\x -> x + 5) :: Int %1 -> Int, (\x -> x + 6) :: Int %1 -> Int) :: (Int -> Int, Int -> Int)
            ex = upcast1 (Left ((\x -> x + 7) :: Int %1 -> Int) :: Either (Int %1 -> Int) (Int %1 -> Int)) :: Either (Int -> Int) (Int -> Int)
            fx :: Int -> Int
            fx = upcast1 ((\x -> x + 8) :: Int %1 -> Int) :: FUN 'Many Int Int
        case (xs, mx, nx, ix, px, ex) of
          ([f1], Just f2, f3 :| [], Identity f4, (f5, f6), Left f7) ->
            [f1 0, f2 0, f3 0, f4 0, f5 0, f6 0, f7 0, fx 0] @?= [1, 2, 3, 4, 5, 6, 7, 8]
          _ -> assertFailure "unexpected pattern in library Subtype1 test"
    ]

test_typingRejections :: TestTree
test_typingRejections =
  testGroup
    "Generically and Subtype1 refusals"
    [ expectDeferredTypeError
        "Generically refuses two distinct types with metadata-free Generic instances"
        ["Cannot derive (<:) via Generically"]
        badGenericallyTwoTypes
    , expectDeferredTypeError
        "Generically refuses two distinct types"
        ["Cannot derive (<:) via Generically"]
        badGenericallyDistinctTypes
    , expectDeferredTypeError
        "Subtype1 refuses Mut"
        ["Mut is invariant in the type it borrows"]
        badSubtype1Mut
    , expectDeferredTypeError
        "Generically1 refuses two distinct functors"
        ["Cannot derive Subtype1 via Generically1"]
        badGenerically1DistinctFunctors
    ]

test_recursiveWitnesses :: TestTree
test_recursiveWitnesses =
  testGroup
    "recursive Subtype and Subtype1 witnesses terminate"
    [ testCase "[] (<:) terminates with UnsafeSubtype" do
        UnsafeSubtype <- Exception.evaluate (subtype @[Share Static Int] @[Share (Al 0) Int])
        NonLinear.pure ()
    , testCase "[] Subtype1 terminates with UnsafeSubtype" do
        UnsafeSubtype <- Exception.evaluate (subtype1 @[] @[] (subtype @(Share Static Int) @(Share (Al 0) Int)))
        NonLinear.pure ()
    , testCase "NonEmpty (<:) terminates with UnsafeSubtype" do
        UnsafeSubtype <- Exception.evaluate (subtype @(NonEmpty (Share Static Int)) @(NonEmpty (Share (Al 0) Int)))
        NonLinear.pure ()
    , testCase "NonEmpty Subtype1 terminates with UnsafeSubtype" do
        UnsafeSubtype <- Exception.evaluate (subtype1 @NonEmpty @NonEmpty (subtype @(Share Static Int) @(Share (Al 0) Int)))
        NonLinear.pure ()
    , testCase "Tree (<:) terminates with UnsafeSubtype" do
        UnsafeSubtype <- Exception.evaluate (subtype @(Tree (Share Static Int)) @(Tree (Share (Al 0) Int)))
        NonLinear.pure ()
    , testCase "Tree Subtype1 terminates with UnsafeSubtype" do
        UnsafeSubtype <- Exception.evaluate (subtype1 @Tree @Tree (subtype @(Share Static Int) @(Share (Al 0) Int)))
        NonLinear.pure ()
    , testCase "NestedView (<:) terminates with UnsafeSubtype" do
        UnsafeSubtype <- Exception.evaluate (subtype @(NestedView Static Int) @(NestedView (Al 0) Int))
        NonLinear.pure ()
    , testCase "NestedView Subtype1 terminates with UnsafeSubtype" do
        UnsafeSubtype <- Exception.evaluate (subtype1 @(NestedView Static) @(NestedView (Al 0)) (subtype @Int @Int))
        NonLinear.pure ()
    , testCase "left-recursive AfterList (<:) terminates with UnsafeSubtype" do
        UnsafeSubtype <- Exception.evaluate (subtype @(AfterList Static Int) @(AfterList (Al 0) Int))
        NonLinear.pure ()
    , testCase "RList Subtype1 through an arity head terminates with UnsafeSubtype" do
        UnsafeSubtype <- Exception.evaluate (subtype1 @(RList Static) @(RList (Al 0)) (subtype @Int @Int))
        NonLinear.pure ()
    ]
