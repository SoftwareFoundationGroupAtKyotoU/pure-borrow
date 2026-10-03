{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -O0 #-}

{- | Reads and writes through borrows, ordered by the state token, in code compiled at @-O0@.

A user's module compiled at @-O0@, or loaded into GHCi, calls the library's compiled code: @-O0@ implies @-fignore-interface-pragmas@, so it sees none of the library's unfoldings.
This is a component of its own, with every module at @-O0@, so that its kernels run that way.
In the main test suite they could not: there, a plain @-O0@ module hides the library's unfoldings from the @-O2@ modules compiled after it in the same session, and one that passes @-fno-ignore-interface-pragmas@ inlines the library's @INLINE@ functions, which a user's @-O0@ module does not.
-}
module Main (main) where

import PureBorrow.Unoptimised.Ordering qualified as Ordering
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "reads and writes through borrows are ordered by the state token, at -O0"
      [ testCase "Ref.Borrow.update writes before the scope ends" do
          Ordering.refUpdateWrites @?= (0, 1)
      , testCase "HashMap.take_ leaves an empty table behind" do
          Ordering.hashMapTakeEmpties @?= ([(1, 10), (2, 20)], [(3, 30)])
      , testCase "a growable size read after reclaim sees the push" do
          Ordering.growableSizeAfterReclaim @?= (3, 4)
      , testCase "a hash map size read after reclaim sees the insert" do
          Ordering.hashMapSizeAfterReclaim @?= (2, 3)
      , testCase "an owner reclaimed after a scope is not served from an earlier dup2" do
          Ordering.refDupReclaimFresh @?= (1, 0)
      , testCase "an owner reclaimed after a scope does not hand back a reference it gave away" do
          Ordering.refDupReclaimOwners @?= (100, 1)
      ]
