{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
This module is meant to be the prelude module of /Pure Borrow/, a Rust-style borrow realization in Linear Haskell.
This module provides only a basic pieces of the API, and you may want to import other modules, e.g. "Control.Monad.Borrow.Pure.BO", for more utilities.
-}
module Control.Monad.Borrow.Pure (
  -- $header

  -- * Lifetimes and Subtyping
  -- $lifetimes

  -- ** Lifetime
  Lifetime,
  type (/\),
  type (<=) (),
  type (>=),
  type Static,
  neverEnds,

  -- ** Subtyping and upcasting
  type (<:),
  upcast,

  -- * Linearity witnesses
  -- $linearly
  Linearly,
  linearly,
  LinearOnly,

  -- * Core 'BO' monad
  BO (),
  runBO,
  runBOLend,
  runBO_,
  srunBO,
  srunBO_,
  askLinearly,
  asksLinearly,
  asksLinearlyM,

  -- ** Subtyping

  -- ** In-place modification with mutable borrows
  modifyBO,
  modifyBO_,
  modifyLinearOnlyBO,
  modifyLinearOnlyBO_,

  -- * Parallel computation
  parBO,

  -- * Borrowing
  Alias,
  AliasKind,
  BorrowKind,
  Borrow,
  Mut,
  Share,
  Lend,
  End,
  After (..),
  coerceShare,
  borrowM,
  borrowLinearlyM,
  sharing',
  sharing,
  (<$~),
  sharing_,
  (<$=),
  reborrowing',
  reborrowing,
  (<%~),
  reborrowing_,
  (<%=),
  share,
  reclaim',
  reclaim,
  pureAfter,
  joinMut,
  joinLend,

  -- ** Copying and Cloning
  Copyable (..),
  copyMut,
  Clone (..),

  -- ** Splitting aliases
  DistributesAlias (),
  split,
  GenericDistributesAlias,
  genericSplit,
  splitPair,
  splitEither,

  -- * Re-exporting Prelude.Linear classes
  Consumable (..),
  Dupable (..),
  dup,
  dup3,
  Movable (..),
  Ur (..),
) where

import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Clone
import Control.Monad.Borrow.Pure.Copyable
import Data.Unrestricted.Linear (Consumable (..), Dupable (..), Movable (..), Ur (..), dup, dup3)

{- $setup
>>> :set -XBlockArguments -XLinearTypes -XNoImplicitPrelude -XImpredicativeTypes -XQualifiedDo
>>> :m -Prelude
>>> import Prelude.Linear
>>> import qualified Data.Vector.Mutable.Linear.Borrow as VL
>>> import Control.Syntax.DataFlow qualified as DataFlow
>>> import Control.Functor.Linear qualified as Control
-}

{- $header
= Pure Borrow: An Overview

This module provides the main API of /Pure Borrow/, the pure realization of Rust-style borrowing in Linear Haskell.

== Examples

You need the following language extensions to use this module:

    * BlockArguments
    * LinearTypes
    * NoImplicitPrelude
    * ImpredicativeTypes
    * QualifiedDo

...and import theese modules:

@
import Prelude.Linear
import qualified Data.Vector.Mutable.Linear.Borrow as VL
import Control.Syntax.DataFlow qualified as DataFlow
import Control.Functor.Linear qualified as Control
@

The following example code initializes a mutable vector, modifies it coordinate-wise, and then read a part and all the contents at the last:

>>> :{
  example1 :: (Int, [Int])
  example1 = linearly \lin -> DataFlow.do
    (lin, lin') <- dup lin
    vec <- VL.fromList [0, 1, 2] lin
    runBO lin' Control.do
      (mvec, lend) <- borrowM vec
      mvec <- VL.modify 0 (+ 3) mvec
      mvec <- VL.modify 2 (+ 5) mvec
      mvec <- VL.modify 0 (* 4) mvec
      let !(Ur svec) = share mvec
      Ur n <- VL.copyAt 0 svec
      pureAfter (n, unur $ VL.toList (reclaim lend))
:}

>>> example1
(12,[12,1,7])

This just returns @(12, [12, 1, 7])@ as expected, which is not so surprising.
But what if you want to modify non-overlapping segments of the vectors /in-parallel/?
In particular, while you have to do two modifications to index @0@ sequentially, you can modify index @2@ in parallel with the first modification to index @0@.
This is where pure concurrency with 'parBO' comes in:

>>> :{
  example2 :: (Int, [Int])
  example2 = linearly \lin -> DataFlow.do
    (lin, lin') <- dup lin
    vec <- VL.fromList [0, 1, 2] lin
    runBO lin' Control.do
      (mvec, lend) <- borrowM vec
      let !(mvec1, mvec2) = VL.splitAt 1 mvec -- (*)
      (mvec, ()) <-
        parBO
          ( Control.do
              mvec1 <- VL.modify 0 (+ 3) mvec1
              VL.modify 0 (* 4) mvec1
          )
          (consume Control.<$> VL.modify 1 (+ 5) mvec2)
      let !(Ur svec) = share mvec
      Ur n <- VL.copyAt 0 svec
      pureAfter (n, unur $ VL.toList (reclaim lend))
:}

>>> example2
(12,[12,1,7])

The line after @(*)@ splits the mutable vector into two non-overlapping mutable borrows, which can be safely used in parallel with 'parBO'.
It just returns the first half of the vector and discards the second half.

But this kind of manual discarding of resources are getting so tedious, right?
This is where our /borrow/-based /affine/ API shines, which allows you to write the same code without manually discarding resources using 'reborrowing':

>>> :{
  example3 :: (Int, [Int])
  example3 = linearly \lin -> DataFlow.do
    (lin, lin') <- dup lin
    vec <- VL.fromList [0, 1, 2] lin
    runBO lin' Control.do
      (mvec, lend) <- borrowM vec
      -- (!)
      mvec <- reborrowing_ mvec \mvec -> Control.do
        let !(mvec1, mvec2) = VL.splitAt 1 mvec
        -- (!!)
        consume
          Control.<$> parBO
            ( Control.do
                mvec1 <- VL.modify 0 (+ 3) mvec1
                VL.modify 0 (* 4) mvec1
            )
            (VL.modify 1 (+ 5) mvec2)
      let !(Ur svec) = share mvec -- (!!!)
      Ur n <- VL.copyAt 0 svec
      pureAfter (n, unur $ VL.toList (reclaim lend))
:}

>>> example2
(12,[12,1,7])

Beware that the line after @(!)@ opens the new sublifetime by 'reborrowing_'.
Within this sublifetime, new mutable borrow @mvec@ is divided into two pieces, and then modified parallely on each slices after @(!!)@.
But this time, the whole splitted @mvec1@ and @mvec2@ are 'consume'd after the 'parBO' returned, but after the sublifetime has been ended, the original mutable borrow to the whole vector is recovered and used in Line @(!!!)@!

This way, you can treat and split mutable/immutable borrows freely without manually dropping/reuniting into the original resources.
-}

{- $lifetimes
Lifetime is a key concept in borrow.
You can understand it is a version of thread parameter @s@ in @'Control.Monad.ST' s@, but refined with subtyping relation t'(<=)' (or /outlives/-relation t'(>=)').

Every @'BO' α@ computation is parametrized with lifetime, and ordinary borrows, such as @'Mut' α a@ or @'Share' α a@, and lenders @'Lend' α a@ also have the lifetime for which they are valid.
To accomodate the casting between different lifetimes, we also provide the 'upcast' operator that has a lifetime parameter according to the sublifetime relation.
The 'upcast' operator casts a given type along t'(<:)' relation, which extends t'(<=)' to the other types appropriately.

Any two lifetimes @α@ and @β@ have the /meet/ @α '/\' β@, which is the longest lifetime that is shorter than both @α@ and @β@; i.e. @α '/\' β@ is the most generic lifetime such that @α '/\' β <= α@ and @α '/\' β <= β@.
We use some tricks using '/\' to workaround type-checking higher-level combinators.
For example, consider the type of 'srunBO_':

@
'srunBO_' :: (forall β. 'BO' (β '/\' α) a) %1 -> 'BO' α a
@

At first glance, the type @forall β. 'BO' (β '/\' α) a@ might looks rather cryptic.
But essentially, the above type is morally equivalent to the following:

@
'srunBO_' :: (forall β \<= α. 'BO' β a) => 'BO' α a
@

That is, all the 'srunBO_' does is that it opens a ephemeral sublifetime @β <= α@, and does all the computation inside it.
However, without involved hacking or type-checker plugins, the type system is not good at treating transitivity of subtyping relation.
By just quantifying over all the lifetimes and combine them with '/\', we can make the type-checker happy without losing generality.

So, if you see the pattern like binding other lifetimes with @forall@ and combined it with '/\', you can think of it as just quantifying over the sublifetime of the current lifetime.
-}

{- $linearly

When you allocate the mutable resources, you must ensure that they are used only /linearly/; i.e. they are used exactly once.
In Linear Haskell, we use /linear arrow/ @%1 ->@ to express this invariant.
More precisely, @a %1 -> b@ reads that /if the application of the function is consumed exactly once, then the argument is consumed exactly once/.
This definition poses a subtle problem: the resource is guaranteed to be used linearly only when the resource is bound under some linear arrow context.
Hence, we must known that we are under the linear context before allocate mutable references, otherwise the mutable state can leak outside.

The 'Linearly' witnesses exactly this invariant.
The important point is that it can be introduced into the context only by 'linearly' combinator:

@
'linearly' :: 'Movable' a => ('Linearly' %1 -> a) %1 -> a
@

This assures that 'Linearly' can be used as linearity witness that can be used when the mutable resources are allocated.
You can duplicate 'linearly' as many as you want by 'dup' and drop it by 'consume'.

@
fromList :: [a] %1 -> 'Linearly' %1 -> 'Data.Vector.Mutable.Linear.Borrow.Vector' a
@

See [Linear Constraints: the Problem with Scopes](https://www.tweag.io/blog/2023-03-23-linear-constraints-linearly/) for more details.

Those mutable datatypes can only be introduced via 'Linearly' witness, so they can be seen as baring the 'Linearly' witness inside.
'LinearOnly' is a type class for such datatypes, and it provides the 'withLinearly' combinator to borrow the linear witness from the context.

Further, running 'BO' monad also requires 'Linearly':

@
runBO_ :: 'Linearly' %1 -> (forall α. 'BO' α a) %1 -> a
@

Hence, you can retrieve 'Linearly' token via 'askLinearly', 'asksLinearlyM', etc.
-}
