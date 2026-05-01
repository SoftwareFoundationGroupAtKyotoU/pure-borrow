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
This module provides only the basic pieces of the API, and you may want to import other modules, e.g. "Control.Monad.Borrow.Pure.BO", "Data.Ref.Linear.Borrow", or "Data.Vector.Mutable.Linear.Borrow", for more utilities.
-}
module Control.Monad.Borrow.Pure (
  -- $header

  -- * Core 'BO' monad
  BO (),
  runBO,
  runBOLend,
  runBO_,
  srunBO,
  srunBO_,

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
  upcast,
  type (<:),

  -- * Linearity witnesses
  -- $linearly
  Linearly,
  linearly,
  LinearOnly,
  withLinearly,
  askLinearly,
  asksLinearly,
  asksLinearlyM,

  -- * Parallel computation
  parBO,

  -- * Borrowing
  -- $borrow

  -- ** Central Borrow types
  Mut,
  Share,
  Lend,
  Borrow,
  Alias,

  -- ** Introduction form
  borrowM,
  borrowLinearlyM,
  share,

  -- ** Reborrowing and computation in sublifetime
  reborrowing',
  reborrowing,
  (<%~),
  reborrowing_,
  (<%=),
  sharing',
  sharing,
  (<$~),
  sharing_,
  (<$=),

  -- ** Finalization and reclamation
  After (..),
  reclaim',
  reclaim,
  pureAfter,
  End,

  -- ** In-place modification with mutable borrows
  modifyBO,
  modifyBO_,
  modifyLinearOnlyBO,
  modifyLinearOnlyBO_,

  -- ** Utility function to manipulate borrows
  joinMut,
  joinLend,
  coerceShare,

  -- ** Copying and Cloning
  -- $copy-and-clone
  Copyable (..),
  copyMut,
  Clone (..),

  -- ** Splitting aliases
  -- $splitting
  splitPair,
  splitEither,
  split,
  DistributesAlias (),
  GenericDistributesAlias,
  genericSplit,

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

The core idea is that mutable resources are accessed through lifetime-indexed borrows:

    * 'Linearly' proves that we are in a context where linear resources can be allocated and used safely.
    * @'BO' α a@ is a computation that may use borrows valid during the lifetime @α@. It also provides pure API with the concurrency primitive.
    * @'Mut' α a@ is a mutable borrow of an @a@ valid during @α@.
    * @'Share' α a@ is an immutable borrow of an @a@ valid during @α@.
    * @'Lend' α a@ is the capability to recover the original @a@ after @α@ ends.
    * @'After' α a@ describes post-processing that runs after @α@ ends, such as reclaiming a 'Lend'.

== Examples

You need the following language extensions to use this module:

    * BlockArguments
    * LinearTypes
    * NoImplicitPrelude
    * ImpredicativeTypes
    * QualifiedDo

...and import these modules:

@
import Prelude.Linear
import Control.Monad.Borrow.Pure
import qualified Data.Vector.Mutable.Linear.Borrow as VL
import Control.Syntax.DataFlow qualified as DataFlow
import Control.Functor.Linear qualified as Control
@

The examples use qualified do-notation. @DataFlow.do@ is convenient for rebiding pure values, and @Control.do@ is the do-notation for linear functors and monads.

The following example initializes a mutable vector, modifies it coordinate-wise, and then reads one element and the final contents:

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
In particular, while you have to do two modifications to index @0@ sequentially, you can modify the segment containing original index @2@ in parallel with the first modification to index @0@.
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
The left branch returns the modified first slice, while the right branch consumes its slice and returns @()@. The whole original vector is later recovered through @lend@.

Manual discarding of split resources becomes tedious quickly.
This is where the /borrow/-based /affine/ API helps: 'reborrowing' lets you work in a shorter lifetime without manually reclaiming the original borrow.

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

>>> example3
(12,[12,1,7])

The line after @(!)@ opens a new sublifetime with 'reborrowing_'.
Within this sublifetime, the new mutable borrow @mvec@ is divided into two pieces, and then both slices are modified in parallel after @(!!)@.
This time, the split @mvec1@ and @mvec2@ are 'consume'd after 'parBO' returns. Once the sublifetime ends, the original mutable borrow to the whole vector is recovered and used at @(!!!)@.

This way, you can treat and split mutable and immutable borrows freely without manually dropping or reuniting them into the original resources.
-}

{- $lifetimes
Lifetime is a key concept in borrowing.
You can understand it as a version of the thread parameter @s@ in @'Control.Monad.ST' s@, but refined with the subtyping relation t'(<=)' (or /outlives/-relation t'(>=)').

Every @'BO' α@ computation is parametrized with lifetime, and ordinary borrows, such as @'Mut' α a@ or @'Share' α a@, and lenders @'Lend' α a@ also have the lifetime for which they are valid.
To accommodate casting between different lifetimes, we also provide the 'upcast' operator that has a lifetime parameter according to the sublifetime relation.
The 'upcast' operator casts a given type along t'(<:)' relation, which extends t'(<=)' to the other types appropriately.

Any two lifetimes @α@ and @β@ have the /meet/ @α '/\' β@, which is the longest lifetime that is shorter than both @α@ and @β@; i.e. @α '/\' β@ is the most generic lifetime such that @α '/\' β <= α@ and @α '/\' β <= β@.
We use some tricks with '/\' to work around type-checking higher-level combinators.
For example, consider the type of 'srunBO_':

@
'srunBO_' :: (forall β. 'BO' (β '/\' α) a) %1 -> 'BO' α a
@

At first glance, the type @forall β. 'BO' (β '/\' α) a@ might look rather cryptic.
But essentially, the above type is morally equivalent to the following:

@
'srunBO_' :: (forall β \<= α. 'BO' β a) %1 -> 'BO' α a
@

That is, all 'srunBO_' does is open an ephemeral sublifetime @β <= α@ and run the computation inside it.
However, without involved hacking or type-checker plugins, the type system is not good at treating transitivity of subtyping relation.
By quantifying over all lifetimes and combining them with '/\', we can make the type-checker happy without losing generality.

So, if you see a pattern that binds other lifetimes with @forall@ and combines them with '/\', you can think of it as quantifying over a sublifetime of the current lifetime.
-}

{- $linearly

When you allocate mutable resources, you must ensure that they are used only /linearly/; i.e. they are used exactly once.
In Linear Haskell, we use /linear arrow/ @%1 ->@ to express this invariant.
More precisely, @a %1 -> b@ reads that /if the application of the function is consumed exactly once, then the argument is consumed exactly once/.
This definition poses a subtle problem: the resource is guaranteed to be used linearly only when the resource is bound under some linear arrow context.
Hence, we must know that we are under a linear context before allocating mutable references, otherwise the mutable state can leak outside.

The 'Linearly' token witnesses exactly this invariant.
The important point is that it can be introduced into the context only by 'linearly' combinator:

@
'linearly' :: 'Movable' a => ('Linearly' %1 -> a) %1 -> a
@

This assures that 'Linearly' can be used as a linearity witness when mutable resources are allocated.
You can duplicate a 'Linearly' token as many times as you want with 'dup' and drop it with 'consume'.

@
fromList :: [a] %1 -> 'Linearly' %1 -> 'Data.Vector.Mutable.Linear.Borrow.Vector' a
@

See [Linear Constraints: the Problem with Scopes](https://www.tweag.io/blog/2023-03-23-linear-constraints-linearly/) for more details.

Those mutable datatypes can only be introduced via a 'Linearly' witness, so they can be seen as carrying the 'Linearly' witness inside.
'LinearOnly' is a type class for such datatypes and we can use it to recover a 'Linearly' witness from such values.

@
'withLinearly' :: ('LinearOnly' a) => a %1 -> ('Linearly', a)
@

Further, running the 'BO' computation also requires 'Linearly':

@
runBO_ :: 'Linearly' %1 -> (forall α. 'BO' α a) %1 -> a
@

Hence, you can retrieve a 'Linearly' token inside 'BO' via 'askLinearly', 'asksLinearlyM', etc.
-}

{- $borrow
To treat a linear resource inside 'BO' monad, you have to borrow it first.
The most typical introduction form is 'borrowM':

@
'borrowM' :: a %1 -> 'BO' α ('Mut' α a, 'Lend' α a)
@

This borrows a linear resource into the same lifetime as the ambient 'BO', returning a 'Mut'able borrow and a 'Lend'er of the original resource.
Or, you can do the linear allocation of the resource and borrow it at the same time with 'borrowLinearlyM':

@
'borrowLinearlyM' :: (Linearly %1 -> a) %1 -> 'BO' α ('Mut' α a, 'Lend' α a)
@

In any case, the main computation with possible destructive updates is done on 'Mut'able borrows, and the original resource will be 'reclaim'ed from the 'Lend'er at the end of the lifetime @α@.
More precisely, @'Lend' α a@ must be processed in an appropriate @'After' α r@ value that is returned to 'runBO', 'srunBO', or the reborrowing operators described later.
@'After' α a@ is a kind of finalizer that will be run after the lifetime @α@ has 'End'ed, and it can be used to reclaim the original resource from a 'Lend'er and do further final computation such as conversion or consumption.

One can 'share' the 'Mut'able borrow into 'Share'd borrow:

@
'share' :: 'Mut' α a %1 -> 'Ur' ('Share' α a)
@

As 'Share' is an immutable borrow, it can be freely duplicated and dropped, as witnessed by the 'Ur' wrapper.
'Share'd borrows are always introduced nonlinearly, so that you can freely use them multiple times or drop them at any time.
Note that 'share' consumes the original 'Mut'. If you want to share the resource temporarily into a sublifetime and then continue mutating afterwards, you can use the 'sharing' combinator (and its variants 'sharing'' and 'sharing_'):

@
'sharing' ::
  forall α α' a r.
  'Mut' α a %1 ->
  (forall β. 'Share' (β /\ α) a -> 'BO' (β /\ α') r) %1 ->
  'BO' α' (r, 'Mut' α a)
@

Analogously, you can reborrow mutable borrows into sublifetimes using the 'reborrowing' combinator (and its variants 'reborrowing'' and 'reborrowing_').

@
'reborrowing' ::
  forall α α' a r.
  'Mut' α a %1 ->
  (forall β. 'Mut' (β /\ α) a -> 'BO' (β /\ α') r) %1 ->
  'BO' α' (r, 'Mut' α a)
@

There is an experimental interface abstracting the reborrowable borrows in "Control.Monad.Borrow.Pure.Experimental.Reborrowable".

== Borrow polymorphism

'Mut', 'Share', and 'Lend' are all specific instantiations of the 'Alias' type:

@
type 'Mut' α a = 'Borrow' 'Mut α a
type 'Share' α a = 'Borrow' 'Share α a
type 'Borrow' bk α a = 'Alias' ('Borrow bk) α a
type 'Lend' α a = 'Alias' 'Lend α a
@

Hence, if you see @'Borrow' bk α a@ in a function, it can be either 'Mut' or 'Share'. If you see @'Alias' ak α a@, it may also be a 'Lend'.

"Control.Monad.Borrow.Pure.Experimental.Borrows" provides an experimental API for treating a bundle of multiple borrows in the same lifetime at once.

== Which combinator should I use?

    * Use 'borrowM' to borrow an existing linear value inside 'BO'.
    * Use 'borrowLinearlyM' to allocate a linear value and immediately borrow it inside 'BO'.
    * Use 'share' to permanently turn a 'Mut' into an unrestricted 'Share'.
    * Use 'sharing' or 'sharing_' to share temporarily and then regain the original 'Mut'.
    * Use 'reborrowing' or 'reborrowing_' to create a shorter-lived 'Mut' and then regain the original 'Mut'.
    * Use 'reclaim' or 'reclaim'' to recover the original resource from a 'Lend' after the lifetime ends.
-}

{- $copy-and-clone

For some types, you can 'copy' them as the direct value out of a borrow:

@
'copy' :: 'Borrow' bk α a %1 -> a
@

Note that 'copy' consumes a borrow linearly.
For 'Share'd borrows it doesn't matter because they are always introduced nonlinearly.
But for 'Mut'able borrows, we cannot use a 'copy'ed value multiple times as 'Mut's are always bound linearly.
To alleviate this problem, we also provide 'copyMut' that wraps copied value inside 'Ur':

@
'copyMut' :: 'Mut' α a %1 -> 'Ur' a
@

Precisely, if the type @a@ does not contain any mutable or foreign resources, it can be safely 'Copyable' out of borrows.
Some examples are (but not limited to):

    * Primitive types, such as 'Int', 'Bool', etc.
    * Immutable data structures, such as lists, tuples of them, etc. (but not mutable vectors, arrays, etc.)

For possibly mutable types, you can still 'clone' them out of borrows linearly.
This includes, for example, 'Data.Ref.Linear.Ref' or 'Data.Vector.Mutable.Linear.Borrow.Vector'.
-}

{- $splitting

You can do case-splitting on 'Borrow's - for example:

@
splitPair :: Alias ak α (a, b) %1 -> (Alias ak α a, Alias ak α b)
splitEither :: Alias ak α (Either a b) %1 -> Either (Alias ak α a) (Alias ak α b)
@

For other datatypes, you can use 'split' to split general parametric types into borrows.
It is morally an instance method of the 'DistributesAlias' class, and you can derive it using @anyclass@ derivation together with the 'Generics.Linear.TH.deriveGenericAnd1' macro.

We also provide experimental splitting on record types in "Data.Record.Linear.Borrow.Experimental.PatternMatch" and "Data.Record.Linear.Borrow.Experimental.Split".
-}
