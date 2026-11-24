{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Monad.Borrow.Pure.Lifetime.Token.Internal (
  -- * Tokens

  -- The newtype constructors stay in this module: see Note [Tokens are opaque values].
  Linearly (UnsafeLinearly),
  linearly,
  Now (UnsafeNow),
  SomeNow (..),
  newLifetime,
  newLifetime',
  EndToken (UnsafeEnd),
  endLifetime,

  -- * The end of a lifetime
  End (..),
  neverEnds,
  After (..),
  unAfter,
  withEnd,

  -- * Linear-only types
  LinearOnlyWitness (..),
  LinearOnly (..),
  withLinearly,
  withLinearly#,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Lifetime.Internal
import Data.Coerce.Directed.Unsafe
import Data.Functor.Linear qualified as Data
import Data.Kind (Constraint, Type)
import Data.Unrestricted.Linear
import GHC.Base (TYPE, UnliftedType, noinline, withDict)
import GHC.Exts qualified as GHC
import GHC.Stack (HasCallStack)
import GHC.TypeError (ErrorMessage (..), Unsatisfiable, unsatisfiable)
import Generics.Linear (Generic (..), Generic1 (..), V1)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Linear qualified as Unsafe

-- What every token is underneath: see Note [Tokens are opaque values].
type family TokenRep :: Type where

-- The one 'TokenRep' the library builds; nothing coerces it back or looks into it.
tokenRep :: TokenRep
{-# INLINE tokenRep #-}
tokenRep = unsafeCoerce ()

-- A 'TokenRep' owns nothing, so it may be dropped or kept unrestricted, as an 'EndToken' is; the bang evaluates it.
unrestrictRep :: TokenRep %1 -> Ur TokenRep
{-# INLINE unrestrictRep #-}
unrestrictRep = Unsafe.toLinear \ !rep -> Ur rep

-- Evaluate a token's representation and drop it.
forceRep :: TokenRep %1 -> ()
{-# INLINE forceRep #-}
forceRep rep = case unrestrictRep rep of
  Ur _ -> ()

type role Now nominal

-- | Witness that the lifetime @α@ is ongoing.

-- A newtype over 'TokenRep', never a type with a single nullary constructor: see Note [Tokens are opaque values].
newtype Now (α :: Lifetime) = UnsafeNowToken TokenRep

-- | Build a 'Now', or match an unrestricted one; drop a linearly bound one with @'consume' ('aff' now)@.
pattern UnsafeNow :: Now α
-- The bang is what evaluates the token: matching a newtype constructor does not.
pattern UnsafeNow <- UnsafeNowToken !_
  where
    UnsafeNow = UnsafeNowToken tokenRep

{-# COMPLETE UnsafeNow #-}

data SomeNow where
  MkSomeNow :: Now (Al i) %1 -> SomeNow

newLifetime :: Linearly %1 -> SomeNow
-- The same value is handed on, so that the 'Now' is as unknown as the token was: see Note [Tokens are opaque values].
newLifetime (UnsafeLinearlyToken !rep) = MkSomeNow (UnsafeNowToken rep)

newLifetime' :: Linearly %1 -> (forall ι. Now (Al ι) %1 -> a) %1 -> a
newLifetime' lin k =
  case newLifetime lin of
    MkSomeNow now -> k now

instance Affine (Now α) where
  -- The same value is handed on, so that the token stays as unknown as it was: see Note [Tokens are opaque values].
  aff (UnsafeNowToken !rep) = UnsafeAff (UnsafeNowToken rep)
  {-# INLINE aff #-}

instance LinearOnly (Now α) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

-- A Generic instance derived for this abstract type would drop the lifetime parameter.
instance
  ( Unsatisfiable
      ( 'ShowType (Now α)
          ':<>: 'Text " has no Generic instance:"
          ':$$: 'Text "it is an abstract lifetime token, whose constructor is hidden."
      )
  ) =>
  Generic (Now α)
  where
  type Rep (Now α) = V1
  from = unsatisfiable
  to = unsatisfiable

instance
  ( Unsatisfiable
      ( 'ShowType Now
          ':<>: 'Text " has no Generic1 instance:"
          ':$$: 'Text "it is an abstract lifetime token, whose constructor is hidden."
      )
  ) =>
  Generic1 Now
  where
  type Rep1 Now = V1
  from1 = unsatisfiable
  to1 = unsatisfiable

type role EndToken nominal

-- | Witness that the lifetime @α@ has ended.

-- A newtype over 'TokenRep', never a type with a single nullary constructor: see Note [Tokens are opaque values].
newtype EndToken (α :: Lifetime) = UnsafeEndToken TokenRep

{- | Build an 'EndToken', or match an unrestricted one.

A delimiter that discharges an 'After' with 'UnsafeEnd' can hand an owner back before the writes of its scope; take the token from the state thread instead, with @Control.Monad.Borrow.Pure.BO.Unsafe.endHere@ or @restoreWithEnd@.
-}
pattern UnsafeEnd :: EndToken α
-- The bang is what evaluates the token, which 'Control.Monad.Borrow.Pure.BO.Internal.reviveOwner' relies on: matching a newtype constructor does not.
-- See Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal" for the ordering a constant token forgoes.
pattern UnsafeEnd <- UnsafeEndToken !_
  where
    UnsafeEnd = UnsafeEndToken tokenRep

{-# COMPLETE UnsafeEnd #-}

instance (α >= β) => EndToken α <: EndToken β where
  subtype = UnsafeSubtype

-- A Generic instance derived for this abstract type would drop the lifetime parameter.
instance
  ( Unsatisfiable
      ( 'ShowType (EndToken α)
          ':<>: 'Text " has no Generic instance:"
          ':$$: 'Text "it is an abstract lifetime token, whose constructor is hidden."
      )
  ) =>
  Generic (EndToken α)
  where
  type Rep (EndToken α) = V1
  from = unsatisfiable
  to = unsatisfiable

instance
  ( Unsatisfiable
      ( 'ShowType EndToken
          ':<>: 'Text " has no Generic1 instance:"
          ':$$: 'Text "it is an abstract lifetime token, whose constructor is hidden."
      )
  ) =>
  Generic1 EndToken
  where
  type Rep1 EndToken = V1
  from1 = unsatisfiable
  to1 = unsatisfiable

-- | End a lifetime, returning the evidence that it has ended.
endLifetime :: Now (Al i) %1 -> (Ur (EndToken (Al i)))
-- Opaque, so that the token is the result of a call on the given 'Now' rather than a constant, and with the 'Now' that 'execBO' hands back depends on the lifetime's effects: see Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal".
-- The same value is handed on: see Note [Tokens are opaque values].
{-# OPAQUE endLifetime #-}
endLifetime (UnsafeNowToken rep) = case unrestrictRep rep of
  Ur rep' -> Ur (UnsafeEndToken rep')

-- | Witness that the lifetime @α@ has ended.
class End (α :: Lifetime) where
  endToken :: EndToken α

-- | Static lifetime lasts forever.
neverEnds :: (HasCallStack, End Static) => a
neverEnds = error "Unreachable: if you see this, you created an End Static in the internal code!"

{- |
Utility type to represent an object available after the lifetime @α@.

You can use 'Control.Applicative' and 'Control.Monad' instances to write 'After' conveniently.
-}
newtype After α a = After ((End α) => a)

type role After nominal representational

instance (α <= β, a <: b) => After α a <: After β b where
  subtype = UnsafeSubtype

instance (α <= β) => Subtype1 (After α) (After β) where
  subtype1 _ = UnsafeSubtype

unAfter :: (End α) => After α a %1 -> a
{-# INLINE unAfter #-}
unAfter (After r) = r

{- | Discharge an 'After' with the evidence that its lifetime has ended.

A bottom token, which anyone can write, fails at the first 'Control.Monad.Borrow.Pure.reclaim' it reaches, instead of handing back a resource whose borrows are still live.
-}
withEnd :: forall α r. EndToken α -> After α r %1 -> r
-- The token is left unevaluated here: see Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal".
{-# INLINE withEnd #-}
withEnd end (After a) = Unsafe.toLinear (withDict @(End α) end) a

instance Data.Functor (After α) where
  fmap f (After r) = After (f r)
  {-# INLINE fmap #-}

instance Control.Functor (After α) where
  fmap f (After r) = After (f r)
  {-# INLINE fmap #-}

instance Data.Applicative (After α) where
  pure a = After a
  {-# INLINE pure #-}
  After f <*> After r = After (f r)
  {-# INLINE (<*>) #-}
  liftA2 f (After a) (After b) = After (f a b)
  {-# INLINE liftA2 #-}

instance Control.Applicative (After α) where
  pure a = After a
  {-# INLINE pure #-}
  After f <*> After r = After (f r)
  {-# INLINE (<*>) #-}
  liftA2 f (After a) (After b) = After (f a b)
  {-# INLINE liftA2 #-}

instance Control.Monad (After α) where
  After r >>= k = After (unAfter (k r))
  {-# INLINE (>>=) #-}
  After r >> After a = After (r `lseq` a)
  {-# INLINE (>>) #-}

-- | Witness that the current computation is in a linear context.

-- A newtype over 'TokenRep', never a type with a single nullary constructor: see Note [Tokens are opaque values].
newtype Linearly = UnsafeLinearlyToken TokenRep

-- | Build a 'Linearly', or match an unrestricted one; drop a linearly bound one with 'consume'.
pattern UnsafeLinearly :: Linearly
-- The bang is what evaluates the token: matching a newtype constructor does not.
pattern UnsafeLinearly <- UnsafeLinearlyToken !_
  where
    UnsafeLinearly = UnsafeLinearlyToken tokenRep

{-# COMPLETE UnsafeLinearly #-}

{-
Note [Tokens are opaque values]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'Linearly', 'Now' and 'EndToken' are newtypes over 'TokenRep', an empty closed type family of this module.
None of them may become a type with a single nullary constructor, or a newtype over one such as @()@.

A token does its job only while the optimizer does not know which value it is.
An allocation that takes a 'Linearly', such as @newRef# seed lin@, is a pure expression: two of them with equal arguments are merged by common-subexpression elimination, and one whose arguments are all constants is floated to the top level and shared by every call.
The library hands each token out as the result of a call the optimizer cannot see into ('linearly', 'withLinearly', 'dup2', 'askLinearly' and 'nowStatic' are @NOINLINE@ and applied through 'noinline', and 'withLinearly#' applies its lambda through 'noinline'), so two allocations that look alike still differ in their token.
Inside those functions the tokens are constants; the call is what hides them.
But forcing a value of a type whose only constructor is nullary tells GHC which value it is, and GHC then puts the constructor in place of the variable.
User code forces a token without meaning to: a bang pattern, linear-base's @$!@, a strict field of a record, or a module compiled with @Strict@ does it.
This happened: after @case dup2 lin of (!l1, !l2)@, @Ref.new seed l1@ and @Ref.new seed l2@ became one reference, and a function that allocated a reference or a vector from a forced token handed out the same one on every call (the kernels of @Control.Monad.Borrow.Pure.Lifetime.TokenSpec@).
A forced 'Now' made the 'EndToken' that 'endLifetime' derives from it a top-level constant shared by every lifetime, and a forced 'EndToken' let a reclaimed owner be read before the scope's writes (Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal").
No pragma on the library's functions prevents this: with such tokens and every @NOINLINE@ of the library turned into @OPAQUE@, the same kernels failed, since the allocation is a call on constants whatever GHC knows about its callee.

'TokenRep' has no equations, so GHC knows no constructor of it: a @case@ on one has only a default alternative, and forcing a token tells GHC nothing, so two forced tokens stay two different variables.
At run time a token is @()@, and nothing is allocated for it.
This relies on GHC not treating 'TokenRep' as a type without values, which would make the code after a forced token unreachable; the kernels above would fail if it did, and they pass on GHC 9.10, 9.12 and 9.14.

A function that receives a token forces it or hands it on, unless it is applied through 'noinline'.
Worker/wrapper drops an argument that its function does not use at all and puts a constant in its place, so a function that ignored its token would receive a constant from every caller.
A token that is forced is passed on as it is, since it is not a constructor that a worker could take apart and rebuild.

A function that makes a token from another hands the same value on, as 'newLifetime', 'endLifetime' and 'aff' on 'Now' do, so that its result is as unknown as its argument was.
One that built its result from 'tokenRep' would return a constant however unknown its argument was.
This happened as well: 'newLifetime' built its 'Now' from a constant, so a 'Control.Monad.Borrow.Pure.runBO' whose action has no free variables depended on nothing but constants, and GHC made the whole run one top-level constant.
Every call of a function that allocated a reference with such a run got the same reference.
A function that returns two tokens cannot hand its argument on twice, since two tokens that are one variable are one expression, and allocations made with them merge: it must be @NOINLINE@ and applied through 'noinline', as 'dup2' is.
Only such a function, a lambda applied through 'noinline', as in 'withLinearly#', or an @OPAQUE@ function that takes no token, as 'reviveAliasWithEnd#' and 'endHere' in "Control.Monad.Borrow.Pure.BO.Internal" are, may build a token from 'tokenRep'.

The newtype constructors are not exported, even to the rest of the library.
The three tokens have one representation, so wherever the constructors are in scope 'Data.Coerce.coerce' and 'Data.Coerce.Directed.upcast' turn a 'Now' into the 'EndToken' of its own lifetime, or an 'EndToken' into a 'Linearly'.
The patterns 'UnsafeLinearly', 'UnsafeNow' and 'UnsafeEnd' build a token and match an unrestricted one.
GHC does not let a pattern synonym match a linearly bound value, so a linear 'Linearly' is dropped with 'consume', a 'Now' with @'consume' ('aff' now)@, and a linear 'EndToken' is used up by @withEndL@ in "Control.Monad.Borrow.Pure.BO.Unsafe".

Forcing a token costs a call.
GHC cannot rule out that a value of type 'TokenRep' is a function, so it evaluates one through the runtime's generic entry, @stg_ap_0_fast@, rather than by testing the tag of the pointer.
On aarch64-darwin with GHC 9.12.4 that is about 0.5 ns for each token forced: a run forces a handful, an allocation one, and a crossing of a scope one only if its 'After' reclaims.
The first fix, a constructor with a lazy field that nothing reads, was forced inline but allocated the 'Now' and the 'EndToken' of every run, 32 bytes, and left a box that worker/wrapper took apart and rebuilt from a constant unless every function passed the field on.
-}

linearly :: (Movable a) => (Linearly %1 -> a) %1 -> a
{-# NOINLINE linearly #-}
linearly = GHC.noinline \f ->
  case move (f UnsafeLinearly) of
    Ur !x -> x

data LinearOnlyWitness a = UnsafeLinearOnly

-- Not phantom, so that deriving via cannot reuse an instance for an unrelated type.
type role LinearOnlyWitness representational

-- | A type that can only be introduced and consumed within a linear context, and hence can be regarded as bearing the 'Linearly' token inside.
type LinearOnly :: forall rep. TYPE rep -> Constraint
class LinearOnly a where
  linearOnly :: LinearOnlyWitness a

-- | Obtain a 'Linearly' witness token from a 'LinearOnly' value, which lives only inside a linear context.
withLinearly :: forall a. (LinearOnly a) => a %1 -> (Linearly, a)
{-# NOINLINE withLinearly #-}
-- Forced outside the lambda: one evaluation per dictionary, not per application.
withLinearly = case linearOnly @_ @a of
  UnsafeLinearOnly -> noinline \ !a -> (UnsafeLinearly, a)

-- | 'withLinearly' for an unlifted type.
withLinearly# :: forall (a :: UnliftedType). (LinearOnly a) => a %1 -> (# Linearly, a #)
withLinearly# = case linearOnly @_ @a of
  UnsafeLinearOnly -> noinline \ !a -> (# UnsafeLinearly, a #)

instance LinearOnly Linearly where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

-- A Generic instance derived for this abstract type would hide the linearity token.
instance
  ( Unsatisfiable
      ( 'ShowType Linearly
          ':<>: 'Text " has no Generic instance:"
          ':$$: 'Text "it is an abstract linearity token, whose constructor is hidden."
      )
  ) =>
  Generic Linearly
  where
  type Rep Linearly = V1
  from = unsatisfiable
  to = unsatisfiable

instance Consumable Linearly where
  consume (UnsafeLinearlyToken rep) = forceRep rep
  {-# INLINE consume #-}

instance Dupable Linearly where
  -- NOTE: without inlining, GHC optimizer (especially, full-laziness and demand analysis)
  -- can eliminate duplicated 'Linearly's too eagerly, ruining the state-threading,
  -- and result in resource corruption in some cases.
  -- Such optimization can manifest when, for example, one duplicates 'Linearly'
  -- tokens multiple times and feed them to different allocation functions.
  -- Although we are not able to detect the exact situation, but we believe that
  -- GHC optimizer then eliminates every invocation on bulk allocation functions
  -- into a single one, which introduces unintended reuse of linear resources.
  -- Hence, we must instruct GHC not to inline this function and force
  dup2 = GHC.noinline \(UnsafeLinearlyToken rep) -> case forceRep rep of
    () -> (UnsafeLinearly, UnsafeLinearly)
  {-# NOINLINE dup2 #-}
