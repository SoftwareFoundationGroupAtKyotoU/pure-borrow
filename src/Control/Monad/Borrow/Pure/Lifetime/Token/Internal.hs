{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Monad.Borrow.Pure.Lifetime.Token.Internal (
  module Control.Monad.Borrow.Pure.Lifetime.Token.Internal,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Lifetime.Internal
import Data.Coerce.Directed.Unsafe
import Data.Functor.Linear qualified as Data
import Data.Kind (Constraint)
import Data.Unrestricted.Linear
import GHC.Base (TYPE, UnliftedType, noinline, withDict)
import GHC.Exts qualified as GHC
import GHC.Stack (HasCallStack)
import GHC.TypeError (ErrorMessage (..), Unsatisfiable, unsatisfiable)
import Unsafe.Linear qualified as Unsafe

type role Now nominal

{- | Witness that the lifetime @α@ is ongoing.

"Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe" exports its constructor as 'UnsafeNowToken', and the pattern 'UnsafeNow'.
-}

-- A data type with a lazy field that nothing reads, never a newtype or a nullary constructor: see Note [Tokens carry a field].
data Now (α :: Lifetime) where
  UnsafeNowToken :: () -> Now α

-- | Construct a 'Now', or match an unrestricted one; match a linearly bound one with @'UnsafeNowToken' _@.
pattern UnsafeNow :: Now α
pattern UnsafeNow <- UnsafeNowToken _
  where
    UnsafeNow = UnsafeNowToken ()

{-# COMPLETE UnsafeNow #-}

data SomeNow where
  MkSomeNow :: Now (Al i) %1 -> SomeNow

newLifetime :: Linearly %1 -> SomeNow
-- The field is passed on, so that the 'Now' is as unknown as the token was: see Note [Tokens carry a field].
newLifetime (UnsafeLinearlyToken field) = MkSomeNow (UnsafeNowToken field)

newLifetime' :: Linearly %1 -> (forall ι. Now (Al ι) %1 -> a) %1 -> a
newLifetime' lin k =
  case newLifetime lin of
    MkSomeNow now -> k now

instance Affine (Now α) where
  -- The field is passed on, so that the token stays as unknown as it was: see Note [Tokens carry a field].
  aff (UnsafeNowToken field) = UnsafeAff (UnsafeNowToken field)
  {-# INLINE aff #-}

instance LinearOnly (Now α) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

type role EndToken nominal

{- | Witness that the lifetime @α@ has ended.

Its constructor is exported only from "Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe", as 'UnsafeEndToken', and the pattern 'UnsafeEnd'.
A delimiter that applies 'UnsafeEnd' directly forgoes the ordering that Note [Owners handed back by reclaim] in @Control.Monad.Borrow.Pure.BO.Internal@ relies on; take the token from the state thread with @Control.Monad.Borrow.Pure.BO.Unsafe.endHere@ or @restoreWithEnd@ instead.
-}

-- A data type with a lazy field that nothing reads, never a newtype or a nullary constructor: see Note [Tokens carry a field].
data EndToken (α :: Lifetime) where
  UnsafeEndToken :: () -> EndToken α

-- | Construct an 'EndToken', or match an unrestricted one; match a linearly bound one with @'UnsafeEndToken' _@.
pattern UnsafeEnd :: EndToken α
pattern UnsafeEnd <- UnsafeEndToken _
  where
    UnsafeEnd = UnsafeEndToken ()

{-# COMPLETE UnsafeEnd #-}

instance (α >= β) => Subtype (EndToken α) (EndToken β) where
  subtype = UnsafeSubtype

-- | End a lifetime, returning the evidence that it has ended.
endLifetime :: Now (Al i) %1 -> (Ur (EndToken (Al i)))
-- Opaque, so that the token is the result of a call on the given 'Now' rather than a constant, and with the 'Now' that 'execBO' hands back depends on the lifetime's effects: see Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal".
-- The field is passed on although the function is opaque: see Note [Tokens carry a field].
{-# OPAQUE endLifetime #-}
endLifetime (UnsafeNowToken field) = Ur (UnsafeEndToken field)

{- | Evidence that the lifetime @α@ has ended.

The safe modules export only its synonym 'End', so no instance can be written against them: the only evidence is what 'withEnd' supplies.
-}
class Ended (α :: Lifetime) where
  endToken :: EndToken α

{- | Witness that the lifetime @α@ has ended.

It is a synonym, so that the safe modules can export it without letting anyone write an instance: the evidence comes only from 'withEnd', applied to the 'EndToken' that ending the lifetime produced.
-}
type End = Ended

-- | Static lifetime lasts forever.
neverEnds :: (HasCallStack, End Static) => a
neverEnds = error "Unreachable: if you see this, you created an End Static in the internal code!"

{- |
Utility type to represent an object available after the lifetime @α@.

You can use 'Control.Applicative' and 'Control.Monad' instances to write 'After' conveniently.
-}
newtype After α a = After ((End α) => a)

instance (α <= β, a <: b) => Subtype (After α a) (After β b) where
  subtype = UnsafeSubtype

unAfter :: (End α) => After α a %1 -> a
{-# INLINE unAfter #-}
unAfter (After r) = r

{- | Discharge an 'After' with the evidence that its lifetime has ended.

A bottom token, which anyone can write, fails at the first 'Control.Monad.Borrow.Pure.reclaim' it reaches, instead of handing back a resource whose borrows are still live.
See Note [Owners handed back by reclaim] in @Control.Monad.Borrow.Pure.BO.Internal@ for why 'withEnd' itself leaves the token alone.
-}
withEnd :: forall α r. EndToken α -> After α r %1 -> r
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

{- | Witness that the current computation is in a linear context.

"Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe" exports its constructor as 'UnsafeLinearlyToken', and the pattern 'UnsafeLinearly'.
-}

-- A data type with a lazy field that nothing reads, never a newtype or a nullary constructor: see Note [Tokens carry a field].
data Linearly where
  UnsafeLinearlyToken :: () -> Linearly

-- | Construct a 'Linearly', or match an unrestricted one; match a linearly bound one with @'UnsafeLinearlyToken' _@.
pattern UnsafeLinearly :: Linearly
pattern UnsafeLinearly <- UnsafeLinearlyToken _
  where
    UnsafeLinearly = UnsafeLinearlyToken ()

{-# COMPLETE UnsafeLinearly #-}

{-
Note [Tokens carry a field]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
'Linearly', 'Now' and 'EndToken' each have a single constructor with a lazy field that nothing reads.
None of them may become a nullary constructor, and none a newtype.

A token does its job only while the optimizer does not know which value it is.
An allocation that takes a 'Linearly', such as @newRef# seed lin@, is a pure expression: two of them with equal arguments are merged by common-subexpression elimination, and one whose arguments are all constants is floated to the top level and shared by every call.
The library hands each token out as the result of a call the optimizer cannot see into ('linearly', 'withLinearly', 'dup2', 'askLinearly' and 'nowStatic' are @NOINLINE@ and applied through 'noinline', and 'withLinearly#' applies its lambda through 'noinline'), so two allocations that look alike still differ in their token.
Inside those functions the tokens are constants; the call is what hides them.
But forcing a value of a type whose only constructor is nullary tells GHC which value it is, and GHC then puts the constructor in place of the variable.
User code forces a token without meaning to: a bang pattern, linear-base's @$!@, a strict field of a record, or a module compiled with @Strict@ does it.
This happened: after @case dup2 lin of (!l1, !l2)@, @Ref.new seed l1@ and @Ref.new seed l2@ became one reference, and a function that allocated a reference or a vector from a forced token handed out the same one on every call (the kernels of @Control.Monad.Borrow.Pure.Lifetime.TokenSpec@).
A forced 'Now' made the 'EndToken' that 'endLifetime' derives from it a top-level constant shared by every lifetime, and a forced 'EndToken' let a reclaimed owner be read before the scope's writes (Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal").

With a field, forcing a token reveals only @UnsafeLinearlyToken x@, where @x@ is as unknown as the token was, so two forced tokens stay two different expressions.
A newtype would not do, since its field is the token itself.

A function that takes a token apart and returns another must pass the field on rather than build a new token from constants, as 'newLifetime', 'endLifetime' and 'aff' on 'Now' do; otherwise its result is a constant however unknown its argument was.
That holds for a function that returns one token: one that returns two must itself be @NOINLINE@ and applied through 'noinline' instead, as 'dup2' is, since two tokens with the same field are one expression, and allocations made with them merge.
This happened as well: 'newLifetime' built its 'Now' from constants, so a 'Control.Monad.Borrow.Pure.runBO' whose action has no free variables depended on nothing but constants, and GHC made the whole run one top-level constant.
Every call of a function that allocated a reference with such a run got the same reference.

@OPAQUE@ does not exempt a function from passing the field on.
It hides the function's body, but not its demand signature, and a function that ignores the field says so there.
GHC then splits a caller that takes a token apart and rebuilds it, as @endLifetime (unaff (aff now))@ does, into a worker that builds the token from nothing, a constant: with 'endLifetime' building its 'EndToken' from constants, every lifetime ended that way got one end token.
Only a function that neither takes a token apart nor shows its demand on one may build a token from constants: one that is @NOINLINE@ and applied through 'noinline', whose demand signature GHC does not see, as the ones above are, or an @OPAQUE@ one that takes no token, as 'reviveAliasWithEnd#' and 'endHere' in "Control.Monad.Borrow.Pure.BO.Internal" are.

The field is unrestricted, which takes the GADT syntax, so that a linearly bound token can be matched as @UnsafeLinearlyToken _@.
The patterns 'UnsafeLinearly', 'UnsafeNow' and 'UnsafeEnd' keep the old names of the constructors for building a token and for matching an unrestricted one, but GHC does not let a pattern synonym match a linearly bound value.

The cost is a token box where there was a static one.
Every run of the 'Control.Monad.Borrow.Pure.runBO' family allocates the 'Now' that 'newLifetime' builds, 16 bytes, and the 'EndToken' and its 'Ur' that 'endLifetime' returns, 32 bytes, and a run suspended behind a lazy owner keeps the field on its stack.
A strict field of type 'Linearly' in a user's record is unpacked to the token's own field, and passing the token on from there allocates the token again, 16 bytes.
0.1.0.0 allocated nothing in either place only because its tokens were constants, the very bug.
-}

linearly :: (Movable a) => (Linearly %1 -> a) %1 -> a
{-# NOINLINE linearly #-}
linearly = GHC.noinline \f ->
  case move (f UnsafeLinearly) of
    Ur !x -> x

data LinearOnlyWitness a = UnsafeLinearOnly

-- A phantom role would let DerivingVia coerce an existing 'LinearOnly'
-- instance to any type at all, without the missing-method warning that
-- otherwise guards user-written instances.
-- A representational role still admits newtype deriving, which is sound: a
-- newtype's non-bottom values are exactly those of the type it wraps.
type role LinearOnlyWitness representational

{- | A (non-bottom) value of the type @a@ can only live in a linear context.

An instance is a claim that 'withLinearly' relies on to mint a 'Linearly', so the safe modules offer only ways that inherit an existing claim: @deriving newtype@, or @deriving via@ a representationally equal type that has one.
An instance with no method, including @deriving anyclass@, is rejected at compile time; writing the method takes "Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe" and the obligation with it.

A record of resources needs no instance: call 'withLinearly' on one of its fields that has one, and rebuild the record.
An instance written for it through "Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe" must ensure that every constructor holds, in a strict field, a value of a type that is 'LinearOnly' itself.
Otherwise a record holding 'undefined' in place of its resource, or 'Nothing' in a field of type @Maybe (Ref Int)@, would mint a 'Linearly' outside any linear context.
-}
type LinearOnly :: forall rep. TYPE rep -> Constraint
class LinearOnly a where
  linearOnly :: LinearOnlyWitness a
  default linearOnly ::
    ( Unsatisfiable
        ( 'Text "A LinearOnly instance must come from a type that already has one."
            ':$$: 'Text "Derive it with `deriving newtype`, or with `deriving via` a representationally equal type."
            ':$$: 'Text "For a record of resources, call `withLinearly` on one of its LinearOnly fields instead, and rebuild the record."
            ':$$: 'Text "Otherwise, write `linearOnly = UnsafeLinearOnly` through Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe,"
            ':$$: 'Text "which asserts that no non-bottom value of the type can exist outside a linear context."
        )
    ) =>
    LinearOnlyWitness a
  linearOnly = unsatisfiable

{- | Mint a 'Linearly' from a value that can only live in a linear context.

It fails rather than mint a token when the 'LinearOnly' instance holds a deferred type error or loops, as one derived via its own type does.
-}
withLinearly :: forall a. (LinearOnly a) => a %1 -> (Linearly, a)
{-# NOINLINE withLinearly #-}
-- The witness is forced outside the lambda, so that an abstract dictionary costs one evaluation rather than a closure per application.
withLinearly = case linearOnly @_ @a of
  UnsafeLinearOnly -> noinline \ !a -> (UnsafeLinearly, a)

-- | 'withLinearly' for an unlifted type.
withLinearly# :: forall (a :: UnliftedType). (LinearOnly a) => a %1 -> (# Linearly, a #)
withLinearly# = case linearOnly @_ @a of
  UnsafeLinearOnly -> noinline \ !a -> (# UnsafeLinearly, a #)

instance LinearOnly Linearly where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance Consumable Linearly where
  consume = \(UnsafeLinearlyToken _) -> ()
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
  dup2 = GHC.noinline \(UnsafeLinearlyToken _) -> (UnsafeLinearly, UnsafeLinearly)
  {-# NOINLINE dup2 #-}
