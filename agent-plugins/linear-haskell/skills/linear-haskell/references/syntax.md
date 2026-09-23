# Syntax for linear code

Every behaviour marked *verified* below was checked by compiling with GHC 9.10.3 and 9.12.4.

## Where multiplicities come from

- Arrow multiplicities are always **declared, never inferred**: a function without a signature is unrestricted.
- `a %1 -> b` is linear, `a -> b` (= `a %Many -> b`) is unrestricted, and `a %m -> b` is multiplicity-polymorphic (experimental: expect inference to be unreliable, and there is no multiplicity multiplication).
  With `UnicodeSyntax`, `a ⊸ b` means `a %1 -> b`.
- Variables bound by a lambda or an equation take their multiplicity from the signature.
  There is no syntax for annotating a lambda binder (`\(%1 x) -> …` is not supported); only `let`/`where` bindings can be annotated.
- `LinearTypes` implies `MonoLocalBinds`.

## `let` and `where` (GHC ≥ 9.10)

A binding can be annotated `%1` (or `%Many`) only if all of these hold:

- it is not top-level;
- it is not recursive;
- it is a pattern binding, including a plain variable: write `let %1 f = \x -> u`, not `let %1 f x = u`;
- the pattern is a variable or is strict; `x@y` and `(x)` do not count as variables.

Without an annotation, GHC infers:

- top-level bindings: `Many`;
- recursive bindings: `Many`;
- lazy non-variable pattern bindings: `Many` — so `let (x, y) = rhs` is always unrestricted, while `let !(x, y) = rhs` can be linear (*verified*: no `%1` needed);
- everything else, including `let f x1 … xn = rhs` and `where` bindings: inferred from the term (*verified* for `where y = x * 2` with a linear `x`).

A pattern is strict (and can therefore be linear) when it is a `case` alternative without `~`, a `let` pattern with `!`, a `let` pattern under `Strict` without `~`, or nested inside a strict pattern.

| Binding (without `Strict`) | Result |
| --- | --- |
| `let %1 x = u` | linear |
| `let %1 !x = u` | linear |
| `let %1 (x, y) = u` | **rejected**: lazy pattern |
| `let %1 !(x, y) = u` | linear |
| `let %Many (x, y) = u` | unrestricted |
| `let (x, y) = u` | inferred unrestricted |
| `case u of (x, y) -> …` | can be inferred linear |
| `case u of ~(x, y) -> …` | inferred unrestricted |

Writing `%Many` (or `%'Many`) needs the constructor in scope: `import GHC.Exts (Multiplicity (..))`, plus `DataKinds` for the quoted form.

## Shadowing and recursive `let`

Linear code conventionally reuses a resource's name after each operation (`arr`, `arr`, `arr`, …) with `-Wno-name-shadowing`, because a consumed linear variable must not be referenced again and a fresh name would leave the dead one in scope.
`let` and `where` bindings are recursive, so shadowing inside them does not refer to the old value:

```haskell
-- WRONG: the arr on the right-hand side is the arr being defined.
let %1 !(Ur v, arr) = Array.read arr i
```

*Verified* behaviour:

- If the shadowed variable is linear, GHC rejects the binding with `Couldn't match type 'Many' with 'One' arising from multiplicity of 'arr'`, because recursive bindings are always unrestricted.
  The message does not mention recursion.
- If the shadowed variable is unrestricted (for example a value obtained from `Ur`), the binding compiles and loops at run time.
  The only hint is `-Wunused-matches` reporting the outer binder as unused.

Use one of these instead:

```haskell
-- case: never recursive.
case Array.read arr i of
  (Ur v, arr) -> …

-- let with fresh names.
let %1 !(Ur v, arr1) = Array.read arr0 i
 in …

-- a qualified do block: '<-' is not recursive (a 'let' statement inside it still is).
Control.do
  (Ur v, arr) <- …
```

## A `do` notation for pure data flow

For pure code that threads resources, a module whose bind is reverse application gives `do` syntax without any monad.
pure-borrow ships it as `Control.Syntax.DataFlow`; projects that do not depend on pure-borrow can copy it:

```haskell
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Syntax.DataFlow ((>>=), (>>), (*>), pure, return, (<*>), (<*)) where

import Prelude.Linear qualified as PL

(>>=) :: a %1 -> (a %1 -> b) %1 -> b
a >>= b = b a

(>>) :: (PL.Consumable a) => a %1 -> b %1 -> b
a >> b = PL.consume a PL.& \() -> b

(*>) :: (PL.Consumable a) => a %1 -> b %1 -> b
(*>) = (>>)

(<*) :: (PL.Consumable b) => a %1 -> b %1 -> a
a <* b = PL.consume b PL.& \() -> a

pure :: a %1 -> a
pure = PL.id

return :: a %1 -> a
return = PL.id

(<*>) :: (a %1 -> b) %1 -> a %1 -> b
f <*> a = f a
```

```haskell
import Control.Syntax.DataFlow qualified as DataFlow

swapFirstTwo :: Array Int %1 -> Array Int
swapFirstTwo arr = DataFlow.do
  (Ur a, arr) <- Array.read arr 0   -- rejected by GHC 9.10; see below
  (Ur b, arr) <- Array.read arr 1
  arr <- Array.write arr 0 b
  Array.write arr 1 a
```

The module deliberately does not export `fail`, which triggers the GHC 9.10 bug described next.

## `QualifiedDo`

- `M.do` desugars with `M.>>=`, `M.>>`, and, for patterns GHC considers failable, `M.fail`.
- `QualifiedDo` does not rewrite anything else, so write `Control.pure`/`Control.return` explicitly.
- In `Control.do`, a statement without `<-` must have type `m ()`: `(>>) :: m () %1 -> m a %1 -> m a` cannot drop a value.
  Bind other results and consume them.
- `x <- act` binds `x` linearly, because the continuation of `(>>=)` is linear.

### The GHC 9.10 `fail` bug

On GHC 9.10 (*verified* on 9.10.3), a constructor pattern on the left of `<-` — `Ur x`, `(Ur x, y)`, a single-constructor `MkT a b`, or a GADT-syntax constructor — makes the renamer demand `M.fail`, even though the pattern cannot fail.
If the module does not export `fail`, compilation stops with:

```text
Not in scope: 'DataFlow.fail'
NB: the module 'Control.Syntax.DataFlow' does not export 'fail'.
```

Variables, tuples of variables, and bang patterns on them (`!(a, b)`) are unaffected.
`Control.do` from linear-base is unaffected in practice: `Control.Functor.Linear` exports `fail`, and no `MonadFail` instance is actually required for an irrefutable pattern.
GHC 9.12 accepts all of these patterns.

Two workarounds:

1. Drop GHC 9.10 support, if the user allows it.
2. Bind a variable and match it with `case`:

   ```haskell
   (len, v) <- size v
   case len of
     Ur n -> …
   ```

## Conditionals and patterns

| Construct with a linear variable | Status |
| --- | --- |
| `if n <= 0 then … else …` | accepted (*verified*); `n` is consumed by the condition |
| `if n < 0 then 0 else n` | rejected: `n` used twice; `dup` it first |
| guards `f n | n <= 0 = …` | rejected: guards use `n` unrestrictedly |
| `MultiWayIf` | rejected, like guards |
| `case n <= 0 of True -> …; False -> …` | accepted |
| `\case` with a linear argument | accepted |
| lazy pattern `~(a, b)` | unrestricted |
| `@`-pattern, view pattern | not linear |
| `_` on a linear field | rejected; consume the field instead |
| linear pattern synonyms | unsupported |
| linear types inside Template Haskell quotes | "will probably not work" (GHC user guide) |

```haskell
clamp :: Int %1 -> Int
clamp n = case dup n of
  (n, n') -> if n < 0 then n' `lseq` 0 else n'
```

A `_` on a linear field is often reported at the binder of the whole scrutinee, not at the `_`: if GHC blames a variable you only pattern-match, look for a discarded linear field inside the pattern.

## Data declarations

- Fields of ordinary `data` declarations, including records, are linear by default, even without `LinearTypes`.
- Use GADT syntax to choose per field: `MkT :: a -> b %1 -> T a b` has an unrestricted first field and a linear second one.
- Multiplicity-polymorphic fields are possible (`MkT3 :: a %m -> T3 a m`), but such constructors are not generalised.
- `newtype` fields must be linear.
- The selector of a record with a single linear field is currently unrestricted.
- GHC ≥ 9.14: record syntax accepts multiplicities, `data R = MkR { x %'Many :: A, y :: B }` gives `MkR :: A %'Many -> B %1 -> R` (this needs `DataKinds` and `import GHC.Exts (Multiplicity (..))`); the selectors are unaffected.

## GHC version matrix

| GHC | Linear-types change relevant to users |
| --- | --- |
| 9.0 | `LinearTypes` introduced. |
| 9.10.1 | Linear `let` and `where` bindings, `%1`/`%Many` annotations on them. |
| 9.10.2 | Fixed linearity of strict unit patterns in local `let` bindings. |
| 9.10.x | `fail` bug for constructor patterns in qualified `do` (above). |
| 9.12.1 | The implicit `forall` of `a %m -> b` is now ordered `forall a m b` (matters for visible type application). |
| < 9.12.3 | Evaluating some linear programs in GHCi or the HLS eval plugin segfaults; compiled code is fine. |
| 9.14.1 | Record syntax accepts field multiplicities. |
| 10.0 (upcoming) | Multiplicities use the new `Modifiers` syntax; `Int %m -> Bool` requires `m`'s kind to be known, e.g. `%(m :: Multiplicity)`; `-XNoModifiers` restores the old behaviour. |

Unless the project says otherwise, write code that compiles on both GHC 9.10.3 and 9.12.4: GADT syntax for mixed fields, and `case` instead of constructor patterns in data-flow `do` blocks.
