# Troubleshooting linear code

## `Couldn't match type 'Many' with 'One' arising from multiplicity of 'x'`

`x` is linear but is used zero times, more than once, or in a position GHC treats as unrestricted.
Check, in this order:

1. **A branch that forgets `x`** (often an error or early-return branch).
   Consume it: `x \`lseq\` …`, or apply the error to it: `error "msg" x`.
2. **Two uses of `x`**, for example in a condition and in a branch.
   `dup` it first, or restructure so the operation returns `x`.
3. **A shadowed name on the right-hand side of its own `let`/`where`.**
   `let %1 !(a, x) = f x` is recursive; use `case`, a qualified `do`, or fresh names.
4. **A lazy pattern**: `let (a, b) = …` or `~(a, b)`.
   Use `let !(a, b) = …` or `case`.
5. **A guard or `MultiWayIf` condition mentioning `x`.**
   Use `if` or `case` on a `Bool`.
6. **A `_` on a linear field.**
   GHC may blame the binder of the whole scrutinee instead of the `_`.
   Plain `data` fields are all linear, even `Int`s; consume them.
7. **An unrestricted consumer**: an ordinary Prelude function (`length`, `show`, `==`, `map`), a `where` helper without a linear signature, or a closure that is itself used unrestrictedly.
   Use the `Prelude.Linear`/`Data.*.Linear` versions, give the helper a `%1 ->` signature and pass `x` explicitly, or `move` the value first if it is `Movable`.
8. **An `@`-pattern or view pattern** — neither is linear.
9. **A value stored in `Ur`**: `Ur`'s field is unrestricted; use `move`.

GHC usually reports this error against the whole equation, and it can hide the other type errors of that definition; if the checklist does not find the cause quickly, bisect as described in the SKILL.md section "Diagnosing multiplicity errors".

## `… arising from a non-linear pattern … (non-variable lazy pattern aren't linear)`

A lazy pattern binding (`let (x, y) = …`, or `~(x, y)`) receives a linear value.
Make it strict (`let !(x, y) = …`, optionally with `%1`) or use `case`.

## `… (non-variable pattern bindings that have been generalised aren't linear)`

A pattern binding was generalised (it got a polymorphic or constrained type) or is recursive, typically because a shadowed name appears on its own right-hand side.
Use fresh names, give the binding a monomorphic type, or use `case`.

## `Not in scope: 'M.fail'` in a qualified `do`

The GHC 9.10 bug: a constructor pattern on the left of `<-` in a `do` block whose module does not export `fail`.
Bind a variable and `case` on it, or require GHC ≥ 9.12.
See `references/syntax.md`.

## A custom `Unsatisfiable` message

The code asked for an instance the library deliberately bans, typically `Movable` for a mutable type (or, with pure-borrow, `Copyable`).
Do not add the instance: consume, borrow, or copy the value through the API the message points to.

## Linear functions where unrestricted ones are expected (and vice versa)

- Passing `f :: a %1 -> b` where `a -> b` is expected is always safe: eta-expand (`\x -> f x`) or use `forget f`.
- Passing `g :: a -> b` where `a %1 -> b` is expected is unsafe in general.
  `Unsafe.Linear.toLinear` (`toLinear2`, `toLinear3`, …) casts it; each use is a proof obligation that the function really is linear, so keep it out of application code.

## The program compiles but loops

A shadowed unrestricted variable on the right-hand side of its own `let` or `where` (`let (a, s) = step s`).
Look for `-Wunused-matches` warnings on the outer binder.

## GHCi or the HLS eval plugin crashes

GHC < 9.12.3 can segfault when *interpreting* some linear programs; compiled code is fine.
Use GHC ≥ 9.12.3 for the REPL, or test with a compiled test-suite.

## Bisecting a stubborn multiplicity error

Use the procedure and example in the SKILL.md section "Diagnosing multiplicity errors".
Two practical notes:

- In a `do` block, stub the tail after the last statement you trust, e.g. `(Ur n, arr) <- step arr` followed by `undefined n arr`.
- Once the multiplicity error is gone, the remaining type errors reappear, and the language server can show the types of sub-expressions again.
