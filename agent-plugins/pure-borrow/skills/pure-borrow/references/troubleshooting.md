# Troubleshooting pure-borrow code

The messages below were produced by GHC 9.12.4 with pure-borrow 0.1; wording varies slightly between compilers.
For general multiplicity errors, see the linear-haskell skill's troubleshooting reference first.

## `Couldn't match type 'Many' with 'One' arising from multiplicity of 'mvec'`

A `Mut` is used twice, or not at all, somewhere in the equation.
Typical causes:

- **Using a `Mut` after `share`**: `share mvec` consumes `mvec` for the rest of the lifetime.
  Use `sharing`/`sharing_` (or `(<$~)`) to read temporarily and get the `Mut` back.
- **Passing the same `Mut` to both sides of `parBO`.**
  Split it first (`VL.splitAt`, `splitPair`, `split`), usually inside `reborrowing_` so the whole borrow comes back afterwards.
- **Forgetting the `Mut` returned by an operation** (`VL.modify 0 f mvec` whose result is not bound).
- **Not consuming a `Mut` at the end**: add `Control.pure (consume mvec)`.

## `Couldn't match type 'Many' with 'One' arising from a non-linear pattern '_'`

`_ <- VL.modify 0 f mvec` tries to discard a linear result.
Bind it and `consume` it (`mvec <- …; Control.pure (consume mvec)`), or use `Control.void` when the value is `Consumable` and you really want to drop it.

## `Couldn't match type: 'Lend α with: 'Borrow bk0 α0 arising from a use of 'consume'`

A `Lend` cannot be consumed: only `Mut` and `Share` are `Consumable`.
Reclaim it inside `pureAfter`/`After` (`reclaim lend`) and then consume or convert the owned value.

## `No instance for 'End α' arising from a use of 'reclaim'`

`reclaim` is only available once the lifetime has ended.
Move the expression into `pureAfter (…)` (the argument of `pureAfter` may use `End α`), or build an `After` with `reclaim'`.

## `because type variable 'β' would escape its scope`

A scope continuation returns something typed with the scope's private lifetime, most often the reborrowed `Mut` itself:

```text
Couldn't match type 'r0' with 'Mut (β /\ α) (VL.Vector Int)'
  Expected: BO (β /\ α) r0
    Actual: BO (β /\ α) (Mut (β /\ α) (VL.Vector Int))
    because type variable 'β' would escape its scope
```

usually together with `Ambiguous type variable 'r0' … Consumable r0`.
Inside `reborrowing_`/`sharing_`, finish with `consume Control.<$> op mvec` (or `Control.pure (consume mvec)`); the outer `Mut` is handed back by the combinator itself.

## `Couldn't match type 'α' with 'β /\ α'` (or `'α1' with 'α'`) mentioning a borrow in the result

A borrow is escaping its lifetime, and the expected result type was fixed by a signature.
For example, returning the scope's `Mut (β /\ α) a` from a `reborrowing` continuation:

```text
Expected: BO (β /\ α) (Mut α (VL.Vector Int))
  Actual: BO (β /\ α) (Mut (β /\ α) (VL.Vector Int))
```

or returning a `Share` out of `runBO_`:

```text
Expected: BO α1 (Ur (Share α (VL.Vector Int)))
  Actual: BO α1 (Ur (Share α1 (VL.Vector Int)))
```

Return unrestricted data (`Ur n`), the restored outer borrow, or an `After` finaliser instead; copy what you need out of the borrow before the scope ends.

## `No instance for 'Movable (VL.Vector Int)' arising from a use of 'linearly'`

`linearly` only returns `Movable` values, so an owned mutable resource cannot leave it.
Convert the resource first (`VL.toVector`, `VL.toList`, `Ref.free`), or keep working inside the linear scope.
`runBO`, `runBOLend`, and the `modifyBO` family have no such constraint and may return owners, as long as the enclosing `linearly` gets something `Movable` in the end.

## `No instance for 'Movable (Ref Int)' arising from a use of 'VL.toList'`

Materialising an element-owning container moves every element into GC ownership, which requires `Movable` elements.
The boxed `VL.Vector` has no other way out (no `Consumable` instance), so a boxed vector of `Ref`s can never be disposed of.
Store elements that are not `Movable` in the growable vector (`Data.Vector.Mutable.Growable.Linear.Borrow`), which is `Consumable` whenever its elements are.

## Different results at `-O0` and `-O1`, or stale sizes

Pure code must not depend on the optimisation level, so this signals a borrow used where its lifetime no longer protects it, most often a `Share` captured by a `pureAfter` block and read after `reclaim` and a later mutation of the owner.
Some header reads (for example the size of a growable vector) are pure functions of the borrow, so the compiler may serve them from an earlier evaluation.
Copy what you need into `Ur` before `pureAfter`, and use only lenders and owned values inside it.

## `thread blocked indefinitely in an MVar operation` around `parBO`

`parBO` does not forward exceptions: when one branch throws (an out-of-bounds index, `error`, a failed pattern), the child thread dies, prints its exception to stderr, and the parent waits forever.
Look at the stderr output for the real error, and check indices and preconditions before forking.

## `<Type> cannot be copied!`

A custom `Unsatisfiable` error: the element type contains mutable state (`Vector`, `Ref`, `HashMap`, …) and must not be `Copyable`.
Work through a borrow of the element (`VL.get` returns one), or `clone` it inside `BO` if you really need an independent copy.

## `Not in scope: 'DataFlow.fail'`

GHC 9.10 rejects constructor patterns such as `(Ur n, v) <- …` in `DataFlow.do`.
Bind a variable and match it with `case`, or require GHC ≥ 9.12.
`Control.do` is not affected.

## Impredicativity errors around scope combinators

`reborrowing`, `sharing`, `runBO`, `srunBO` and friends take rank-2 continuations.
Enable `ImpredicativeTypes`, pass continuations with `BlockArguments` (`reborrowing_ mvec \mvec -> …`) rather than through `$`, and give local helpers explicit signatures.

## Parallel code runs sequentially

`parBO` forks Haskell threads; compile with `-threaded` and run with `+RTS -N` (or `-N<k>`) to use several cores.
`parBO` evaluates each side's result to WHNF on its own thread, so make sure the expensive work happens inside the `BO` actions rather than in a lazy result.
