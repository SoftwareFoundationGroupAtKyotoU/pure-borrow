# Pure Borrow Performance and Borrowed-Container Plan

## Status and scope

This is a planning document only. It responds to
`workspace/FEEDBACK-FOR-PURE-BORROW.md`, collected during the ongoing port of
the [Herbrand CDCL SAT solver](https://github.com/konn/herbrand) on branch
`konn/pure-borrow`. The feedback was produced against Pure Borrow revision
`79a0d1878ccbce8895039c253cc9e462a788d3f3`; the moving downstream branch is
context, not benchmark provenance.

Most later composition and measurement work described below is still planned.
The earlier Tamagoh-driven work recorded in the previous version of this file
is already present in the repository: direct inlinable `BO`/`After`/`Par`
methods, `subShare`, and hot-loop API guidance. R1 now also has a provisional
direct `copyAtMut` fast path.

The current worktree additionally contains the boxed fixed-storage
safe/`.Internal` split and a boxed growable MVP exposed from
`Data.Vector.Mutable.Growable.Linear.Borrow`. The growable owner uses a stable
`Ref` header over logical length plus replaceable backing storage, projects
only its initialized prefix through `getContents`, and deliberately cannot be
split. Focused semantics, ownership, role, and compile-failure coverage is in
place. A first optimized-Core smoke check shows one header read outside the
fixed-view worker and no header operation in that worker; full allocation,
paired runtime, R2/R3, unboxed-container, and supported-GHC inspection gates
remain pending. This structural result is not yet a performance claim.

The intended outcome is:

1. remove the confirmed per-cell lifetime-opening path from common fixed
   container reads;
2. provide general safe fixed and growable borrowed containers whose backing
   contents can be opened once through a short-lived fixed-size borrow;
3. establish a workload-independent multi-store transaction pattern that
   compiles to a compact primitive worker; and
4. cover both a tight heterogeneous scan and a resumable multi-store worklist
   with general in-repository regressions, with Herbrand retained only as
   downstream validation.

The work is additive unless an existing implementation can change without
altering its public type or semantics. `copyAtMut` remains a one-operation
convenience API, and the existing
`Data.Vector.Mutable.Linear.Borrow.Vector` continues to support linearly owned
elements.

## Evidence boundary

| Finding | Confidence | Planning consequence |
| --- | --- | --- |
| The former compositional `copyAtMut` expanded through `sharing`/`srunBO`/`askLinearly`, and optimized Core retained lifetime/dictionary machinery. | Confirmed by downstream Core inspection. The current provisional direct fast path avoids this expansion; R1 retains the former composition as a control. | Investigate making plain `sharing` erase to a representation-level scope; keep the direct copied-read fast path only while that composition remains materially slower. |
| Opening the backing content once around a loop can remove repeated outer-header opening. | Structurally demonstrated for the boxed growable `getContents` benchmark root: optimized Core performs one header read before the worker, whose hot loop contains backing-array reads/writes but no `Ref` operation. This is a smoke check, not the full cross-version or runtime gate. | Retain `getContents` plus the trusted rank-2 `withContent` convenience as the general safe formulation; automate the one-open/no-per-cell-delimiter inspection and complete allocation and paired runtime gates before claiming a performance benefit. |
| Herbrand's former `Data.Set` conflict-analysis cost was a downstream algorithm gap. | Confirmed and already removed downstream. | Do not design a Pure Borrow change around this obsolete gap. |
| Historical paired runs contain a 6–10× `3blocks` slowdown despite less total allocation and comparable search work. | A real signal, but the cited campaign lacked complete provenance and replacement exact-checkpoint results are pending. | Freeze a new exact downstream snapshot and do not use the historical ratio as an acceptance baseline. |
| The remaining slowdown is caused by `BO`, or entirely by the uninterrupted six-store watch scan. | Not established. The production path still crosses trail-literal and unit-enqueue/resume boundaries. | Keep separate generic linked-scan and resumable-worklist regressions; do not redesign `BO` from this evidence. |
| Safe fixed and boxed-growable low-level containers now exist in the worktree; fixed-unboxed, growable-unboxed, and a compact multi-store transaction pattern remain absent. | The boxed growable API and its fixed initialized-prefix projection are implemented and tested. Downstream R2/R3 composition and unboxed controls have not yet been supplied. | Complete the remaining unboxed and multi-store deliverables, then validate the boxed API in R2/R3 rather than treating API presence alone as acceptance. |

The plan targets a confirmed granularity problem and a missing safe
abstraction. It does not promise that the new API will remove the full Herbrand
outlier.

API completeness is not sufficient. Each landed primitive must also meet its
phase-specific Core, allocation, and runtime gate. The plan includes direct
optimization of current Pure Borrow operations and an evidence-driven
representation/compiler workstream, not just new container types.

## Upstream selection rule

Herbrand is an evidence source, not the design boundary. A change belongs in
Pure Borrow only when its public contract can be explained without SAT/CDCL
concepts and is useful to other mutable algorithms.

The candidate upstream abstractions are therefore limited to:

- fixed and growable borrowed containers;
- a same-lifetime linear `getContents` projection from a general mutable or
  shared growable `Borrow` to fixed content of the same borrow kind;
- `withContent` scopes over the initialized fixed-size prefix;
- copied reads, replacement, reserve, `push`, and `extend`; and
- general composition of several independently borrowed owners.

Herbrand-specific store layouts, watch states, clauses, trails, VSIDS, enqueue
rules, and conflict outcomes do not become public types, operations, or
mandatory upstream regressions. Pure Borrow does not take a dependency on
Herbrand.

Multi-store composition uses the existing `Muts`/`reborrowings` API. This plan
adds no bundle-specific content projection or convenience.

## Safety and representation constraints

All proposed APIs must preserve these invariants:

- `getContents` consumes one occurrence of
  `Borrow bk α (GrowableVector a)` and returns
  `Borrow bk α (Fixed.Vector a)`. It preserves borrow kind and lifetime and
  does not recover the growable borrow. For `Mut`, this irreversibly gives up
  growth access through that occurrence; `Share` remains read-only and follows
  its normal unrestricted rules.
- The projection is safe because a mutable growable capability is consumed,
  a shared capability cannot grow, and the returned fixed borrow has the same
  kind and lifetime. Its public surface cannot grow or expose spare capacity.
  Reading the stable header and constructing the bounded slice remain trusted
  internal operations. The
  growable header and fixed vector are representationally different. No
  satisfiable relation
  `GrowableVector a <: Fixed.Vector a` nor
  `Fixed.Vector a <: GrowableVector a` may exist, and there is no `Coercible`
  relationship between them. This is a rule about the element types; it does
  not change any existing lifetime or `Mut`/`Share` subtyping rule. Optionally
  add explicit instances guarded by
  `Unsatisfiable (Text "GrowableVector and Vector cannot be subtypes of each other")`
  in both directions solely to improve the type error; such instances provide
  no usable subtype evidence.
- `withContent` runs in the growable borrow's ambient lifetime `α`. Its
  continuation is polymorphic in a fresh `β` and runs in `BO (β /\ α)`, so no
  separate outer-lifetime subtyping parameter is needed.
- The continuation receives one linear occurrence of
  `Borrow bk (β /\ α) (Fixed.Vector a)` over the initialized logical prefix.
  A shared callback can apply `move` to regain ordinary unrestricted use. The
  result type is independent of the rigid, universally quantified `β`, so the
  shortened borrow cannot be returned at a caller-nameable lifetime. Unpacking
  an existentially hidden `β` supplies no outlives relation to an ambient `BO`;
  ordinary copied observations produced inside the scope may of course be
  returned.
- A mutable growable owner is unavailable during the continuation and is
  returned only after the short-lived content borrow has ended. Independently
  retained shared aliases remain read-only.
- A growable owner is never splittable: splitting either duplicates its stable
  allocation header or creates independently replaceable headers over storage
  that can overlap after growth. Both violate the allocation invariant, and
  concurrent mutation through the resulting capabilities would not be
  race-free. Parallel subdivision is permitted only after `getContents`
  produces the fixed initialized-prefix view; every fixed piece must end
  before growth access is recovered.
- The fixed view exposes neither spare capacity nor growth, reservation,
  replacement, freeze, or consumption of the growable owner. Growth remains
  an owner-level operation
  performed outside `withContent`.
- The view's bounds equal the logical length captured when `withContent`
  opens the outer header. Uninitialized capacity is not represented in the
  safe view.
- Checked operations preserve existing bounds-error behaviour. Every
  unchecked operation documents its exact index and initialization
  preconditions.
- Replacing a growable backing buffer preserves the values in the initialized
  prefix exactly once on normal return. No strong exception guarantee is
  claimed until the implementation defines and tests an exception policy.
- `0 <= logicalLength <= capacity` is maintained across every public
  operation. Length is published only after all newly initialized cells are
  valid.
- `Copyable` authorizes copied reads; it does not authorize retirement. Boxed
  growth that copies and retires old slots also requires `Consumable`.
- Existing `NOINLINE`/`GHC.noinline` barriers around linear lifetime witnesses
  are not weakened. A zero-cost `sharing` path must avoid needing a new
  `Linearly` witness by construction, not expose or inline the existing
  witness constructor.
- Trusted backing-array operations are confined to container `.Internal`
  modules. Under this repository's convention those modules are exposed for
  package composition but Haddock-hidden; safe algorithm modules import only
  no-suffix or explicitly experimental APIs.

Linear record splitting proves that field capabilities are each consumed once.
Non-aliasing also relies on each owner having been built by safe constructors;
the plan therefore says that splitting *linearly separates independently
constructed roots*, not that generic splitting alone proves their backing
stores are distinct.

## Regression topology and provenance

### Downstream evidence snapshot

Before making new Herbrand claims during optional downstream validation,
freeze an evidence snapshot with:

- exact Herbrand commit and tree, cleanliness, and the source branch recorded
  separately;
- exact polished-primary baseline commit/tree where it is used;
- exact Pure Borrow commit/tree;
- GHC, Cabal, package database, Cabal plan/freeze, build flags, RTS settings,
  GC, CPU, and capability count;
- fixture/reducer/trace, executable, runner, analyzer, and raw-result hashes;
  and
- analyzer configuration, seed, and statistical-script version.

The branch `konn/pure-borrow` remains useful for collaboration but is never
used as a reproducible identity.

### Three progressively available regressions

The regression suite is staged because Pure Borrow does not yet expose the
unboxed and growable owners required by the final workloads.

**R1 — Existing boxed-vector copied-read loop.** This is immediately buildable.
It compares the current public `copyAtMut` loop with an equivalent direct boxed
mutable-vector control. It freezes semantics, bounds behaviour, optimized Core,
and allocation before changing `copyAtMut`.

**R2 — Six-root heterogeneous linked scan.** Enable the Pure Borrow candidate
only after the fixed and growable MVPs exist. It contains three fixed unboxed
roots, one growable boxed root, and two growable unboxed roots; linearly
separates the six independently constructed owners; opens each growable
content once; performs a deterministic 4,096-node linked scan; reads all
roots; conditionally writes two roots; closes the content scopes; and reclaims
all owners.

R2 is a general container/code-generation stress test. Its shape is motivated
by the feedback, but its specification is independent of SAT semantics.

**R3 — Resumable heterogeneous worklist traversal.** Specify this independently
as a deterministic graph/worklist workload. Fixed roots hold offsets, marks,
and scalar state; growable roots hold adjacency/payload data, a work queue, and
an output log. Keep two modes with the same graph and transition semantics:

- **R3a, open once:** pre-reserve and initialize fixed-capacity queue/log
  storage, keep their logical frontiers in separately borrowed scalar state,
  and hold one content borrow per growable root through the traversal. This
  isolates the no-growth composition and recursive worker.
- **R3b, push/extend and reopen:** use the public owner-level `push` or `extend`
  operation. Close the current common fixed-content scope, grow, reopen the
  required group, and resume. Run no-growth, sparse-growth, and dense-growth cases with
  visits held comparable. This measures the unavoidable owner/header and
  delimiter cost as a function of resumptions rather than pretending it is
  constant.

Both modes consume several work items, scan adjacent entries, conditionally
update marks/scalars, support normal drain and a deterministic early-stop
outcome, and resume the current traversal after a logical enqueue transition.

Freeze expected dynamic operation counts, transition counts, content-scope
open/reopen counts, termination outcomes, and a final-state digest. Maintain
direct and public Pure Borrow implementations with identical backends,
checkedness, logical-length rules, strictness, initial state, logical hot-path
data-access/transition counts, and timed setup/reset/freeze boundaries. Report
Pure Borrow's header/borrow operations separately. R3a must open each root
`O(1)` times outside the hot recursive SCC; R3b may perform `O(growths)`
open/close operations, but their count must match the frozen transition trace
and their cost must be reported against both visits and reopenings.

An exact reduced Herbrand trace is optional downstream evidence, separate from
R3 and from API promotion. If a benchmark-only snapshot of Herbrand's adapters
is needed for that comparison, keep it outside the public library and label
its measurements non-product evidence. Do not upstream those unsafe adapters
merely to satisfy “regression first.”

## Proposed work

### Phase 0 — Freeze specifications and the existing-API baseline

Implement R1 and freeze deterministic, application-independent inputs,
expected results, benchmark protocol, provenance manifest, and direct controls
for R2 and R3. R2/R3 need not yet have public Pure Borrow candidates.

Treat the committed `Aliases`/`Muts`/`Shares`/`Lends`, `reborrows`/
`reborrowings`, and associated-lifetime `Reborrowable` API as the new
baseline. Before optimizing it, add focused typing/semantic coverage and bring
the umbrella tutorial plus experimental Haddocks into agreement with the new
`AliasKind` and associated-type signatures; do not preserve examples of the
superseded higher-kinded `Borrows` formulation.

Correct the current `assocBorrowEq` signature before using this baseline as
proof evidence. It must quantify a `BorrowKind`, lifetimes, and a resource and
prove associativity for `Borrow bk lifetime resource`, matching
`assocBorrowR`/`assocBorrowL`; the current kind-correct but accidentally
partially applied formulation does not state that law. Add a focused typing
case so a future `AliasKind` refactor cannot silently change the theorem again.

Place semantic tests in the existing tasty tree. Plan a dedicated
optimization-enabled `pure-borrow-inspection` test component for named
benchmark roots, using `inspection-testing`/`tasty-inspection-testing` or a
small version-aware Core extraction script after an implementation spike.
Put runtime workloads in a new internal benchmark module/component rather than
the qsort or FFT suites. Use cabal-gild discovery for new modules.

Wire structural inspection into each GHC matrix job. Keep paired wall-clock
campaigns in a controlled/manual performance job; current CI runs only the
qsort benchmark in one matrix entry and is not a stable timing environment.
Run explicitly with `+RTS -N1` because existing benchmark components may
default to all capabilities.

### Phase 1 — Runtime-erased sublifetime combinators and fixed-vector fast paths

Treat the current sublifetime implementations as constructive/theoretical
references, not necessarily the final execution paths. For example, the safe
type of `sharing` already delimits the temporary shared borrow:

```haskell
sharing ::
  Mut α a %1 ->
  (forall β. Share (β /\ α) a -> BO (β /\ α') r) %1 ->
  BO α' (r, Mut α a)
```

Investigate a small trusted `.Internal` core in which sublifetime delimiters
are erased at runtime:

- retain the outer `Mut` representation internally while the continuation
  receives only a shortened `Share`;
- instantiate and invoke the universally quantified continuation normally,
  linearly, and exactly once; coerce only its
  `State# (ForBO (β /\ α'))` threading to the ambient `BO α'`
  state token through a primitive linear in both the action and token;
- return the outer `Mut` only after the continuation completes; and
- introduce no `Linearly`, `Now`, `End`, `After`, `withDict`, or fresh runtime
  token on this non-finalizing path.

This is still a statically delimited lifetime; “no-op” means that the
delimiter erases from optimized execution. The safe public type remains
unchanged. The implementation must not inline or fabricate the existing
`Linearly` witness.

Audit and optimize the family in two proof layers:

1. **Non-finalizing scopes:** `srunBO_`, `sharing`, `sharing_`,
   `reborrowing`, `reborrowing_`, the new plural
   `reborrowings`/`reborrowings_` over `Muts`, and the corresponding
   experimental `locally`/`locally_` operations. These should be candidates
   for a direct `runST`-style state-token coercion plus
   representation-level lifetime narrowing/restoration. The plural operation
   may change only the lifetime in `AliasKind`; its element list, spine order,
   and payload types remain identical.
2. **Finalizing scopes:** `srunBO`, `sharing'`, `reborrowing'`, and
   `reborrowings'`/`locally'`, whose continuations return `After β`. Audit
   whether the non-finalizing primitive can simplify them, but defer a new
   trusted finalizing boundary until whole-root Core shows residual
   `After`/`End` machinery in a relevant workload or an independent
   microbenchmark. Any later boundary must discharge `End β` only after
   the continuation has completed and must demonstrate dictionary-thunk
   elimination, not merely disappearance of source-level names.

Build the public family from the smallest possible internal primitives.
Do not duplicate bespoke unsafe code in every combinator. Prove the whole
non-finalizing family, but initially land only the narrow scalar
result-discarding paths needed by R1. Keep plural and result-producing paths
constructive until R2/R3 provide evidence for their optimized forms.
Finalizing-layer optimization is a separate conditional follow-up, not a
container prerequisite.

Do not use one unconstrained alias coercion for the open `Reborrowable` class.
Third-party instances need not use the `Alias` representation and may attach
meaningful work to `locally'`. Optimize the built-in `Mut` and `Share`
instances and the explicit audited `Reborrowable (Muts α)` instance, or
add an instance-supplied non-finalizing method with a constructive default.
Preserve the new `LifetimeOf`/`WithLifetime` associated-type contract and
source/semantic compatibility for downstream instances.

The class equality
`bor ~ WithLifetime bor (LifetimeOf bor)` does not by itself state replacement,
composition, or idempotence laws for `WithLifetime`, nor a
`Reborrowable (WithLifetime bor β)` instance. Runtime-erasure and nested
reasoning therefore use only the separately audited equations for `Mut`,
`Share`, and `Muts`; a third-party `locally'` method remains authoritative.

The soundness note must cover:

- the standard Rank-N/skolem argument: the delimiter chooses rigid fresh
  `β`, the result type does not mention it, and existentially hiding it
  cannot manufacture the outlives premise needed by lifetime-indexed access in
  an ambient `BO`;
- the outer `Mut` is inaccessible during the continuation;
- nested scopes and affine early discard;
- exactly one execution of the action and allocation effects under nested
  scopes;
- completion of every safe structured-concurrency child before the outer
  `Mut` is restored;
- why the state-token coercion cannot duplicate allocation or linear
  resources;
- mutable reborrows being restored only after exclusive inner access ends;
- plural `reborrows` evaluating a delayed input bundle exactly once before
  producing one shortened `Muts` bundle plus one lender, preserving the
  element list and spine order exactly, and recovering the exact original
  bundle rather than reconstructing it from a shortened or projected spine;
  and
- for finalizing scopes, `End β` evidence becoming usable only at the
  boundary where `β` has actually ended.

Add focused semantic, negative-typing, and optimized-Core fixtures for each
combinator family, including nested `sharing`/`reborrowing`, generic
`Reborrowable`, affine discard, result-producing continuations, and parallel
continuations. Add a delayed-`Aliases`-spine fixture that detects repeated
evaluation or construction and inspect the plural worker for the same
exactly-once strictness guarantee as scalar `reborrow`. Retain representative
negative typing cases for direct lifetime escape and an existential lacking an
ambient outlives witness; do not treat an opaque existential package as a
usable escape. Audit all safe concurrency entry points, not only `parBO`, for
structured completion.
Document normal-return behaviour under child/parent exceptions; detached
unsafe forks remain an explicit caller proof obligation, and no exceptional
restoration guarantee is claimed before an exception protocol exists. Require
the optimized non-finalizing workers to contain no `askLinearly`,
`newLifetime`, `withDict`, or delimiter allocation.

Stage the trusted change from the narrowest surface: first prove the internal
single-state-thread primitive and use it only where the result is consumed
inside scalar result-discarding `sharing_`/`reborrowing_` paths, plus a direct
`copyAtMut` fast path if R1 still needs it. Keep `reborrowings_`, all
result-producing variants, and generic `locally`/`locally_` constructive until
the R2/R3 baseline and exact-once plural-spine tests exist. Optimize those
public paths only after the skolem, linear-restoration, and
representation-specific arguments are accepted. For every erased `_` variant,
strictly sequence `consume r` (as with the current `lseq` pattern) before
returning the retained outer alias or bundle; lazy consumption must not overlap
restored access. If these arguments remain unresolved, retain the constructive
implementations.

For a later finalizing layer, it is explicitly invalid to coerce
`After β r` directly to `r`. The boundary must construct or supply valid
`End β` evidence after completion. Its performance target is to keep
evidence discharge outside the hot SCC and have it erase under optimization,
not to assume that the current dictionary representation has no semantic
operation.

No standalone `End`/`After` eliminator is exposed, including from an exposed
`.Internal` module. The trusted operation must combine rank-2 introduction,
single execution of the state action, and discharge for the same private fresh
atomic lifetime `Al i`; it may not instantiate the binder with `Static`, the
ambient lifetime, or any caller-nameable lifetime. For finalizing
`sharing'`/`reborrowing'`/`reborrowings'`, combine the user's
`After β r` with the corresponding hidden scalar or `Muts` lender before
one strict discharge, as the constructive implementations do. Phantom erasure
must not weaken that sequencing. Add a negative case using `neverEnds` or a
caller-nameable lender to ensure an unrelated lifetime cannot be ended.

Keep the internal proof/signature for lexical restoration in non-finalizing
`sharing`/`reborrowing` separate from the lender-under-`After` restoration in
their finalizing variants, even if optimized Core eventually looks similar.

R1 should first measure whether this removes the complete
`copyAtMut -> sharing -> srunBO -> askLinearly` path. If it does, retain
`copyAtMut` as the ordinary composition. If residual overhead remains,
investigate a direct implementation whose raw boxed read is immediately
wrapped as a temporary borrow, eliminated through `Copyable.copy`, and never
returned as the aliased `a`.

The direct `copyAtMut` fast path is provisional rather than a permanently
preferred implementation. Once result-producing `sharing`/`reborrowing` and
their audited cousins are essentially no-ops in the relevant whole-root
optimized Core—and any finalizing `srunBO` machinery used by their constructive
forms also erases—restore the older compositional `copyAtMut` implementation
and rerun R1. This gate is separate from making non-finalizing `srunBO_` erase.
Keep the restored composition whenever its Core, allocation, and runtime match
the direct fast path within the frozen R1 tolerance; retain the trusted direct
read only while benchmark evidence shows a material advantage.

In either case preserve `copyAtMut`'s checked bounds behaviour. Accept the
change only when typing/soundness review, R1 semantics, optimized Core,
allocation, and runtime gates all pass.

### Phase 2 — Fixed-storage prerequisites and unboxed borrowed-array MVP

Prototype a fixed-capacity unboxed owner behind a safe experimental module and
a Haddock-hidden `.Internal` implementation. A stable module after promotion
could be `Data.Array.Mutable.Linear.Unboxed.Borrow`.

Before either growable MVP, move the existing boxed
`Data.Vector.Mutable.Linear.Borrow.Vector` representation into a
Haddock-hidden exposed
`Data.Vector.Mutable.Linear.Borrow.Internal` module while preserving the safe
module's exact API. The internal module may expose a trusted O(1) raw-slice
constructor to sibling container implementations; it must not become a safe
public constructor. Treat this safe/`.Internal` split and its regression gate
as the first Phase 2 delivery, not as incidental work deferred until the boxed
growable implementation. Give the new fixed-unboxed owner the same split from
the outset.

The MVP distinguishes these operations:

- safe construction from an immutable unboxed vector copies it;
- an unsafe ownership-taking thaw, if useful, is named separately;
- owner size;
- checked and unchecked copied reads, requiring `Unbox a` and `Copyable a`;
- checked and unchecked writes returning the displaced value;
- consuming zero-copy freeze, requiring `Copyable a` when it returns an
  unrestricted immutable vector;
- explicitly copying materialization from a live shared borrow, as a distinct
  operation;
- a conventional `Consumable` instance requiring `Consumable a`; and
- direct mutation through `Mut`, without an additional scoped-view type for an
  already fixed-capacity owner.

Do not assume that `Unbox a` proves Pure Borrow's `Copyable` or `Consumable`
invariants. Add negative role tests for element-to-element and
backend-to-backend coercions.

### Phase 3 — Growable unboxed and Copyable/Consumable boxed MVPs

Run a design spike for the stable owner identity before fixing public types.
Compare a `Data.Ref.Linear.Ref` header with a new nominal mutable header. A
replaceable owned value is a separate owned-only API alternative, not a
candidate for `reserve`/`push`/`extend` through `Mut`: an existing `Lend` would
otherwise reclaim the old representation after growth. Record how each viable
stable-header choice:

- opens `(logicalLength, backingBuffer)` once per no-growth content scope;
- prevents access to the growable owner while its fixed content borrow is
  live;
- replaces the buffer on growth; and
- optimizes in R2/R3.

The benchmark-prioritized implementation has produced the boxed MVP first for
payloads satisfying the required ownership constraints. Keep the unboxed
growable MVP as the next backend-specific counterpart. The common owner-level
surface is:

- empty construction and construction with initial capacity;
- logical length and capacity;
- checked/unchecked copied reads and updates;
- `reserve`/`reserveAdditional`;
- `push` of one linearly supplied element;
- `extend` from an immutable input, which copies;
- consuming freeze/materialization and a conventional `Consumable` instance;
  and
- `getContents` for a same-lifetime initialized fixed-size projection, with
  `withContent` using the trusted runtime-erased rank-2 shortening described
  below and remaining observationally equivalent to `reborrowing` for `Mut`.

Safe construction or `extend` that copies from immutable unboxed input
requires `Unbox a` and `Copyable a`. Unboxed growth that copies into a new
buffer and retires the old initialized cells requires
`Unbox a`, `Copyable a`, and `Consumable a`, just as boxed reallocation needs
the corresponding ownership proof. A weaker constraint is acceptable only
after a separate destructive-move implementation and proof.

Normal-return ownership is the guarantee: the returned owner contains the
original initialized prefix exactly once. Preflight size arithmetic and
capacity before mutation, do not run user callbacks during partial growth, and
document how synchronous/asynchronous exceptions are handled. Do not claim
that the old owner is recoverable after an exception until that is actually
implemented.

For boxed v1:

- copied read requires `Copyable a`;
- growth that copies to a new backing store and retires old slots requires
  both `Copyable a` and `Consumable a`;
- consuming the owner is provided only through its `Consumable` instance;
- non-growth replacement returns the displaced value; and
- batch move from a linearly owned source is deferred with arbitrary linear
  element support.

#### Fixed-content scope

Make the primitive operation a same-lifetime linear projection:

```haskell
import Data.Vector.Mutable.Linear.Borrow qualified as Fixed

getContents ::
  Borrow bk α (GrowableVector a) %1 ->
  Borrow bk α (Fixed.Vector a)

withContent ::
  Borrow bk α (GrowableVector a) %1 ->
  ( forall β.
    Borrow bk (β /\ α) (Fixed.Vector a) %1 ->
    BO (β /\ α) r
  ) %1 ->
  BO α (r, Borrow bk α (GrowableVector a))

withContent_ ::
  Consumable r =>
  Borrow bk α (GrowableVector a) %1 ->
  ( forall β.
    Borrow bk (β /\ α) (Fixed.Vector a) %1 ->
    BO (β /\ α) r
  ) %1 ->
  BO α (Borrow bk α (GrowableVector a))
```

The boxed signatures above need no backend representation constraint. The
unboxed backend's corresponding `getContents`, `withContent`, and
`withContent_` require `Unbox a` unless the selected header representation
stores that dictionary existentially. Prefer the explicit constraint; if the
dictionary-carrying alternative is prototyped, count the retained dictionary
in whole-root Core before accepting it.

`getContents` is a conversion between representationally different types: the
growable owner contains a stable mutable header while `Fixed.Vector` denotes
the currently initialized slice. Its trusted internal implementation consumes
one growable-borrow occurrence, uses one container-internal header-projection
primitive, constructs a constant-time slice over `[0, logicalLength)`, and
returns that slice with exactly the input borrow kind and lifetime. A mutable
projection prevents growth because it does not return the growable borrow; a
shared projection remains read-only.

Specify that internal primitive precisely. It may use
`Data.Ref.Linear.unsafeReadRef`, whose raw read duplicates ownership while
leaving the `MutVar` cell unchanged, alias the returned backing owner only at
the input borrow kind, and consume `pop (aff duplicateRef)` (or the equivalent
linear elimination) to forget the duplicate returned `Ref` handle. It may
pattern-match the duplicated header value to construct the fixed slice, but it
must not free or mutate the authoritative cell or backing store: the dormant
scalar or plural lender retains the original header while the fixed slice
physically aliases its backing buffer. It performs no header write and is not
expressible through `Data.Ref.Linear.Borrow.update`. This ownership
duplication/forgetting step is a trusted proof obligation confined to the
growable container's `.Internal` module. If a later header design uses a
destructive read instead, it must restore that header before lender recovery;
the one-read/no-write proof applies only to the non-destructive `Ref` peek.

Calling `getContents` directly on `Mut` permanently gives up growth access
through that occurrence; it does not reconstruct or return the growable owner.
Recovery is instead supplied by an enclosing existing reborrow/lender.

`withContent` uses the same trusted rank-2, runtime-erased shortening pattern
as the scalar borrow scopes: it retains the outer borrow, projects a
representation-identical shorter occurrence through `getContents`, executes
the callback once, and restores the original occurrence only on normal return.
For `Mut` this is observationally the existing
`reborrowing`/`getContents` composition. For `Share`, the same signature
preserves the borrow kind while the callback can `move` its linear occurrence
for unrestricted reads. The rigid fresh lifetime prevents either fixed view
from escaping. Aligning the ambient effect lifetime with `α` keeps the
convenience signature simple; multi-owner composition uses one `reborrowings`
call over `Muts` in Phase 4 rather than naively nesting this wrapper.

`Data.Vector.Mutable.Linear.Vector` from the selected `linear-base` 0.7
dependency is itself growable and is therefore not the fixed content type
intended here. Reuse Pure Borrow's fixed
`Data.Vector.Mutable.Linear.Borrow.Vector` or the corresponding new fixed
unboxed owner.

Creating the logical-prefix view must be constant-time, using the internal
mutable-vector slice constructor introduced in Phase 2. The stable header
remains the authoritative owner; the projection reads its current backing
store but cannot resize or replace it. Prove that consuming the linear header
borrow after the raw read does not free or move the underlying owner retained
by its lender. Preserve any backing offset and construct exactly
`[0, logicalLength)` relative to that backing slice. Treat slice-metadata
allocation as an inspection question, not as a guaranteed zero-allocation
property.

The proof must explicitly cover affine discard: `getContents` manufactures
exactly one bounded slice and consumes the growable `Mut`; `reborrowing` or
`reborrowings` owns recovery independently of whether the callback returns any
view value. Audit the complete safe `Fixed.Vector` surface to confirm it cannot
resize, replace, freeze, consume the growable owner, or reveal backing
capacity.
Unsafe-named raw operations retain their stated caller proof obligations.
An existentially packaged slice may retain its backing allocation, but
unpacking it yields no ambient outlives witness. Include that case in residency
tests without treating the opaque package itself as a soundness failure.

The boxed growable owner returns the existing boxed
`Data.Vector.Mutable.Linear.Borrow.Vector` as its fixed content. The unboxed
growable owner returns the fixed unboxed owner introduced in Phase 2. Both
representations require real safe/`.Internal` module splits and nominal roles
where backend selection could otherwise be coerced.

The Haddock for both operations must state their performance purpose
explicitly: a `GrowableVector` keeps replaceable content behind an outer
mutable header so growth preserves the outer owner's identity;
`getContents` opens that indirection once for repeated no-growth access, while
`withContent` combines the projection with a short reborrow so the growable
borrow is recovered afterward. It must also state that only the initialized
fixed-size prefix is exposed and cannot reserve or grow.

Opening the header once is not sufficient if every fixed-vector access still
constructs a lifetime delimiter. Promotion therefore requires R2 Core to show
both one real header opening for the scope and zero per-cell delimiter
machinery, obtained through the Phase 1 erased path or a separately justified
direct fixed-access implementation.

Do not expose safe spare-capacity access in the MVP. If a general client later
needs it, design a separate initialization-aware API and justify it
independently.

For an invariant spanning several growable vectors, document the safe pattern:
pre-reserve every owner, collect their mutable borrows into `Muts`, call
`reborrowings`, pattern-match the statically known shortened bundle, and call
the scalar `getContents` on each concrete growable member before entering the
worker. Perform
`push`/`extend` through owner operations between common content scopes. Do
not claim that independent calls make a cross-container size invariant atomic
unless a later typed transaction enforces it.

### Phase 4 — Compact multi-store composition

Use the revised heterogeneous formulation in
`Control.Monad.Borrow.Pure.Experimental.Borrows`. `Aliases k xs` is the
representation, while `Muts α xs`, `Shares α xs`, and
`Lends α xs` are the relevant aliases.

Keep the low-level alias-bundle module independent of container backends.
Add no mapped-list family, bundle projection, projection class, or general
“project the contents of any owner” abstraction. `getContents` is the only
content projection. Multi-owner algorithms already know the `Muts` shape they
constructed, so they pattern-match that shape once at the scope boundary and
apply `getContents` directly to each growable member.

`reborrowings` supplies one common fresh lifetime and retains the original
`Muts` bundle through its plural lender. Equivalently, generic code may use
`locally` through the explicit `Reborrowable (Muts α)` instance and its
`LifetimeOf`/`WithLifetime` equations. Scalar `getContents` then consumes each
selected shortened growable capability into a fixed view of that same
lifetime.
Constructive `reborrowings'` must combine the user's `After β r` with the
lender for the unchanged original `Muts α xs`. An optimized
non-finalizing form must lexically retain that exact original spine and
payloads. Neither form may reconstruct a growable owner from a projected fixed
view.

The one-vector `withContent`/`withContent_` operations from Phase 3 remain as
the scalar conveniences. Add no plural equivalent or fixed-arity bundle
functions.

For a statically known mixed bundle, pattern-match once at the boundary and
apply the scalar projection to each growable member:

```haskell
reborrowings owners \case
  fixed :- growableBoxed :- growableUnboxed :- BNil ->
    worker
      fixed
      (Boxed.getContents growableBoxed)
      (Unboxed.getContents growableUnboxed)
```

Composition across independently reopenable groups uses separate bundles and
nested delimiters. For example, R2 may either use one statically known mixed
bundle and the direct pattern above, or reborrow fixed and growable groups at
different nested boundaries when that produces better Core. At the deepest
scope it may use only the existing lifetime subtyping operations to shorten
unchanged fixed-borrow element lists to the common meet before calling the
worker. The rank-N binders prevent any shortened borrow from escaping.

Likewise, R3b keeps long-lived roots in one outer bundle and independently
captures a queue/log growable bundle for an inner `reborrowings` scope. The
inner callback pattern-matches the queue/log bundle and calls `getContents` on
those members. The inner originals are recovered before growth and reborrowed
afterward. This
means separate `Muts` values: `WithLifetime (Muts α) β` changes the
outer alias lifetime only and does not recursively rewrite a `Muts` embedded
as an element. Treat literal `Muts`-inside-`Muts` elements as opaque and do not
use that shape for this design.

Call the outer members *long-lived roots*, not necessarily read-only roots:
they remain `Muts` unless deliberately converted to `Shares`. Each R2/R3b
typechecking fixture must pattern-match its exact `Muts` list, call the
backend-qualified scalar projection for every growable member, linearly thread
each separate bundle, recover the original inner growable `Muts`, perform
owner-level `push` or `extend` in the ambient `BO`, and then reborrow/project that inner
group again. Include required meet reassociation and lifetime-only `upcast`
witnesses explicitly rather than assuming inference.

Prototype singleton, statically known mixed, nested subgroup, and R2 shapes
before documenting the preferred pattern. Keep `Aliases` construction and
projection out of the hot-worker Core: linearly separate independently
constructed roots outside the measured worker, then pass their fixed mutable
borrows as distinct linear worker arguments. Do not infer representation
safety from an arbitrary `Reborrowable` instance.

Pattern-matching a `Muts` spine does not itself allocate, and `reborrows` must
retain the exact original spine rather than reconstruct it. Account separately
for constant-size slice metadata from each scalar `getContents` and for
`(:-)` allocation only at explicit bundle construction/reconstruction sites.
R2/R3a can construct their initial bundles outside the measured kernel. R3b
may rebuild an inner spine after owner-level growth, giving an
`O(innerRootCount * reopenCount)` boundary term, but that is not an intrinsic
cost of every reborrow or projection. Require no `Aliases` allocation in the
per-entry hot SCC.

The supported composition must:

- open each owner/header once in R2 and R3a, and once per declared fixed
  no-growth segment in R3b;
- keep queue-update/resume control inside R3b's logical owner-passing
  operation, ending the current common content scope before growth and
  reopening the required group for the resumed segment;
- avoid rebuilding a boxed aggregate at each node;
- end every short content borrow and return every growable owner exactly once;
  and
- avoid excessive worker arity or recursive-body duplication.

Every nested operation must retain each fresh sublifetime and the linear
return discipline of every owner. Reject the design if it requires unsafe
client code, leaves `Aliases` constructors/dictionaries in the recursive SCC,
duplicates projection/restoration, or creates a large specialized worker.

For R3b, compare one flat bundle, which reopens every member, with nested outer
and queue/log bundles, which should reopen only the inner members. Freeze and
inspect the actual per-root reopen formula; retain hierarchical grouping only
when its delimiter and dictionary machinery erases outside the growth
boundary.

Success means ordinary safe code expresses R2 and R3. Replacing the general
transactions with a larger workload-specific `unsafeSystemIOToBO` kernel is
not a substitute because it merely moves a client's trusted boundary upstream.

### Phase 5 — General Pure Borrow optimization loop

Use R1, R2, and R3 to optimize Pure Borrow itself in increasing scope:

1. **Convenience-operation fast paths.** Fuse copied reads and similar
   one-operation container methods so they do not open an avoidable ephemeral
   lifetime, while preserving their public types, bounds behaviour, and
   ownership proofs.
2. **Scope opening.** Inline content-borrow construction/elimination and
   growable-header opening so each occurs once per no-growth segment and no
   wrapper, tuple, dictionary, or header access remains in the recursive hot
   SCC.
3. **Backend operations.** Specialize checked/unchecked content reads, writes,
   and owner-level `push`/`extend` calls to their boxed/unboxed backends. Keep checks outside
   inner unchecked workers after a checked scope entry when the loop invariant
   proves bounds.
4. **Worker shape.** Tune strictness, argument grouping, worker/wrapper
   boundaries, and specialization only from measured Core. Prefer separate
   scalar content-borrow arguments or an unboxed representation when a boxed
   aggregate survives per iteration; prevent duplication of a large recursive
   body.
5. **Allocation and retention.** Remove allocation proportional to visits,
   then compare residency, bytes copied, and mutator time so a lower allocation
   count is not mistaken for complete performance parity.

Set `INLINE`/`INLINABLE`/`SPECIALIZE` pragmas only where the named inspection
roots demonstrate a stable benefit on supported GHCs. Do not remove or weaken
the soundness-motivated lifetime-witness barriers.

Escalate to a general `BO` representation or combinator change only if the same
residual overhead survives across at least R2 and R3 after container and worker
optimizations. Such a change needs an isolated before/after Core proof, all
existing soundness tests, and evidence that it improves more than one
container/workload shape. A workload-specific raw kernel is not a `BO`
optimization.

### Phase 6 — Batch operations, documentation, and promotion

After R2/R3 validate the MVP representations, add only bulk operations justified
by measured header-opening costs. `extend` should preflight capacity, open
the header once, initialize the whole input, and publish length last.

Document four access levels:

- one-operation access (`copyAtMut`, ordinary update);
- share-once loops (`sharing` plus `subShare`);
- one-container rank-2 no-growth transactions; and
- multi-container no-growth transactions.

Explain that `withContent` is a scoped performance operation, not a persistent
owner: it opens the growable container's outer indirection once for a
no-growth region. Include complete open/use/close/reclaim examples and the
initialized-prefix invariant.

Promote experimental modules to stable safe names only after:

- ownership and initialization notes are reviewed;
- semantic, negative typing, Core, allocation, and runtime gates pass on the
  MVP;
- API names and constraints solve a general container/transaction problem;
- an independent non-SAT example uses the safe API without importing
  `BO.Unsafe`.

Removal of Herbrand's unsafe adapters is desirable external validation, not a
condition for landing or promoting the general API.

Stable candidates, subject to that gate, are:

- `Data.Vector.Mutable.Linear.Borrow[.Internal]`, with the current safe
  surface preserved and only the representation moved;
- `Data.Array.Mutable.Linear.Unboxed.Borrow[.Internal]`;
- `Data.Vector.Mutable.Growable.Linear.Borrow[.Internal]`; and
- `Data.Vector.Mutable.Linear.Unboxed.Borrow[.Internal]`.

Mirror them under `test/`, let cabal-gild discover the modules, and add only
the inspection dependencies needed by the dedicated test component. Existing
`vector` and `primitive` dependencies should be evaluated before adding a new
container package.

## Verification and acceptance gates

### Semantics and typing

- Unit/property tests for sizes 0, 1, logical length, capacity, and growth
  boundaries.
- Model-based sequences for reserve, `push`, `extend`, update, freeze, and
  owner consumption, introduced with their corresponding phases.
- Content-borrow/owner round trips preserve contents and the initialized
  prefix.
- Direct `getContents` preserves borrow kind, captured logical length, and
  contents. A mutable occurrence cannot then perform safe grow/reserve; a
  shared occurrence remains read-only. For `Mut`, the `withContent`
  convenience must be observationally equivalent to explicit
  `reborrowing`/`getContents` composition, while shared coverage must show that
  `move` permits repeated reads without allowing the shortened view to escape.
- Typing tests freeze the revised `Aliases` roles and the exact committed
  subtype premises: scalar `Mut` and plural `Muts` require both element/list
  directions, scalar `Share` and plural `Shares` require the forward
  direction, and `Lends` retains its current lifetime/element direction.
  These are ground truth, not optimization candidates. Also freeze the
  `Reborrowable (Muts α)` `LifetimeOf`/`WithLifetime` equations.
- Compile-time negative tests cover content-borrow escape,
  direct growable/fixed element subtyping (including the intended
  `Unsatisfiable` diagnostic if adopted), owner-to-content-borrow coercion,
  lifetime-index swapping,
  element/backend coercion, duplication/consumption, generic splitting, and
  growth while a content
  borrow is live. Keep direct linear-owner reuse in a separate compile-failure
  fixture because GHC does not defer multiplicity errors; validate that fixture
  through the Cabal-selected compiler rather than adding it to the ordinary
  test component.
- Role/constructor tests cover fixed and growable boxed/unboxed owners as well
  as the fixed-content view. Element/backend roles must be nominal wherever a
  coercion could select different backing operations, and safe code must not
  construct a view over spare capacity.
- Alias-provenance audit/tests construct every R2/R3 root independently before
  linear separation and reclaim every owner.
- R2/R3 direct and Pure Borrow variants must match expected logical hot-path
  data-access/transition counts, outcomes, and final digest before timing is
  accepted. Fixed content-scope open/close operations are accounted for
  separately.
- Existing public API tests and doctests remain green.

### Optimized Core

Give R1, R2, and R3 named `NOINLINE` benchmark roots solely to anchor
inspection. Inspect two regions:

1. the whole named root, counting calls to content-scope delimiters,
   growable-header `Ref` reads/writes, continuation/result allocations, and
   entries into the recursive worker, plus slice-metadata allocation and
   `Aliases` constructor allocation only at declared construction or
   reconstruction sites; and
2. the hot recursive strongly connected component, checking its primitive
   access and control-flow shape.

Require the whole-root counts to match the workload formula: one opening per
growable root for R2/R3a, and the frozen initial-plus-reopen count for R3b. A
runtime-erased lifetime delimiter does not imply that a real mutable-header
read/write can disappear.

Give `getContents`, `withContent`, and a statically pattern-matched
mixed-`Muts` boundary small named roots as well. Each growable projection
should contain exactly one header read plus constant-time slice construction,
no header write, and no lifetime delimiter of its own. After inlining, the
convenience boundary must add no runtime lifetime delimiter, and multi-root
boundaries must add only the already-budgeted `reborrowings` boundary. No
projection or `Aliases` bundle machinery may remain in the per-entry worker.

For every declared mixed-bundle and nested-group shape, record the number and
Core size of specialized opener bindings, the size of the single recursive
worker, and object text size or a stable Core-size proxy. Also retain
simplifier-tick/compile-time diagnostics after the baseline exists. Small
unrolled boundary wrappers are acceptable; duplicated large workers or
specialization growth disproportionate to root count is not.

For the hot recursive strongly connected component:

- require the expected primitive backing-array access and control-flow shape
  relative to the direct control;
- reject calls to explicit lifetime-opening wrappers/markers;
- reject growable-header `Ref` operations inside the hot SCC for R2/R3a; in
  R3b place them only in the declared push-or-extend/reopen boundary, never in the
  per-entry scan worker;
- count lifted allocations and duplicate specialized copies of the recursive
  body; and
- record worker size/arity, setting numerical budgets only after the direct
  and initial Pure Borrow baselines exist.

Names such as content-view constructors or `After` may disappear through
type/newtype erasure, and
names such as `nospec` vary by GHC. Keep the feedback's negative-name list as
diagnostic reporting rather than the proof oracle. Use version-aware
structural checks in each supported-GHC CI job and retain a readable Core
excerpt for failures.

### Allocation

Build and reset input state outside the measured kernel, and sweep node/visit
counts. For R3b, also sweep no-growth, sparse-growth, and dense-growth cases
and report slopes against both visits and frozen reopen counts. Prefer scaling
one fresh-state traversal until it is long enough for
meaningful counters. If repetitions must be batched, prebuild independent
states outside measurement and consume exactly one fresh state per repetition
in both variants; validate counters and digest for every repetition. Never
rerun a drain over already-mutated state. Gate on the Pure
Borrow-minus-direct bytes-per-visit slope with a tolerance chosen from the
baseline, not on total program allocation being constant.

Report slice-metadata allocation per scalar `getContents` separately from
explicit `Muts` spine construction/reconstruction and from per-visit
allocation. Initial spine construction stays outside the measured kernel.
Where R3b must rebuild an inner spine after growth, report its boundary term
against inner group size and reopen count. The per-visit slope must still match
the direct control within the baseline-derived tolerance.

Report total allocation, bytes per visit, maximum residency, bytes copied, GC
time, and mutator time separately. Lower total allocated bytes alone does not
rule out retention, live-set, cache, or code-layout effects.

### Runtime

Run R1, R2, and R3 single-threaded with identical builds and `+RTS -N1`.
Exclude or include setup/reset/freeze identically in each pair and state the
choice. Scale a fresh workload above timer/process-start noise, or use the
prebuilt-independent-state batching protocol from the allocation section.

Use alternating fresh-process pairs after declared warm-up. Analyze paired log
ratios, with the pair as the resampling unit and a predeclared 95% one-sided
paired-log bootstrap upper confidence bound with recorded seed. Choose and
freeze the minimum sample count using an independent variance pilot whose
observations are excluded from the final campaign; otherwise predeclare a
sequential stopping rule. Declare the outlier policy before the final run.
Report geometric mean and confidence interval for each predeclared
workload/outcome case as well as any aggregate, so an aggregate cannot hide a
`3blocks`-style outlier.

- Initial engineering margin: upper confidence bound no more than 1.10 versus
  the direct control.
- Desired release target: consider 1.05 only after the variance study shows
  that a small-single-digit gate is stable.
- Do not use wall-clock thresholds in ordinary shared CI. Structural Core and
  allocation-slope checks are the portable CI gates; paired timing belongs in
  a controlled performance job.

If Core passes but runtime fails, investigate code layout, worker size/arity,
enqueue/resume boundaries, strictness, and backend operations before changing
`BO`. If R2 passes but R3 fails, the difference is evidence about the wider
general worklist/control path. If local regressions pass but Herbrand does not,
reduce a new difference from the exact production snapshot before expanding
upstream APIs.

### Repository validation for a later implementation

- Format Haskell with Fourmolu and package/project files with cabal-gild.
- Use HLS diagnostics during iteration.
- Run phase-focused tests, then `cabal build pure-borrow`,
  `cabal test pure-borrow-test`, and `cabal test pure-borrow-doctests` on the
  locally selected GHC 9.12.4.
- Run version-aware inspection in the 9.10.3/9.12.4/9.14.1 CI matrix.
- Keep controlled runtime campaigns separate from the ordinary matrix.

## Delivery order and decision gates

1. **Freeze now-buildable evidence:** R1 plus R2/R3 specifications, direct
   controls, expected traces/digests, provenance schema, revised-API typing
   coverage, and the `assocBorrowEq` correction.
2. **Narrow R1 optimization:** prove the single-state-thread primitive and
   apply it only to scalar result-discarding scope paths; retain a direct
   `copyAtMut` implementation only if composition still leaves measured
   overhead.
3. **Boxed fixed-storage prerequisite:** move the existing fixed boxed
   representation behind its safe/`.Internal` boundary and establish the
   trusted constant-time slice constructor without changing the safe API.
4. **Minimal growable boxed MVP:** settle its stable header and land boxed
   `getContents`/`withContent` with the one-read/no-write/slice gates, while
   supporting general Copyable/Consumable payloads without claiming arbitrary
   linear-element growth.
5. **Fixed unboxed MVP:** establish direct fixed-owner mutation on one new
   backend with the same safe/`.Internal` split.
6. **Growable unboxed MVP:** settle stable header and logical-length
   publication, establish `getContents`, then derive the short-sublifetime
   `withContent` convenience.
7. **R2 and R3 composition:** validate constructive `reborrowings` over
   statically known `Muts` groups, direct per-member `getContents`, and nested
   reopenable groups. Land compiling exact-list, backend-qualified shape
   fixtures—including linear recovery, only-needed spine reconstruction,
   push-or-extend/reopen, and explicit meet reassociation—before implementing or
   timing R3b.
8. **Evidence-directed delimiter and PB optimization:** only now consider
   runtime-erasing plural/result-producing combinators, then apply
   representation/inlining/worker changes justified across R1–R3; consider
   `BO` changes only at the stated evidence gate.
9. **Bulk operations and promotion:** add measured batch operations, finalize
   docs, and promote APIs deliberately.
10. **Downstream campaign:** rerun the corrected exact-checkpoint Herbrand
   comparison as external validation, and decide from new evidence whether
   residual work belongs in general containers, GHC code generation, or the
   downstream application.

Each phase has a focused gate; the full provenance campaign is not repeated
before its required containers exist.

## Explicit non-goals

- Replacing `BO` based only on the current feedback.
- Adding solver-specific types, propagation operations, or a Herbrand
  dependency.
- Moving the whole solver kernel into an upstream unsafe primitive.
- Reopening the already-closed Herbrand conflict-analysis algorithm gap.
- Allowing growth through a fixed-length content borrow.
- Calling capacity-wide access safe without an initialization protocol.
- Supporting arbitrary linear boxed-element growth without a separate move,
  partial-initialization, exception, and consumption proof.
- Treating a moving downstream branch name as benchmark provenance.
- Promising end-to-end parity before the corrected downstream campaign exists.
