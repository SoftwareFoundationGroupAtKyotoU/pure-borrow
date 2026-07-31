# Pure Borrow Performance and Borrowed-Container Plan

## Status and scope

This document records both the remaining plan and the status of implemented
prerequisites. It responds to
`workspace/FEEDBACK-FOR-PURE-BORROW.md`, collected during the ongoing port of
the [Herbrand CDCL SAT solver](https://github.com/konn/herbrand) on branch
`konn/pure-borrow`. The original feedback was produced against Pure Borrow
revision `79a0d1878ccbce8895039c253cc9e462a788d3f3`; the container follow-up was
tested at `cdf6368178ee03a7e6db49c0bcd4a329b89598d5`. The moving downstream
branch is context, not benchmark provenance.

The `cdf6368` downstream experiment materially changes the active priority.
Herbrand proved that both whole-record reborrow/split and statically known
`Muts`/`reborrowings` transactions recover every lender and reproduce the
direct-control trace. No new multi-store combinator is required. The remaining
measured gap was the ordinary element-owning fixed view returned by growable
`getContents`: its copied reads and displaced-value writes kept the recursive
worker above the frozen Core and runtime gates.

The current branch goes one step beyond that experiment. Commit `4aabfae`
implements
`Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted`, whose
`getContents` projects to the already implemented backend-generic unrestricted
fixed vector. This is the first implementation of the downstream feedback's
preferred completion. Signed commit `40b458a` completes P0a's dedicated
ownership/typing gate, and signed commit `8db6d4a` completes the P0b six-root
`MultiStoreScan` integration and its GHC 9.12.4 structural gate. Signed commit
`78f8ce2` completes P1 attribution, the header-matched comparator, allocation
measurement, and the paired runtime gate. Signed commit `25485e5` implements
P2's resumable R3a/R3b benchmark and freezes its semantic baseline, reviewed
GHC 9.12.4 O2 Core, whole-root allocation, and selected paired-runtime
evidence. Three independent adversarial reviews found no soundness or
ownership violation. The performance review did find comparator, Core-scope,
and provenance gaps; the current worktree fixes the comparator and preserves
the reviewed evidence limits below. The current orthogonal follow-up holds
capacity or batch size fixed and shows that dense-drain overhead survives with
zero buffer reallocations. This promotes a narrow plural-boundary attribution
investigation—not an assumed optimization—while broad `BO` representation
work remains deferred.

The earlier Tamagoh-driven work recorded in the previous version of this file
is already present in the repository: direct inlinable `BO`/`After`/`Par`
methods, `subShare`, and hot-loop API guidance. R1 also has a provisional
direct `copyAtMut` fast path.

The current worktree additionally contains boxed and unboxed fixed and
growable owners. These four Pure Borrow families are element-owning: they
exclusively own the resources represented by their entries and bind those
entries linearly. The growable variants are exposed from
`Data.Vector.Mutable.Growable.Linear.Borrow` and
`Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow`. They use stable `Ref`
headers over logical length plus replaceable backing storage, project only
their initialized prefixes through `getContents`, and deliberately cannot be
split. Growth destructively transfers the initialized prefix into fresh
uninitialized storage, so `reserve` and `push` do not require element
`Copyable` or `Consumable` constraints. Focused model, ownership, role,
negative-typing, content-scope, and benchmark-equivalence coverage is in
place.

A fifth fixed-capacity family is implemented at
`Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted`. Its public
`Vector v a` keeps the owner and backing storage linear while binding
elements nonlinearly as GC-owned values. The backend parameter remains public;
safe immutable construction copies, consuming freeze is O(1), live snapshots
copy the backing, and no element capability class participates in ordinary
container operations. The qsort and FFT APIs using this family remain backend
polymorphic. Only their benchmark roots select the unboxed backend.

The corresponding sixth, growable family is implemented at
`Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted`. It retains
the same unrestricted element policy and public backend parameter behind a
stable header, supports reserve/push/extend, freezes the initialized prefix in
O(1), and projects that prefix as the fixed unrestricted family. Semantic,
backend-role, no-element-capability, negative-typing, and small specialization
coverage exists. P0a now completes its growable/fixed coercion and subtype
directions, lifetime-index, generic-split, borrow-to-content, mutable/shared
escape, and kind/lifetime-preservation matrix. It also records the trusted
header-peek/lifetime-shortening proof and adds a generic-family compile-failure
fixture for growth while content is live. The focused 37-case suite, complete
281-case main suite, and 20 doctests pass on GHC 9.12.4; the dedicated
owner-reuse fixture is rejected with the expected multiplicity error. The
heterogeneous `MultiStoreScan` worker-shape, attribution, allocation, fair
direct-comparator, and paired-runtime gates now pass on GHC 9.12.4. R3—not
another container design spike—now controls its next performance promotion.

The boxed and unboxed growable optimized-Core smoke checks show one header
read outside each fixed-view worker and no header operation in those workers.
The unboxed worker reduces to `readIntArray#`/`writeIntArray#` recursion, while
the whole Pure Borrow root retains its one-time
`askLinearly`/finalization boundary. The unboxed benchmark now separates two
different questions under descriptive names:

- `kernel/...` roots use benchmark-local, monomorphic `Int` finalizers after
  owner reclamation. They exclude public `Movable` materialization solely to
  isolate update and growth work; they are not public-API parity evidence.
- `public-materialization/...` roots exercise the real consuming `toVector`
  boundary of the element-owning family, including one `move` call per owned
  element. They do not predict the freeze cost of a distinct
  non-element-owning family.

At 1,048,576 elements, a noisy kernel run measured 1.22 ms direct versus
1.48 ms Pure Borrow for fixed storage, 1.25 ms versus 1.48 ms for growable
no-growth, and 4.33 ms versus 8.25 ms for forced geometric growth. The
separate public-materialization roots measured 128 μs and 8.0 MB versus
13.4 ms and 56 MB for fixed storage, and 127 μs and 8.0 MB versus 11.8 ms
and 56 MB for growable storage. These are smoke results, not parity claims:
the public Pure Borrow roots include the required per-element `move`, while
the direct controls merely freeze GC-owned `Int` storage. Semantic tests
verify equal complete results at both evidence boundaries and the same final
geometric-growth capacity.
The upstream direct six-root multi-store scan is frozen under descriptive
`MultiStoreScan` names. A separate evidence root observes operation counts at
the actual access sites and matches the uninstrumented benchmark root's output
and digest. Herbrand's retained downstream copy completed safe candidates at
`cdf6368` covering both supported ownership shapes and the replacement of all
three genuinely fixed roots with unrestricted views. All matched 4,096 visited
indices, the 26,318-event ordered trace, and every frozen digest. The
ordinary worker measured 196 Core lines/10,760 characters and about
349--350 us; changing the three genuinely fixed roots to the unrestricted
family reduced it to 167 lines/7,540 characters and about 197--199 us, but
still failed the frozen 125-line/4,504-character generic gate.

P0b now ports those validators upstream and adds direct-record and nested
`Muts` all-unrestricted candidates. Each candidate constructs six independent
owners, opens the three growable content views once, consumes every view,
recovers every lender, and materializes every owner exactly once. Both match
the complete trace and all frozen digests. On GHC 9.12.4 O2, both roots call
one shared 124-line/4,192-character recursive worker. That worker contains
exactly five `readIntArray#`, one `readArray#`, and two `writeIntArray#`
operations, with no growable-header operation, `Aliases`, `getContents`, or
reborrow scaffolding. Each whole root contains exactly three
`unsafeReadRef#` header reads and no header write. The benchmark module object
is 250,632 bytes. The main suite passes 283 tests, the inspection suite passes
16 tests including six new stable negative-Core checks, and all 20 doctests
pass. This is a semantic and structural success, not a runtime claim.
Signed commit `8db6d4a` records that P0b boundary.

P1 retains the all-owning and fixed-unrestricted-only candidates in the same
module and adds a direct control whose three locally allocated headers cannot
be optimized away. Their GHC 9.12.4 O2 recursive workers measure
191 lines/10,141 characters, 161/6,898, and 120/3,989 for all-owning,
fixed-only-unrestricted, and all-unrestricted respectively, versus
101/3,629 for direct. The all-unrestricted worker remains one recursive SCC
with the same five `readIntArray#`, one `readArray#`, and two
`writeIntArray#` operations. Its explicit local `Int64#` loop keeps the digest
unboxed until the ownership boundary; it adds no unsafe ownership operation or
public API. The benchmark module, now including all attribution controls, has a
332,840-byte object; a separate allocation harness measures fresh,
independently prepared traversals.

Before that worker-shape correction, the same-build pilot measured 354 μs
all-owning, 199 μs fixed-only-unrestricted, 67.4/67.0 μs for the two
all-unrestricted shapes, and 58.9 μs for both direct controls. The predeclared
21-pair direct-shape campaign failed with a 1.147362× geometric mean and
1.154867 one-sided 95% paired-log bootstrap UCB. Core and allocation then
identified one boxed `Int64` accumulator per recursive call. After the
explicit unboxed loop, 1,000 independent traversals allocate 197,201 bytes
each for both direct controls, 1,542,225 all-owning, 952,449
fixed-only-unrestricted, 199,113 all-unrestricted direct-shape, and 199,353
all-unrestricted nested-shape. The selected safe shape is therefore within
1,912 bytes per traversal, or 0.467 bytes per visit, of the header-matched
direct control.

The final 21 alternating fresh-process pairs, after two excluded warm-up pairs,
produce a 1.014222× geometric mean and 1.019416 one-sided 95% paired-log
bootstrap UCB with seed `0x50b02026`, 100,000 bootstrap samples, and no outlier
removal. This passes both the initial 1.10 engineering margin and the desired
1.05 target on this machine. A separate nine-pair pilot measured nested over
direct composition at 0.996514× with a 1.000709 UCB; the difference is not a
reason to prefer the more elaborate nested form, so the direct-record shape
remains the default. `bench/run-multi-store-scan-paired.mjs` preserves the
fresh-process pairing/bootstrap protocol, while the
`multi-store-scan-allocation` benchmark component preserves the
prebuilt-input, GC-flushed allocation measurement. Supported-GHC inspection
for P1 remains pending; the current P2 evidence follows.

P2 now freezes a benchmark-local deterministic graph/worklist under
`PureBorrow.Internal.Bench.Worklist.Resume`: 4,096 nodes, degree three,
fixed offsets/marks/state roots, growable adjacency/payload/queue/log roots,
mark-on-enqueue traversal, and both complete drain and 1,365-visit early stop.
The direct and safe roots agree on every final vector and all operation
models for open-once plus flat and hierarchical reopen shapes across
no-growth, sparse-growth, and dense-growth schedules. These access counts are
derived from visits/enqueues rather than independently instrumented; final
vectors and digests are the semantic oracle. Drain performs 4,096
visits, 4,095 enqueue transitions, 12,288 edge/payload/mark reads, and ends at
digest `2728622868939553119`; early stop performs 1,365 visits, 2,888 enqueue
transitions, 4,095 edge/payload/mark reads, and ends at digest
`5952155574826728904`. Seeded roots provide independent fresh traversals for
allocation measurement without changing direct-versus-safe equivalence.
Tests now require seeds 1 and 37 to produce different digests and require
repeated/interleaved evaluations to remain deterministic.

R3a opens four growable contents once and performs no resume, header-update,
or growth boundary. R3b freezes 20/67/513 drain resume boundaries for batch
sizes 256/64/8 and 10/24/172 for early stop. Initial capacities 4,096/256/1
produce no/sparse/dense growth. Two orthogonal controls retain capacity 4,096
while selecting batch 64 or 8, so they reproduce the matching sparse/dense
resume, open, and header-update counts with zero buffer growth. Flat grouping
opens four contents per segment; the selected hierarchy holds shared graph
views plus marks/state in one outer scope and reopens only the two-member
queue/log frontier. Its observed open formula is `2 + 2 * segments`, versus
`4 * segments` for flat. The complete main suite still passes 286 tests and
the inspection suite passes 22 tests. The automated inspection tests cover
the already projected open-once and resume edge workers only: they exclude
dictionaries, growable headers, plural bundles, projection/extend/reborrow
calls, and generic fixed-vector access calls. They do not prove the whole
resume boundary.

A separate manual O2 dump closes the immediate comparator question and scopes
the remaining Core work. Direct flat and nested controls compile to distinct
recursive drivers (787 and 765 Core terms), with no per-segment `Maybe`
shape dispatch. Pure Borrow flat and nested drivers are 1,559 and 1,457 terms
respectively and each enters one shared resume SCC. The shared main resume
worker is 188 terms versus 155 for the direct segment worker; its one
specialized edge worker is 163 terms versus 145 for the direct edge worker.
Together the safe main/edge workers contain the expected five unboxed reads,
one boxed read, and one unboxed write, with no growable `Ref`, `GrowableVector`,
`Aliases`, or `reborrowings` operation. Static boundary code has six
`unsafeReadRef#` sites in the flat driver and four in the hierarchy because
the latter keeps graph views open. Exact occurrence/allocation counts are
manual evidence: inspection-testing 0.6.3 cannot express the required
whole-boundary count, size, duplication, or arity obligations.

The fresh-seed allocation harness measures the complete root: symmetric
allocation/reset plus materialization of all seven direct and safe owners are
inside the interval. It reports the initial capacity, batch size, visits, and
resume counts separately. R3a's safe-minus-direct allocation is exactly 2,360
bytes for both drain and early stop (0.576 bytes per drain visit). For
hierarchical R3b, safe-minus-direct whole-root excess is 8,296/5,592 bytes for
capacity 4,096/batch 256. At batch 64 it is 21,016/9,400 with capacity 4,096
and 21,016/9,416 with capacity 256 and actual growth. At batch 8 it is
140,904/49,656 with capacity 4,096 and 140,888/49,672 with capacity 1 and
actual growth. Each pair is drain/early. Thus holding resume count fixed while
adding growth changes the safe-minus-direct excess by at most 16 bytes per
root. The material excess is therefore present without buffer replacement and
is essentially unchanged when growth is added at fixed resume counts. Merely
normalizing `excess - 2,360` by frozen resume count remains a descriptive
comparison, not a decomposition of the plural boundary.
Dense-drain hierarchy allocates 78,160 fewer bytes than the safe flat root in
the original baseline, while their direct controls differ by only 32 bytes.

This is still a preliminary whole-root allocation gate: it has one graph size,
two visit targets, and no prebuilt-state kernel interval. The orthogonal
controls separate growth from the resume schedule but do not separate fixed
setup from the per-resume term. `max_live_bytes` is process-lifetime high water
including startup and the pre-measurement sample, not interval residency.
Allocated/copied/time fields are interval deltas; the printed
`total_bytes_per_resume` is total allocation per resume, not safe-minus-direct
boundary excess. Each measured root is fully forced with a unique seed, but
the loop takes visits/resumes from the pre-measurement sample and aggregates
digests without an independent per-repetition oracle. Per-repetition
structural/digest validation remains part of the prebuilt-state harness gate.

The selected runtime evidence uses the final O2 source and one executable:
alternating fresh processes, two excluded warm-up pairs, 21 retained pairs,
5% tasty-bench standard-deviation target, no outlier removal, seed
`0x50b02026`, and 100,000 paired-log bootstrap samples. R3a drain is
0.972877×/0.976004 one-sided 95% UCB. Hierarchical sparse drain is
1.030250×/1.039778 and passes 1.05. Dense early stop is
1.009929×/1.029168. Dense complete drain is
1.075265×/1.097431: it narrowly passes the 1.10 engineering gate and misses
1.05. A direct same-build comparison selects hierarchy over the safe flat
root at 0.965671×/0.973470 UCB. The correlated dense signal motivates
boundary attribution, but that original coupled-capacity campaign alone did
not prove whether reopening or growth was the cause. The orthogonal follow-up
below closes the immediate question of whether growth is required or a
material contributor for this workload; it does not identify the remaining
boundary cost by itself.

The checked-in provenance manifest for those selected rows is:

- benchmark source SHA-256:
  `550d242486026b519e0314e97506663f42e2efbcc29f326df467fd128a60f9f2`;
- executable SHA-256:
  `46268847f7640bfa17ce1fda1f7df011f2e96a3cadc606dab13502f4eaf4cd93`;
- runner SHA-256:
  `36acb2e185bbce19d5286744ff240fbdd85b8e34789b545fdab53a38fcedf352`;
- GHC 9.12.4, `bench-suites` and benchmark executable both `-O2`, wall time,
  `-N1`.

| Selected case | Geomean | One-sided 95% UCB | Raw JSON SHA-256 |
| --- | ---: | ---: | --- |
| open-once drain, safe/direct | 0.972877 | 0.976004 | `39224628c12d0fcce006c433cb96eb99a224408c974580d7bd3b0ae8a916db38` |
| sparse drain, hierarchical safe/direct | 1.030250 | 1.039778 | `9ed54525d9aa0fc2b881b1dc4a8e2912c61c6f8142f12417821ca9024ff0c8bc` |
| dense drain, hierarchical safe/direct | 1.075265 | 1.097431 | `75443179439fa20e0ca54bcf16f5de52214773cbd6a6d20832ce5ea33d4f89d5` |
| dense early, hierarchical safe/direct | 1.009929 | 1.029168 | `06d0ac74ceea51fa017093f91b25ba26a5fbe7610027fef98fcbc4f956a8f48b` |
| dense drain, hierarchical/flat safe | 0.965671 | 0.973470 | `d00a7d51435b9e071b073690d4dac4a80bc7a04c14b45b23498f3a8855171a75` |

The orthogonal drain campaign uses one later O2 executable and the same
21-pair protocol. Its source SHA-256 is
`1ab9422c76cc2fed5a8e4bd92e21dd66a5520dd750964e1694f0554f2bef9da9`
and executable SHA-256 is
`a0d1c2af13698673b38268e19522e8d4e9293edf5102de712dc6f1244d00bec0`;
the runner hash is unchanged. The two same-batch comparisons show that actual
growth is not required for, and does not materially increase, the safe/direct
ratio. The high-resume batch-8 control misses 1.05 even with capacity 4,096 and
zero buffer growth.

| Capacity / batch / growth | Resumes | Geomean | One-sided 95% UCB | Raw JSON SHA-256 |
| --- | ---: | ---: | ---: | --- |
| 4,096 / 256 / none | 20 | 1.015817 | 1.021214 | `26697fdcb14780613c69f346317ee4d56952fbe527451fee339aa91ee3d4926c` |
| 4,096 / 64 / none | 67 | 1.015723 | 1.022836 | `21027e1a345fb2bd298c94e57199fcca2927a7da0d1f9a965af9a685fd913ff5` |
| 256 / 64 / sparse | 67 | 1.016398 | 1.020767 | `01df7634455652206f37f60e58569c520fb491c2b151be91deb06bbcd08036e6` |
| 4,096 / 8 / none | 513 | 1.069158 | 1.073317 | `5abe7dd0bd278c6d472fff1cc8827eb131aebf2e090b5338aabf0857dad5b842` |
| 1 / 8 / dense | 513 | 1.063709 | 1.066591 | `b3dcd644794c65e2a10a7373c2669ea94768ff191c68dcb495bdd3be8c168d31` |

The raw CSV/JSON paths remain ignored under
`bench-results/worklist-resume/reviewed/`; the manifest retains their digests
and the exact reproducibility boundary. The orthogonal capacity/batch control
and no-growth drain campaign are complete. Remaining early-stop campaigns,
the node-count/prebuilt-state allocation sweep with per-repetition validation,
automated whole-boundary Core extraction, exact-one-row paired-runner
hardening, and supported-GHC checks keep P2 active.
`worklist-resume-allocation` and
`bench/run-worklist-resume-paired.mjs` preserve the fresh-seed allocation and
fresh-process paired protocols under benchmark-specific names.

On the pinned GHC 9.12.4 O2 build, paired single-capability runs against the
previous boxed element-owning roots measured:

- qsort at 8,192 elements: 1.35 ms and 2.1 MB before, 0.435 ms and
  2.4 MB with the unboxed unrestricted root;
- qsort at 32,768 elements: 6.88 ms and 8.5 MB before, 3.21 ms and
  9.5 MB after;
- FFT at 65,536 elements: 18.8 ms and 74 MB before, 3.26 ms and
  26 MB after; and
- FFT at 1,048,576 elements: 635 ms and 1.4 GB before, 57.9 ms and
  418 MB after.

The monomorphic benchmark Core contains primitive unboxed array reads and
writes in the recursive workers, without generic-vector method calls there.
The qsort object shrank from 147 KB to 142 KB even though its Core text grew
from 245 KB to 279 KB. The FFT object grew from 105 KB to 142 KB, and its Core
text from 57 KB to 119 KB. The final specialization shape has exactly one
primitive recursive combine worker. A second, non-recursive entry worker
performs only the first iteration before entering that recursive SCC; the
generic defining module is compiled without `SpecConstr` so it does not also
retain a recursive specialized clone.

A dedicated O2 `pure-borrow-inspection` suite now materializes the stable
negative Core facts. Each monomorphic term and its inspection cases live in
the same module and the term has an explicit type signature. The FFT combine
root has no type-class dictionaries, boxed mutable-vector backing, or listed
generic-vector operations. The qsort root has no boxed mutable-vector backing
or listed generic-vector operations and may retain only its concrete
`Vector` and `Ord` dictionaries. The ordinary GHC matrix runs this suite
through `cabal test`. Positive primitive-operation presence, exact recursive
SCC and entry-worker counts, object size, and compile/simplifier cost are not
expressible by these inspection predicates and remain version-aware
diagnostics.

One forced O2 compile of the benchmark FFT module reported 675 ms total and
327 ms in simplifier passes, versus 352 ms and 132 ms for the previous
module. The approximately 323 ms absolute module-compile increase is recorded
as part of the specialization tradeoff; supported-GHC CI must catch any
substantially worse shape or cost.

The permanent same-root decomposition benchmark separates element ownership
from storage representation. On GHC 9.12.4 O2 with one capability:

- qsort at 8,192 elements measured 1.48 ms and 3.2 MB for owning boxed,
  644 μs and 2.1 MB for unrestricted boxed, and 392 μs and 2.3 MB for
  unrestricted unboxed;
- qsort at 32,768 elements measured 7.50 ms and 14 MB, 3.97 ms and 8.5 MB,
  and 2.65 ms and 9.0 MB respectively;
- FFT at 65,536 elements measured 19.2 ms and 79 MB, 23.3 ms and 114 MB,
  and 3.35 ms and 26 MB respectively; and
- FFT at 1,048,576 elements measured 578 ms and 1.5 GB, 775 ms and 2.2 GB,
  and 58.1 ms and 418 MB respectively.

Thus qsort benefits first from the unrestricted element policy and then from
unboxed storage; its unboxed backend adds 0.2–0.5 MB relative to unrestricted
boxed storage while materially reducing time. FFT does not benefit from
unrestricted boxed storage: its improvement is specifically the primitive
unboxed specialization. The approximately 35% FFT benchmark-object growth is
a deliberate, bounded specialization tradeoff for about 10× lower runtime and
72% lower allocation at the large root. It remains acceptable only while the
stable inspection obligations pass and the supported-GHC diagnostic
inspection retains one primitive recursive hot SCC; a second recursive worker
remains a rejection.

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
| The former compositional `copyAtMut` expanded through `sharing`/`srunBO`/`askLinearly`, and optimized Core retained lifetime/dictionary machinery. | Confirmed by downstream Core inspection. The current provisional direct fast path avoids this expansion; R1 retains the former composition as a control. | Keep the direct path and R1 control. Revisit general `sharing` erasure as maintenance only after R2/R3, unless a new named root makes it a blocker again. |
| Opening the backing content once around a loop can remove repeated outer-header opening. | Confirmed in the complete six-root all-unrestricted R2 candidate on GHC 9.12.4: each whole root performs three header reads and no header write, while the selected 120-line/3,989-character worker contains only the expected primitive backing-array operations and an unboxed digest accumulator. Final paired runtime is 1.014222× with 1.019416 one-sided 95% UCB against the header-matched direct control. | Retain `getContents` plus the trusted rank-2 `withContent` convenience. Carry the structural checks across supported GHCs and reuse the same path in R3; do not design another scope API without a newly identified residual operation. |
| Herbrand's former `Data.Set` conflict-analysis cost was a downstream algorithm gap. | Confirmed and already removed downstream. | Do not design a Pure Borrow change around this obsolete gap. |
| Historical paired runs contain a 6–10× `3blocks` slowdown despite less total allocation and comparable search work. | The historical campaign had incomplete provenance, but it has now been replaced by a corrected 294-pair exact-commit campaign: combined ratio 1.1589 with 1.1818 95% UCB, 10/14 per-case gates passing, and both `flat200` and `3blocks` failing. A standalone production root-chain control measured 12.4291× while the conflict-analysis/insertion control measured 0.3308×. | Use the corrected results as downstream evidence, not as an upstream microbenchmark gate. Preserve separate compact R2 and resumable R3 regressions so the propagation/control-path signal is not misattributed to `BO`. |
| Pinning `cdf6368` and migrating `Alias` to `Borrow` may itself regress production. | Rejected by the corrected 448-pair API-adoption campaign: all results and per-case gates passed, combined geomean was 0.9912× with 1.0043× 95% UCB, and the six named hot/entry modules had byte-identical optimized Core. | Treat the dependency/API migration as complete and safe to retain. Do not attribute the separate experimental `MultiStoreScan` worker gap to that migration. |
| The remaining slowdown is caused by `BO`, or entirely by the uninterrupted six-store watch scan. | Not established. The production path still crosses trail-literal and unit-enqueue/resume boundaries. | Keep separate generic linked-scan and resumable-worklist regressions; do not redesign `BO` from this evidence. |
| Boxed and unboxed fixed and growable element-owning containers plus backend-generic fixed and growable GC-element implementations now exist. | The fixed GC-element family has backend-generic semantic tests, O(1) consuming freeze, capability-callback absence tests, nominal-role boundaries, and primitive monomorphic qsort/FFT Core. P0a completes the growable boundary and trusted-projection matrix; P0b completes heterogeneous whole-worker evidence; P1 passes attribution, allocation, and paired-runtime gates without another public surface. | Treat the container and current access surface as sufficient for R2 and carry it unchanged into R3. Do not reopen representation or broad `BO` design without a new residual operation. |
| Existing record reborrow/split and `Muts`/`reborrowings` composition may be insufficient for six heterogeneous stores. | Rejected by the `cdf6368` downstream experiment: both shapes recover every lender and reproduce the complete direct trace. Lifetime, record-splitting, `Muts`, and header operations are outside the recursive worker. | Treat composition as validated API usage. Keep both shapes as Core controls, but add no bundle-specific projection or Herbrand-shaped combinator. Focus on the projected growable content's element-access surface. |
| Replacing only the three fixed roots with unrestricted fixed vectors may solve the multi-store gap. | It materially improved the downstream candidate from about 349--350 us to 197--199 us and reduced Core from 196/10,760 to 167/7,540 lines/characters, but still missed the frozen structural gate. Same-build P1 reproduces the attribution at 354 μs all-owning and 199 μs fixed-only-unrestricted, then reaches 120/3,989 Core and runtime parity only after all six views are unrestricted and the recursive digest is explicitly unboxed. | Retain all three shapes as permanent attribution controls. The completed evidence selects the all-unrestricted direct-record shape; no new access API is justified. |

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

Element ownership is an independent container-design axis:

- Every family considered here linearly owns its mutable backing storage.
  Element ownership determines how the values stored in that backing are
  bound; it does not determine storage ownership.
- An **element-owning** container exclusively owns the resources represented
  by its entries and binds them linearly. Consuming materialization into an
  unrestricted result must require `Movable` and invoke `move` for every
  initialized entry.
- A **non-element-owning** container binds entries nonlinearly. Those entries
  are already GC-owned, so consuming freeze may transfer the backing storage
  to an immutable vector in O(1) without an element capability operation.
- This choice is orthogonal to boxed versus unboxed representation, fixed
  versus growable storage, and mutable versus immutable access. The complete
  design space therefore has a separate element-ownership dimension; no API
  may infer one axis from another.

The fixed and growable boxed and unboxed representation-specific Pure Borrow
vectors implemented in this plan are element-owning. The fixed and growable
non-element-owning families are distinct public types in `Unrestricted`
modules, so their nonlinear element binding and O(1) freeze contract cannot be
confused with those owners. Their addition does not weaken the existing
owners' materialization boundary or their `Movable` tests.

Avoid multiplying the storage implementation across this matrix. Expose the
new GC-element family from
`Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted`, with the
public type
`Vector v a = Vector (Mutable v RealWorld a)` parameterized by the immutable
`vector` backend `v`. The constructor remains hidden. The implemented
growable module exposes `GrowableVector v a` analogously. An `MVector mv a`
constraint alone supplies mutable operations but no immutable-vector operation
or result in the API; parameterizing by immutable `v` supplies both its
associated `Mutable v` representation and the correct generic freeze result
without inverse-family or equality plumbing.

The public backend parameter is an extensibility and trust boundary. Safe
operations rely on the standard `Data.Vector.Generic.Vector` and `MVector`
contracts: fresh allocation, exact lengths and bounds, reads and writes that
affect only the addressed range, disjoint slices for disjoint index ranges,
and the documented freeze/thaw alias behaviour. No operation may assume a
stronger backend property without a separately audited constraint. Declare
both `v` and `a` nominal, hide the representation constructor, and reject
coercion across backends, element types, and the existing element-owning
families.

Keep the element-owning and GC-element policies as distinct public types in
distinct modules; do not encode their different surfaces through an open
ownership-policy class. Keep algorithms such as qsort and FFT backend
polymorphic in their public APIs. Their benchmark roots select a concrete
backend and provide the monomorphic boundary at which optimized Core must
erase generic backend and element-representation dispatch. Specialization is
a benchmark-side performance requirement, not a soundness premise or a reason
to close the public API.

The experimental multiplicity-polymorphic generic vector is not a shortcut
between these policies and is outside this performance track. Its `One` mode
currently routes linear elements through ordinary generic-vector operations,
while the public `Data.Vector.Generic.Vector`/`MVector` contracts do not state
the multiplicity-preservation laws needed to justify that transport for every
custom backend. Before it can share implementation with a growable family or
serve as promotion evidence, give it an independent backend-by-backend
linearity proof/audit, reject `One`/`Many` coercion explicitly, and show that
construction, reads, writes, freeze, list conversion, cloning, and splitting
consume each linearly owned element exactly once. Until then it remains
experimental and quarantined from R2/R3; this plan neither treats it as a
soundness counterexample nor relies on it.

The two ownership modes have deliberately different surfaces:

- element-owning reads return lifetime-indexed element borrows, replacement
  returns the displaced element linearly, retirement uses `Consumable`, and
  consuming materialization uses `Movable`;
- non-element-owning reads and displaced elements are unrestricted, updates
  and insertion bind their arguments nonlinearly (or consume an explicit
  `Ur a`), owner retirement consumes only the backing storage and needs no
  element capability, and consuming freeze is O(1). It does not expose mutable
  element borrows that would falsely claim exclusive ownership.

Both fixed splitting and growable `getContents` preserve the backend and
element-ownership mode. Roles and negative typing tests must reject coercion
between ownership modes and between boxed and unboxed backends.

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
- `Copyable` authorizes copied reads; it does not authorize retirement.
  Evaluating `copy` must complete the copy and return weak-head-normal form.
  Container copied-read paths must force that result before the source borrow
  ends or a mutable borrow is recovered. Composite instances must complete
  every recursive component copy before returning.
  Growable relocation is instead a destructive ownership transfer: allocate
  fresh uninitialized storage, bulk-copy exactly `[0, logicalLength)`, abandon
  the now-inaccessible old backing storage, initialize any new suffix, and
  publish the new header last.
- An ordinary arrow binds its input nonlinearly. Immutable vectors, ordinary
  lists, and their elements are therefore GC-owned; cloning their backing
  entries must neither require nor invoke an element capability class. Such a
  value may be consumed repeatedly. Exactly-once retirement applies only when
  the value is bound linearly. `Copyable` is separate: `copy` consumes a
  `Borrow` linearly and is not an operation on an unrestricted source.
- Consuming freeze/materialization transfers elements from a linear owner into
  an unrestricted, GC-owned result and therefore requires `Movable`, not
  `Copyable`. It invokes `move` on every initialized element, permitting the
  `Movable` instance to perform any necessary deep copy before the backing
  storage becomes unrestricted.
- Existing `NOINLINE`/`GHC.noinline` barriers around linear lifetime witnesses
  are not weakened. A zero-cost `sharing` path must avoid needing a new
  `Linearly` witness by construction, not expose or inline the existing
  witness constructor.
- Trusted backing-array operations are confined to container `.Internal`
  modules. Under this repository's convention those modules are exposed for
  package composition but Haddock-hidden; safe algorithm modules import only
  no-suffix or explicitly experimental APIs.

Before the unrestricted growable family enters R2, review its trusted
`getContents`/`withContent` implementation as a proof obligation, not merely a
test target. Record why the `unsafeReadRef` peek and discarded duplicate
header handle consume the input occurrence exactly once while leaving the
authoritative header dormant in its lender; why the lifetime coercion changes
only the phantom lifetime; why the callback executes exactly once; and why
the retained growable capability is inaccessible until all fixed pieces end.
Audit the complete fixed unrestricted surface to confirm that `Mut` cannot
coexist with reserve/growth, `Share` cannot mutate, the slice is exactly
`[0, logicalLength)`, and no safe operation can resize, consume/freeze the
backing, or expose capacity. Nominal backend/element roles must prevent
selecting incompatible backing operations.

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

The regression suite was staged around progressively available container
support. All fixed and growable, element-owning and unrestricted families now
exist. The upstream `MultiStoreScan` direct control and all-unrestricted
candidate are frozen; downstream ordinary and fixed-unrestricted candidates
are now upstream attribution controls as well. P1 passes on GHC 9.12.4; R3
remains pending.

**R1 — Existing boxed-vector copied-read loop.** This is immediately buildable.
It compares the current public `copyAtMut` loop with an equivalent direct boxed
mutable-vector control. It freezes semantics, bounds behaviour, optimized Core,
and allocation before changing `copyAtMut`.

**R2 / MultiStoreScan — six-root heterogeneous linked scan.** The immediate
upstream candidate contains three fixed unrestricted unboxed roots, one
growable unrestricted boxed root, and two growable unrestricted unboxed roots.
It linearly separates the six independently constructed owners, opens each
growable content once, performs a deterministic 4,096-node linked scan, reads
all roots, conditionally writes two roots, closes the content scopes, and
reclaims all owners. Keep the all-ordinary and fixed-unrestricted-only
downstream shapes as exact attribution controls rather than replacing them.

`MultiStoreScan` is a general container/code-generation stress test. Its shape is motivated
by the feedback, but its specification is independent of SAT semantics.

Its direct control is frozen as `MultiStoreScan`: 4,096 visited nodes, 24,576
element reads, 1,742 element writes, three header reads, and final digest
`7192365686207673759`. The completed downstream candidates additionally
freeze the 26,318-event ordered trace, mark digest
`-5655863917889937928`, score digest `-1217547283655932101`, and complete
trace digest `-6999049615496738955`. The upstream evidence and both
all-unrestricted ownership shapes now reproduce that complete trajectory.
Code and benchmark names describe the workload; they do not use a
context-dependent round number.

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

Place semantic tests in the existing tasty tree. The optimization-enabled
`pure-borrow-inspection` component materializes stable negative Core
obligations for the monomorphic qsort and FFT roots with
`tasty-inspection-testing`. Define each inspected term in the same module as
its test case and give it an explicit monomorphic type signature; keep
`Main` only as the test-tree aggregator. Add later R1/R2/R3 roots by the same
pattern. Use version-aware Core extraction for positive primitive-operation
presence, recursive-SCC counts, entry-worker counts, size, and compile-time
diagnostics that the inspection predicates cannot express. Put runtime
workloads in a new internal benchmark module/component rather than the qsort
or FFT suites. Use cabal-gild discovery for new modules.

The ordinary `cabal test` step wires the inspection component into each GHC
matrix job. Keep paired wall-clock campaigns in a controlled/manual
performance job; current CI runs only the qsort benchmark in one matrix entry
and is not a stable timing environment. Run explicitly with `+RTS -N1`
because existing benchmark components may default to all capabilities.

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

In either case preserve `copyAtMut`'s checked bounds behaviour. Require
retirement-sensitive coverage showing that copied results reach weak head
normal form before mutable recovery or source-borrow termination. Accept the
change only when typing/soundness review, R1 semantics, optimized Core,
allocation, and runtime gates all pass.

### Phase 2 — Fixed-storage prerequisites and unboxed borrowed-vector MVP

The fixed-capacity unboxed owner is implemented under the stable, vector-aligned
module `Data.Vector.Unboxed.Mutable.Linear.Borrow`, with a Haddock-hidden
`.Internal` implementation and the owner type named `Vector`. This deliberately
mirrors `Data.Vector.Mutable.Linear.Borrow` and @vector@'s
`Data.Vector.Unboxed.Mutable` namespace. Stable exposure here records the
chosen public shape; it does not waive the remaining allocation, repeated
runtime, R2/R3, or supported-GHC gates.

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
- checked and unchecked `get`/`head`/`last` operations returning an element
  `Borrow` under `Unbox a`, matching the boxed vector;
- `copyAt` and `copyAtMut` producing `Ur`-wrapped copied values under
  `Unbox a` and `Copyable a`;
- checked and unchecked writes returning the displaced value;
- consuming materialization, requiring `Movable a`, invoking `move` for every
  owned element, and freezing the rewritten backing storage as an unrestricted
  immutable vector;
- explicitly copying materialization from a live shared borrow, as a distinct
  operation;
- a conventional `Consumable` instance requiring `Consumable a`; and
- direct mutation through `Mut`, without an additional scoped-view type for an
  already fixed-capacity owner.

All construction, replacement, update, swap, and element-wise consumption
ownership claims are normal-return guarantees; no owner recovery is promised
after a synchronous or asynchronous exception. Keep a possible tail-recursive
or single-pass replacement for the current recursive `fromList` length/fill
path as follow-up work if large-list stack or retention measurements justify
it.

### Phase 3 — Growable boxed and unboxed MVPs

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

The benchmark-prioritized implementation now provides both backend-specific
element-owning MVPs. Their common owner-level surface is:

- empty construction and construction with initial capacity;
- logical length and capacity;
- checked/unchecked copied reads and updates;
- `reserve`/`reserveAdditional`;
- `push` of one linearly supplied element;
- `extend` from an immutable input, which copies its GC-owned backing entries
  without an element capability callback;
- consuming freeze/materialization under `Movable`, with `move` invoked for
  every initialized element, and a conventional `Consumable` instance;
  and
- `getContents` for a same-lifetime initialized fixed-size projection, with
  `withContent` using the trusted runtime-erased rank-2 shortening described
  below and remaining observationally equivalent to `reborrowing` for `Mut`.

Safe construction or `extend` from an immutable input requires no element
capability class; the source and its entries are bound nonlinearly and are
GC-owned. The unboxed backend still requires `Unbox a` as a representation
constraint.
`reserve`, `reserveAdditional`, and `push` use the reviewed destructive
transfer described above, so relocation does not copy a value while keeping
the source reachable and does not retire the transferred values. Those
operations therefore need no element capability constraint for boxed storage
and only `Unbox a` for unboxed storage. Final owner retirement still requires
`Consumable a`; consuming materialization into an unrestricted result instead
requires `Movable a` and uses its `move` method for every initialized element.

Normal-return ownership is the guarantee: the returned owner contains the
original initialized prefix exactly once. Preflight size arithmetic and
capacity before mutation, do not run user callbacks during partial growth, and
document how synchronous/asynchronous exceptions are handled. Do not claim
that the old owner is recoverable after an exception until that is actually
implemented.

For both backends:

- copied read requires `Copyable a`;
- destructive relocation preserves each initialized value exactly once
  without invoking `copy` or `consume`;
- consuming the owner is provided only through its `Consumable` instance;
- non-growth replacement returns the displaced value; and
- batch move from a linearly owned source is deferred with arbitrary linear
  element support.

The later backend-generic unrestricted growable family is also implemented.
Its entries are GC-owned and bind nonlinearly, so ordinary reads, displaced
values, growth, retirement, and O(1) consuming freeze invoke no element
capability. Its `getContents` preserves both the backend parameter and
unrestricted ownership mode:

```haskell
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Unrestricted

getContents ::
  Borrow bk α (GrowableVector v a) %1 ->
  Borrow bk α (Unrestricted.Vector v a)
```

This is not an alternative representation experiment anymore; it is the
current candidate answer to the downstream request for unrestricted growable
content. First complete its ownership/typing matrix and review the trusted
projection/restoration proof. The subsequent work is to prove the complete
mixed boxed/unboxed transaction's Core, allocation, and runtime shape. The
element-owning families remain necessary for genuinely linear entries and are
retained as controls, but they are not the preferred Herbrand-shaped candidate
merely because they landed first.

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

Herbrand's `cdf6368` experiment closes the API-design question for the
no-growth transaction: both whole-record reborrow/split and a statically known
`Muts` subgroup are sound, recover every original owner, and produce the exact
direct trace. Phase 4 is therefore an upstream regression and code-generation
phase, not a search for another composition abstraction. P1 selects
whole-record reborrow/split as the simpler default after the two implemented
shapes measure equivalently; retain the nested `Muts` form as a control and as
the documented pattern for clients that already hold a heterogeneous bundle.

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

Use R1, R2, and R3 to optimize Pure Borrow itself in evidence order:

1. **Complete the unrestricted R2 transaction.** First express the six-root
   worker entirely through the current public unrestricted fixed and growable
   APIs. This is an integration step, not authorization for a new unsafe
   primitive. Compare the whole-record and `Muts` shapes and preserve the two
   older downstream candidates as attribution controls.
2. **Backend operations and worker shape.** If the all-unrestricted candidate
   still misses the frozen structural gate, specialize its checked/unchecked
   reads and writes to the boxed/unboxed backends, keep checks outside an inner
   unchecked worker where a checked transaction invariant proves bounds, and
   tune strictness, argument grouping, worker/wrapper boundaries, and
   specialization from measured Core. Prevent duplicate recursive bodies and
   per-visit boxed aggregate reconstruction.
3. **Resumable control.** Implement R3a and R3b after R2 is attributable.
   Keep enqueue/resume or push/reopen control inside the logical transaction
   and measure cost against both visits and reopenings. If R2 passes but R3
   fails, investigate the wider control path before changing any lifetime
   representation.
4. **Convenience and delimiter follow-up.** Revisit R1 copied-read fusion and
   runtime-erased result-producing/plural scopes only when their named roots
   still retain measurable machinery. Herbrand already places lifetime,
   record-splitting, `Muts`, and header operations outside its recursive
   worker, so these changes are no longer prerequisites for R2.
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

Stable modules and candidates, subject to their remaining gates, are:

- `Data.Vector.Mutable.Linear.Borrow[.Internal]`, with the current safe
  surface preserved and only the representation moved;
- `Data.Vector.Unboxed.Mutable.Linear.Borrow[.Internal]`;
- `Data.Vector.Mutable.Growable.Linear.Borrow[.Internal]`; and
- `Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow[.Internal]`.

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
  fixture: GHC 9.12.4 was verified to reject the resulting `Many`/`One`
  multiplicity mismatch while compiling the module even with
  `-fdefer-type-errors -Wno-deferred-type-errors`. Validate that fixture through
  the Cabal-selected compiler rather than adding it to the ordinary test
  component.
- Role/constructor tests cover fixed and growable boxed/unboxed owners as well
  as the fixed-content view. Element/backend roles must be nominal wherever a
  coercion could select different backing operations, and safe code must not
  construct a view over spare capacity.
- Before the generic unrestricted growable family enters R2, complete its
  typing matrix: reject growable/fixed coercion, both subtype/upcast
  directions, lifetime-index swapping, generic splitting, owner/content
  coercion, shared-content escape, and reuse or growth of the original
  mutable growable occurrence while a content borrow is live. Multiplicity
  reuse requires a separate Cabal-selected compile-failure fixture on GHC
  9.12.4 because its `Many`/`One` mismatch is not deferred even with
  `-fdefer-type-errors -Wno-deferred-type-errors`. Add positive tests that
  `getContents` preserves borrow kind and lifetime for both `Mut` and `Share`,
  and that `reborrowing`/`getContents` is observationally equivalent to
  `withContent`.
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

For the GHC 9.12.4 R2 evidence root, freeze these exact structural gates before
timing:

- the whole root performs exactly three growable-header reads and zero header
  writes;
- transaction opening, record splitting, `Muts`, `reborrowings`, and all three
  `getContents` projections remain outside the hot recursive SCC;
- the SCC contains one recursive body with five unboxed primitive reads, one
  boxed primitive read, and two unboxed primitive writes per logical
  iteration;
- the SCC contains no bounds-check branch, displaced-value/exchange read,
  generic-vector selector, type-class dictionary, `Ref`/header operation,
  lifetime delimiter, `Aliases` constructor, `reborrowings`, or `getContents`
  machinery; and
- the downstream 125-line/4,504-character worker ceiling applies only when
  using the same extraction and normalization command. Record worker arity,
  duplicate specializations, object text, simplifier ticks, and compile time;
  reject a second recursive specialization. Do not transfer a whole-object
  ceiling to a module containing several attribution controls.

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

- R2 attribution compares the all-unrestricted candidate with the same-build
  ordinary and fixed-unrestricted-only candidates. The downstream
  349--350 us and 197--199 us observations remain historical baselines, not
  gates for a new build.
- Do not gate the current candidate against the 56.4 us direct root: GHC
  eliminates that root's locally created `IORef` headers while every Pure
  Borrow candidate performs three real header reads. Use the retained
  `direct/header-matched` control; report `direct` only as a lower bound.
- Retain the frozen 125-line/4,504-character generic-worker gate. If the
  all-unrestricted worker misses it, record which checked operation,
  dictionary, coercion, or displaced-value path remains before proposing a
  new API.
- Initial engineering margin: upper confidence bound no more than 1.10 versus
  a header-matched direct control.
- Desired release target: 1.05 after a variance study shows that a
  small-single-digit gate is stable. P1 meets this locally at 1.019416 UCB;
  R3 and supported-GHC validation remain separate gates.
- Do not use wall-clock thresholds in ordinary shared CI. Structural Core and
  allocation-slope checks are the portable CI gates; paired timing belongs in
  a controlled performance job.

If Core passes but runtime fails, investigate code layout, worker size/arity,
enqueue/resume boundaries, strictness, and backend operations before changing
`BO`. If R2 passes but R3 fails, the difference is evidence about the wider
general worklist/control path. If local regressions pass but Herbrand does not,
reduce a new difference from the exact production snapshot before expanding
upstream APIs.

The stop/rollback rules are:

- if deterministic R2 semantics or lender recovery fails, stop before Core
  work and fix the safe ownership formulation;
- if R2 misses its Core gates, skip the paired campaign and fix
  specialization/inlining first; add neither a new bulk primitive nor a `BO`
  change;
- if Core passes but runtime fails, audit machine-code layout, arity,
  strictness, and comparator matching before changing the API;
- if R2 passes but R3 fails, attribute the next investigation to resumption,
  reopen, or wider control flow;
- if R2 and R3 pass but Herbrand still fails, reduce an exact downstream
  difference before expanding upstream;
- if whole-record and nested-`Muts` timing is inconclusive, retain
  whole-record reborrow/split as the simpler default; and
- a performance failure blocks performance promotion and downstream store
  migration, not retention of the generic family once its independent safety
  gate remains green.

### Repository validation for a later implementation

- Format Haskell with Fourmolu and package/project files with cabal-gild.
- Use HLS diagnostics during iteration.
- Run phase-focused tests, then `cabal build pure-borrow`,
  `cabal test pure-borrow-test`, and `cabal test pure-borrow-doctests` on the
  locally selected GHC 9.12.4.
- Run version-aware inspection in the 9.10.3/9.12.4/9.14.1 CI matrix.
- Keep controlled runtime campaigns separate from the ordinary matrix.

## Delivery order and decision gates

The prerequisite ledger is no longer the active queue. Fixed and growable
element-owning vectors, the generic unrestricted fixed vector, the generic
unrestricted growable vector, stable-header projection, the revised
`Aliases`/`Muts` API, the R1 direct copied-read path, and the direct R2 fixture
already exist. Herbrand has additionally validated the two intended
multi-owner transaction shapes. Preserve those results, but do not schedule
their design work again.

The active priority order is:

1. **P0a — unrestricted growable soundness gate (complete in signed commit
   `40b458a`).** The generic unrestricted growable ownership/typing matrix,
   trusted `getContents`/`withContent` proof review, and generic owner-reuse
   compile-failure fixture now pass the gates above. `4aabfae` still postdates
   the downstream `cdf6368` experiment and is not itself
   downstream-validated; this completion is the upstream proof/test gate, not
   a performance claim.
2. **P0b — upstream all-unrestricted R2 integration (complete in signed commit
   `8db6d4a`).** The ported validators and both validated ownership shapes match
   the complete 4,096-index/26,318-event trajectory and every digest. Each uses
   safe public ownership APIs plus public `unsafeGet`/`unsafeWrite` under the
   frozen in-bounds worker invariant; neither imports `BO.Unsafe` nor a
   container `.Internal` module. Every short fixed view is consumed and every
   independently constructed original owner is recovered and materialized
   exactly once. The shared recursive worker passes the frozen structural gate
   at 124 lines/4,192 characters, and the stable inspection anchors exclude
   dictionaries, growable headers, plural aliases, generic accessors, and
   projection calls from the hot worker.
3. **P1 — attributable R2 optimization (complete in the current worktree).**
   Same-build all-owning, fixed-only-unrestricted, and all-unrestricted
   controls reproduce the historical attribution. The header-matched direct
   control retains three real reads. A first 21-pair campaign failed at
   1.147362×/1.154867 UCB; allocation and Core isolated one boxed digest per
   visit. An explicit local `Int64#` loop removes that allocation without a new
   API or unsafe ownership operation. The final candidate measures
   120 Core lines/3,989 characters, 0.467 excess bytes per visit, and
   1.014222×/1.019416 UCB, passing both runtime margins. Direct and nested
   ownership shapes are timing-equivalent, so retain the simpler direct-record
   form.
4. **P2 — resumable R3a/R3b (active, reviewed baseline in place).** The
   deterministic worklist, open-once and reopen modes, flat/hierarchical
   shapes, frozen modelled counts, semantic tests, edge-worker inspections,
   manual whole-boundary Core, symmetric whole-root allocation, and selected
   O2 paired campaigns now exist. Retain the hierarchical shape: same-build
   dense timing and allocation both beat flat. The same-capacity batch sweep
   and same-batch capacity comparisons are complete: zero-growth and growing
   controls have effectively identical safe-minus-direct allocation at fixed
   resume counts, and both batch-8 runtime controls miss 1.05. Remaining P2
   work is the early-stop timing matrix, a node/visit allocation sweep with
   setup outside the kernel and per-repetition validation, automated retained
   Core extraction, exact-one-row paired-runner hardening, and supported-GHC
   structural checks.
5. **P4a — narrow plural-boundary attribution (ahead of P3, not yet an
   optimization).** R3a and the 20/67-resume R3b controls pass 1.05; both
   513-resume batch-8 controls pass 1.10 and miss 1.05. Orthogonal allocation
   and timing now rule out actual buffer growth as a prerequisite for the
   residual. Attribute `Aliases` reconstruction, result tuples, and
   `reborrowings`/`After` allocation at the two-member frontier boundary; do
   not reopen the data-access or growable-vector API. Attempt a safe general
   optimization only if that attribution survives. Preserve the current API
   and require the frozen vectors/digests, manual Core shape, and early-stop
   cases to remain unchanged.
6. **P3 — conditional surface changes (deferred).** R3 currently exposes no
   missing data access, growth, or content-view operation: the public
   unrestricted path expresses the entire workload safely. Add an unchecked
   or bulk transaction only if P4a proves that the residual cannot be removed
   at the general plural boundary. Keep every precondition explicit and test
   checked-entry equivalence. Genuinely linearly owned elements remain a
   separate workload.
7. **P4b — broad delimiter or `BO` representation work (still
   conditional).** Escalate beyond the plural boundary only if its measured
   residual survives the narrow P4a attempt across R2 and R3. R1 convenience
   cleanup may proceed independently but is not a blocker for the multi-store
   path.
8. **P5 — documentation, promotion, and downstream validation.** Document the
   validated safe transaction shape, run supported-GHC structural checks, add
   only measured batch operations, and deliberately promote the unrestricted
   growable family. Then rerun the corrected exact-checkpoint Herbrand
   campaign to decide whether any remaining issue belongs upstream, in GHC
   code generation, or in the application.

Each priority has its focused gate. Do not repeat the full downstream
provenance campaign before P0b--P2 establish which upstream candidate is being
validated.

### Adversarial review resolution — 2026-07-31

Three independent reviews challenged this reorder:

- The soundness review rejected combining proof completion with R2
  implementation. P0a is now a separate blocking gate, the new generic family
  is described as a candidate rather than downstream-validated, and its
  trusted header peek/lifetime shortening requires an explicit proof review.
  The review found an evidence gap, not a concrete exploit.
- The ownership review found the unrestricted growable normal-return paths
  consistent with GC-owned elements, last-published logical length, nominal
  roles, and initialized-prefix projection. It required the expanded negative
  typing matrix and quarantined the experimental multiplicity-polymorphic
  vector from this track because arbitrary generic backends lack a stated
  linear-element transport contract.
- The performance review moved the all-unrestricted R2 worker and its Core
  gates ahead of timing, R3 immediately after attributable R2, and all
  delimiter/`BO` work to conditional follow-up. It also rejected the current
  direct runtime root as a fair parity comparator until its three headers are
  matched.

No reviewer approved proceeding to P0b before P0a passed. The completed
typing matrix, trusted-code proof record, positive equivalence tests, and
generic owner-reuse compile failure now resolve those blockers. Their
P0b structural demands now also pass; their remaining performance findings
through P1 now pass as well. Their remaining findings are represented in
P2--P5 and the stop rules above.

The three independent P2 reviews add this resolution:

- Soundness found no lifetime, linearity, aliasing, or root-level unchecked
  bounds violation. The exported unchecked edge workers are now explicitly
  documented as benchmark-internal inspection anchors whose bounds are
  established only by the roots.
- Ownership found no unsafe import, overlapping owner, escaping content view,
  growth-with-live-view, double reclaim, or mutable result escape. Its
  freshness-coverage gap is closed by explicit seed sensitivity plus
  repeated/interleaved determinism assertions.
- Performance rejected the first allocation attribution and mixed-build
  timing prose. Direct teardown now materializes all seven roots symmetrically;
  flat and nested direct controls dispatch once into distinct recursive
  drivers; the selected timings share one executable/runner hash; and the plan
  distinguishes automated edge inspection, manual boundary Core, whole-root
  allocation, process high-water residency, and still-pending causal sweeps.
  It supports deferring P3 and investigating the plural boundary before broad
  `BO` work, but does not yet approve a boundary optimization.

## Explicit non-goals

- Replacing `BO` based only on the current feedback.
- Designing a new multi-owner combinator after both existing safe shapes have
  already recovered every lender and matched the complete downstream trace.
- Treating the 56.4 us direct `MultiStoreScan` root as a fair parity comparator
  while its local headers are optimized away.
- Prioritizing delimiter erasure ahead of the all-unrestricted growable-content
  integration without new whole-root evidence.
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
