# Pure Borrow: Linear Haskell Meets Rust-Style Borrowing

Pure Borrow achieves Rust-style borrowing in Linear Haskell with purity.

## Getting Started

### Requirements

We have tested the artifact in the following environments:

- macOS Tahoe on Apple Silicon (ARM); and
- Ubuntu 24.04 on both amd64 and arm64.

We also expect the artifact to run on the following environments:

- Recent macOS on Apple Silicon;
- Recent major Linux distro on amd64 or aarch64; and
- WSL on Recent Windows on amd64.

### Two Options to Run Our Artifact

There are two options to run our artifact:

- Option A: Use a container image; or
- Option B: Use GHC installed locally.

If you want to run our benchmark suite, Option A is usually the most convenient (provided you have a container runtime, such as Docker, Podman, or Apple Container).

In particular, we provide a Linux Image that can be run on amd64 and arm64. If you are using Linux on those architectures, you can just use this option without fear of the overhead.

You can also use Option B to try the artifact locally or to run the benchmark without overhead.

### Files

The main files in the Zenodo record are as follows:

- `README.md`: This README.
- `pure-borrow-docker-image-arm64/amd64.tar`: OCI images of our development, which can be used for Option A.
  + It was built using `dockerfiles/artifact/Dockerfile` in the source `pure-borrow-src.tar.gz`.
- `pure-borrow-src.tar.gz`: The complete source distribution of our Pure Borrow. It can be used for Option B.
- `pure-borrow-reference.tar.gz`: An HTML API Reference of Pure Borrow. It was generated with Haddock from the source above.

Recently, after the original submission, we discovered a subtle concurrency bug in the original implementation of our work-stealing API (not our core Pure Borrow API), introduced for the parallel quicksort case study (§4.1 in the original paper). We developed a new implementation to fix the bug. This change has already been communicated to the paper's reviewers. Please refer to the section "On the Bug in Our Original Work-Stealing API Implementation" for the details.

Regarding this new version, we include the following files in the Zenodo record:

- `pure-borrow-docker-image-fixed-arm64/amd64.tar`: OCI images, with the fixed implementation.
- `pure-borrow-src-fixed.tar.gz`: The complete source distribution of our Pure Borrow, with the fixed implementation.
- `new-benchmark-results.png`: The new benchmark results for the fixed implementation (a counterpart of Fig. 13, §4.2, of the original paper).

Each file with "-fixed" is a counterpart of the file without "-fixed" for the fixed implementation. Because this fix does not change the interfaces, the HTML API reference is shared by the fixed version.

### Option A: Using a Container Image

First, you need to install a Docker-compatible container runtime that can run Linux images. The description below assumes `docker` CLI, but you can use any Docker-compatible runtime as you like.

Depending on the host architecture, you can download one of the following from Zenodo:

- ARM64: `pure-borrow-docker-image-arm64.tar`;
- x86_64: `pure-borrow-docker-image-amd64.tar`.

After download, you can load the image as follows (replace `ARCH` with the appropriate one):

```bash
docker load -i pure-borrow-docker-image-ARCH.tar
```

After you import the image, you can run the following for a quick check, which should finish within a second:

```bash
$ docker run --rm -it pure-borrow demo
Sorting 256 elements with mode: Parallel 8
         XXX,XXX bytes allocated in the heap
...

  Productivity  XX.X% of total user, XX.X% of total elapsed
```

Default entrypoint of `pure-borrow` image is set to run `artifact-runner` executable, and the current working directory is set to `/workspace`.

In what follows, if you are asked to "run `artifact-runner` with cli arguments...", you can just call:

```bash
mkdir -p workspace
docker run --cpus 10 -v $(pwd):/workspace --rm -it pure-borrow ARG1 ARG2 ...
```

### Option B: Using GHC Installed Locally

If you have cabal >=3.12 and GHC ==9.10.3 installed locally, you should be able to compile our artifact.

We strongly recommend using GHCup (https://www.haskell.org/ghcup/) to set up a development environment for Haskell. Please refer to the official website for instructions on installing GHCup.

After you have installed GHCup, you can run the following commands to install the needed GHC & cabal:

```bash
ghcup install ghc 9.10.3
ghcup install cabal 3.16.1.0
```

Then, download `pure-borrow-0.1.0.0.tar.gz` from Zenodo, unpack it, and then build all targets.

```bash
tar xzvf pure-borrow-0.1.0.0.tar.gz
cd pure-borrow-0.1.0.0
cabal update
cabal build all
```

Then, you can run the short version of the benchmark for a quick check, which is expected to finish within ~5 minutes:

```bash
cabal run -- artifact-runner demo
Resolving dependencies...
Build profile: -w ghc-9.10.3 -O1
In order, the following will be built (use -v for more details):
...

Resolving dependencies...
Sorting 256 elements with mode: Parallel 8
         XXX,XXX bytes allocated in the heap
...

  Productivity  XX.X% of total user, XX.X% of total elapsed
```

You can run the executables of our artifact through `cabal run`.

In what follows, if you are asked to "run `artifact-runner` with cli arguments...", you can just call:

```bash
# Beware the standalone "--" to delimit the context!
cabal run -- artifact-runner ARG1 ARG2 ...
```

## Step-by-Step Instructions for Evaluation

To evaluate our artifact, you can:
- Reproduce the benchmark results; and
- Review the implementations.

### Reproducing the Benchmark Results

To reproduce the benchmark results (Fig. 13, §4.2) in our submission, you can just run `artifact-runner` without any CLI options.

The results will be saved as `qsort-raw.csv` and `qsort.csv`.

So, if you are using (Option A) Docker, you can just run:

```bash
docker run --cpus 10 -v $(pwd):/workspace --rm -it pure-borrow
```

Or, if you are (Option B) building the artifact locally, you can run:

```bash
cabal run -- artifact-runner
```

After everything is done, there should be two outputs, `qsort-raw.csv` and `qsort.csv`:

- The file `qsort-raw.csv` is the raw output CSV file emitted by the benchmarking framework.
- Then, `qsort.csv` is the final product generated from `qsort-raw.csv`, ready to be fed directly into the plotting library (pgfplots in our case).

Each column name consists of an algorithm name and a metric (e.g., `workSteal10Mean` consisting of `workSteal10` and `Mean`).

We have the following algorithm names:
- `intro` means "introsort from `vector-algorithms`";
- `sequential` means "sequential quicksort";
- `parallelN` means "naïve parallel quicksort with budget N"; and
- `workStealN` means "work-stealing quicksort with N workers".

We have the following metrics:
- `Mean` means "Mean CPU Time [ms]";
- `Stddev` means "Standard Deviation of CPU Time [ms]";
- `Alloc` means "Total Allocation [MB]";
- `Copied` means "Total Memory Copied during GC [MB]"; and
- `Peak` means "Peak Allocation [MB]".

Figure 13 (§4.2) in our submission plots "Mean CPU Time [ms]" and "Total Allocation [MB]".

#### Notes on benchmark parameters

You can also run the benchmarks with a smaller number of cases by `artifact-runner quick` (or `docker run ... pure-borrow quick`). It runs only five inputs and uses only four cores.

To control the number of cases, you can run `artifact-runner bench` with the following CLI options:

| Option | Description |
| :----: | :---------: |
| `-N NUM` or `--threads NUM` | Runs with `NUM` cores. Default: 10. |
| `-s NUM` or `--size NUM` | Takes benchmark against `NUM + 1` cases, each of size `32768 * i/NUM` for i = 0, .., NUM |

### Reviewing the Implementations

Our source distribution is published as `pure-borrow-src.tar.gz` in the Zenodo record. It contains a cabal project for our Pure Borrow library. You can review our implementations to verify the information provided in our submission.

We have the following folders:
- The folder `src` contains the library code for the core Pure Borrow API and the parallel quicksort case study (without benchmarking).
- The folders `internal-src`, `bench` and `app` contain code for benchmarking and a demo.
- The folder `test` contains tests.

The API reference for the library code, generated by Haddock, is available as `pure-borrow-reference.tar.gz` in the Zenodo record.

#### Core Pure Borrow API

The core Pure Borrow API is all located under the `src` directory. Here we list important modules:

##### Module `Control.Monad.Borrow.Pure`

This module exports the user-facing API items of Pure Borrow.

Its header (`{- $header ... -}`) contains code fragments corresponding to the code examples in Figs. 4-6.
The same code is also tested in `Data.Vector.Mutable.Linear.BorrowSpec` under `test` dir.

The module exports the following API items, as described in the original paper:

- Imported from `Control.Monad.Borrow.Pure.Internal`.
  + Fig. 7b: `BO α a`, `parBO`, `execBO`, `sexecBO`.
  + Fig. 7c: `Borrow bk α a` `Mut α a`, `Share α a`, `borrow`, `reclaim`, `share`.
  + Fig. 9: `joinMut`, `reborrow`.
  + Fig. 7e: `Copyable`, `copy`.
  + Fig. 7f: `splitPair`, `splitEither` (as well as general `split`).
- Derived borrowing combinators.
  + Fig. 7b: `runBO`, `srunBO`.
  + Fig. 9: `reborrowing`, `sharing`.
- Re-exports from `Control.Monad.Borrow.Pure.Lifetime`.
  + Fig. 7a: `Lifetime` type, subtype `<=` on lifetimes, `Static` lifetime, and `(/\)` on lifetimes.
- Re-exports from `Control.Monad.Borrow.Pure.Lifetime.Token`.
  + Fig. 7a: `Now α`, `End α`, `newLifetime'` (named "newLifetime" in the paper), `endLifetime`.

##### Module `Data.Vector.Mutable.Linear.Borrow`

This module exports the Pure Borrow-oriented API items for the mutable vector type `Vector a`, as described in §3. It is extensively used for our case study (§4).

It exports the following items, as described in the original paper:

- Fig. 7d: `get` (named "getAt" in the paper), `swap` ("swapAt"), `update` ("updateAt"), `modify` ("modifyAt").
- Fig. 7e: `copyAt`.
- Fig. 7f: `splitAt`.
- Fig. 9: `copyAtMut`.

##### Other modules

- `Control.Monad.Borrow.Pure.Ref` provides a Pure Borrow interface for the mutable reference type `Ref a`, which is a simplified version of `Vector a` used for our formal calculus (§5).
- `Data.Coerce.Directed` provides the subtyping relation `(<:)` (corresponds to ≼ in the paper) and `upcast` in Fig. 13.
- `Control.Syntax.DataFlow` provides a function that is needed to use `do`-notations with Linear Monads under [`QualifiedDo` extension][qdo]. The code is almost a verbatim copy from [Linear Constraints GHC proposal][lc-proposal].

[qdo]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html
[lc-proposal]: https://github.com/tweag/ghc-proposals/blob/linear-constraints/proposals/0621-linear-constraints.rst#do-notation

Note that `*.Internal` modules typically contain implementation details and unsafe internal definitions for the library. Library users are not expected to use these modules directly.

#### Case study

For our Parallel Quicksort case study (§4), we implemented the following:

- `qsort` in `Data.Vector.Mutable.Linear.Borrow`: The "naïve" parallel quicksort (Fig. 10, §4.1).
- `Control.Concurrent.DivideConquer.Linear` provides a general divide-and-conquer concurrency API based on Pure Borrow. It is used to implement the "work-stealing" version of quicksort.
  + Under the hood, the API carefully uses unsafe data types (such as OnceChan or Queue) internally, which can break the purity if misused. The API aims at safe encapsulation of them.
  + `qsortDC` implements the work-stealing quicksort algorithm using the general divide-and-conquer API.

#### Benchmarking

The code used to generate the Plot of Quicksort Benchmark (Fig. 13) is located under `internal-src/qsort-bench-suites/PureBorrow/Internal/Bench/QSort.hs`.
Then we apply `app/convert-qsort-bench-csv.hs` to generate the final CSV so that it can be fed into pgfplots.

We also developed the `artifact-runner` app (`app/artifact-runner.hs`) to automate the two-step procedure above and enable parameterized benchmarks, where we can customize parameters such as the number of cores or plotting points (unlike `convert-qsort-bench-csv`, which hard-codes such parameters).

#### Simple demo

For a simple demo, we implemented `app/qsort.hs`. It just sorts a random array of the specified length, and can also be called via `artifact-runner demo` (or `docker run pure-borrow demo`).

It accepts the following CLI arguments:

| Option | Description |
| :----: | :---------- |
| `-n NUM` | Sort the array of length `NUM` (default: 8) |
| `-s NUM` | Random seed. (default: random) |
| `-p N` / `-S` / `-w` | Sorting algorithm. `-p N` means Naïve parallel sort with budget `N`; `-S` means sequential quicksort; `-w` uses works-stealing |

It prints neither the original nor the sorted arrays, just evaluates the output array into normal form. Hence, for small input sizes, the command may appear to be doing nothing. But if you increase the size to a large number (e.g., `4096124`), you can observe the difference between sorting algorithms.

## On the Bug in Our Original Work-Stealing API Implementation

Recently, after the submission, we discovered a subtle concurrency bug in the original implementation of the Work-Stealing API. We developed a new version to fix this bug.

Notably, the fix only changes the implementation in the `Control.Concurrent.DivideConquer.Linear` module and its submodules. This is not the bug of the core Pure Borrow API.

### Cause of the Bug and Our Fix

The old code assumed an incorrect invariant about the timing of synchronization of the concurrent computation, resulting in a partially non-sorted array with a low probability, depending on thread scheduling.

The attached "fixed" version quickly addresses this issue by using a global atomic counter for a sanity check. We have tested the new implementation under various conditions to confirm that it works correctly.

However, the introduction of a global atomic counter reduced the performance significantly. For the new benchmark results, please see `new-benchmark-results.png` included in the Zenodo record. The good news is that the fixed implementation still runs faster than the sequential and naïve implementations when run with four worker threads.

We are working on the work-stealing API implementation to achieve better performance while avoiding bugs.

### On the Reviews

We don't think this diminishes the value of our contribution. The work-stealing API was meant to be a proof of concept demonstrating that our Pure Borrow serves as a useful basic building block for constructing more practical and complex APIs.

Indeed, no reviews highlighted the performance results in §4.2 as a strength of this submission. Also, Reviewer C aptly noted as follows:

> I don’t really care about the performance results for this work; especially not for just a single benchmark, but as a ”sanity check” they are leaning in the right direction.

Also, this change regarding the work-stealing API has already been communicated to the paper's reviewers on HotCRP.
