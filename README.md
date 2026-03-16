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

The files uploaded to Zenodo are as follows:

- `README.md`: This README.
- `pure-borrow-docker-image-arm64/amd64.tar`: OCI images that can be used for Option A.
  + It was built using `dockerfiles/artifact/Dockerfile` in the source `pure-borrow-src.tar.gz`.
- `pure-borrow-src.tar.gz`: The complete source distribution of our Pure Borrow. It can be used for Option B.
- `pure-borrow-reference.tar.gz`: An HTML API Reference of Pure Borrow. It was generated with Haddock from the source above.

### Option A: Using a Container Image

First, you need to install a Docker-compatible container runtime that can run Linux images. The description below assumes `docker` CLI, but you can use any Docker-compatible runtime as you like.

Depending on the host architecture, you can download one of the following from Zenodo:

- ARM64: `pure-borrow-docker-image-arm64.tar`
- x86_64: `pure-borrow-docker-image-amd64.tar`

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

Our source distribution is published as `pure-borrow-src.tar.gz` in Zenodo. It contains a cabal project for our Pure Borrow library.

You can review our implementations to verify the information provided in our submission. Here we explain our implementations.

#### Library

The library code is located under the `src` directory. Its API reference, generated by Haddock, is available as `pure-borrow-reference.tar.gz` on Zenodo.

Here we list important modules:

- `Control.Monad.Borrow.Pure` exports the user-facing API functions of Pure Borrow, including the API described in §3.
- `Data.Ref.Linear` provides a Pure Borrow-based interface to mutable reference, as described in §3.
- `Data.Vector.Mutable.Linear.Borrow` provides a Pure Borrow-based interface to a mutable vector, as described in §3. This is extensively used in the Case Study section (§4).
- `Control.Syntax.DataFlow` provides a function that is needed to use `do`-notations with Linear Monads under [`QualifiedDo` extension][qdo]. The code is almost verbatim copy from [Linear Constraints GHC proposal][lc-proposal].
- The `*.Internal` modules contain implementation details and are for library implementors, exposing unsafe internal definitions.

[qdo]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html
[lc-proposal]: https://github.com/tweag/ghc-proposals/blob/linear-constraints/proposals/0621-linear-constraints.rst#do-notation

#### Case study

For our Parallel Quicksort case study (§4), we implemented the following:

- `qsort` in `Data.Vector.Mutable.Linear.Borrow`: A "naïve" parallel quicksort described in the first half of §4.1.
- `Control.Concurrent.DivideConquer.Linear` provides a general divide-and-conquer concurrency API based on Pure Borrow. It is used to implement the "work-stealing" version of quicksort.
  + Under the hood, the API uses unsafe datatypes (such as OnceChan or Queue), which can break the purity if misused. Nevertheless, our implementation uses those primitives carefully for safe encapsulation. Library users can safely compose the API functions to write pure parallel array programs.
  + `qsortDC` implements the work-stealing quicksort algorithm using the general divide-and-conquer API.

#### Benchmarking

The code used to generate the Plot of Quicksort Benchmark (Fig. 13) is located under `internal-src/qsort-bench-suites/PureBorrow/Internal/Bench/QSort.hs`.
Then we apply `app/convert-qsort-bench-csv.hs` to generate the final CSV so that it can be fed into pgfplots.

We also developed the `artifact-runner` app (`app/artifact-runner.hs`) to automate the two-step procedure above and enable parameterized benchmarks, where we can customize parameters such as the number of cores or plotting points (unlike `convert-qsort-bench-csv`, which hard-codes such parameters).

#### Simple demo

For a simple demo, we implemented `app/qsort.hs`. It just sorts a random array of the specified length, and can also be called via `artifact-runner demo` (or `docker run pure-borrow demo`).

It accepts the following CLI arguments:

| Option | Description |
| :----: | :---------: |
| `-n NUM` | Sort the array of length `NUM` (default: 8) |
| `-s NUM` | Random seed. (default: random) |
| `-p N` / `-S` / `-w` | Sorting algorithm. `-p N` means Naïve parallel sort with budget `N`; `-S` means sequential quicksort; `-w` uses works-stealing  |

It prints neither the original nor the sorted arrays, just evaluates the output array into normal form. Hence, for small input sizes, the command may appear to be doing nothing. But if you increase the size to a large number (e.g., `4096124`), you can observe the difference between sorting algorithms.
