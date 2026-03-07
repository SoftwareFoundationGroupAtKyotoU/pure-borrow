# Pure Borrow: Pure realization of Rust-style borrows in Linear Haskell

This is `pure-borrow`, a library that realizes Rust-style borrows in Linear
Haskell in a pure manner.

## Getting Started

The code is tested in the following environments:

- macOS Tahoe on Apple Silicon (ARM)
- Ubuntu 24.04 on both amd64 and arm64

Beside it, we expect the code to run on one of the following environments:

- Recent macOS on Apple Silicon
- Recent major Linux distro on amd64 or aarch64
- WSL on Recent Windows on amd64

There are two options to run the artifact:

1. Using Container Image
2. Locally installed GHC

If you want to run the benchmark suite used in the paper, option (1) is most handy (provided that you have a container runtime, e.g. Docker / Podman / Apple Container, etc).
In particular, we provide a Linux Image that can be ran on amd64 and arm64.
If you are using Linux on those arch, you can just use this option without worrying about the overheads.

You can also use the option (2) if you want to try it locally, or to run the benchmark without overhead.

## Option A: Using Container Image

First, you need to install container runtime.
The description below assumes `docker` CLI, but you can use any OCI-compatible runtime as you like.

TODO: Write it down

After you imported the image, you can run the following for a quick check, which should finish within a second:

```bash
$ docker run --rm -it pure-borrow demo
Sorting 256 elements with mode: Parallel 8
         XXX,XXX bytes allocated in the heap
...

  Productivity  XX.X% of total user, XX.X% of total elapsed
```

Default entrypoint of `pure-borrow` image is set to run `artifact-runner` executable and the current working directory is set to /workspace.
So, in what follows, if you are asked to "run `artifact-runner` with cli arguments...", you can just call:

```bash
mkdir -p workspace
docker run --cpus 10 -v $(pwd):/workspace --rm -it pure-borrow ARG1 ARG2 ...
```

## Option B: Using GHC installed locally

If you have cabal>=3.12 and GHC==9.10.3 installed, you can compile our code without modification.
If you haven't, we strongly recommend to use [GHCup][ghcup] to setup Haskell environment.

[ghcup]: https://www.haskell.org/ghcup/

Please refer the official page for the installation procedure of GHCup.
After you have installed GHCup, you can run the following commands to install needed GHC & cabal:

```bash
ghcup install ghc 9.10.3
ghcup install cabal 3.16.1.0
```

Then, download `pure-borrow-0.1.0.0.tar.gz` from Zenodo, unpack, then build all targets.

```bash
tar xzvf pure-borrow-0.1.0.0.tar.gz
cd pure-borrow-0.1.0.0
cabal update
cabal build all
```

Then, you can run the short version of the benchmark for a quick check, which should finish within ~5min:

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

You can run executables provided by our implementation throuh `cabal run`.
So, in what follows, if you are asked to "run `artifact-runner` with cli arguments...", you can just call:

```bash
# Beware standalone "--" to delimit the context!
cabal run -- artifact-runner ARG1 ARG2 ...
```

## Instruction for Benchmarking

To reproduce the benchmarking result (Fig. 13) in §4.2, you can just run `artifact-runner` without any CLI options.
The results will be saved as `qsort-raw.csv` and `qsort.csv`.

So, if you are using (1) Docker, you can just run:

```bash
docker run --cpus 10 -v $(pwd):/workspace --rm -it pure-borrow
```


Or, if you are (2) running locally, you can run:

```bash
cabal run -- artifact-runner
```

After everything is done, there should be two outputs: `qsort-raw.csv` and `qsort.csv`.
The file `qsort-raw.csv` is the raw output CSV file which is emitted by benchmarking framework.
Then, `qsort.csv` is the final product that was generated from `qsort-raw.csv` so that it can be directly fed into plotting library (pgfplots for our case).
We plot selected columns of form `*Mean`, `*Alloc` in Fig. 13.
Suffices

If you are running Docker on non-Linux environment, it can contain


## Overview of Artifacts

- `pure-borrow-container.tar`: OCI Image for Option (1).
  + Built from `dockerfiles/artifact/Dockerfile` in `pure-borrow-src.tar.gz`.
- `pure-borrow-src.tar.gz`: a complete source distribution, to be used with Option (2).
- `pure-borrow-reference.tar.gz`: An HTML API Reference of Pure Borrow.

### Structure of Source Distribution

Source Distribution contains a cabal project of Pure Borrow implementation.

#### Library Code

The library code is located under `src` directory.
The module `Control.Monad.Borrow.Pure` exports all the user-facing API.

API Reference is attached as `pure-borrow-reference.tar.gz` in Zenodo, so you can skim through it to check what kind of API is provided.

#### Benchmarking

The code used to generate the Plot of Quicksort Benchmark (Fig. 13) is located under `internal-src/qsort-bench-suites/PureBorrow/Internal/Bench/QSort.hs`.
Then we apply `app/convert-qsort-bench-csv.hs` is used to generate the final CSV so that it can be fed into pgfplots.

As these two-step procedure gets tedious and `convert-qsort-bench-csv` hard-codes the parameters (e.g. # of cores or plotting points), we devised `artifact-runner` app (`app/artifact-runner.hs`), which automates this process and is capable of treating parametrized benchmarks.

#

In Option (1), `docker run pure-borrow` 

### Run benchmarks

#### Option 1

#### Option 2

### Source Code Layout

The implementation is located under `src` directory.
This provides
