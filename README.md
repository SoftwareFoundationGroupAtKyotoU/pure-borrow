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

#### Benchmarking Code

The code used to generate the Plot of Quicksort Benchmark (Fig. 13) is located under `internal-src/qsort-bench-suites/PureBorrow/Internal/Bench/QSort.hs`.
Because the raw output CSV of the benchmark is not suited for the use with our plotting library (pgfplot), we 

## Step-by-step instructions

### Run benchmarks

#### Option 1

#### Option 2

### Source Code Layout

The implementation is located under `src` directory.
This provides
