---
name: haddock
description: |
  Read Haskell package documentation, Haddock HTML, Hackage pages, or dependency source for cabal nix-style projects. Prefer locally built/cached docs and source from the cabal store, dist-newstyle, plan.json, and repo-cache; fall back to Hackage only for packages outside the project dependency set or when local docs are unavailable.
---

# Haskell Haddock And Source Reader

Read documentation and source code for dependencies of a cabal nix-style project, preferring local files over the network. Exhaust local sources before fetching from Hackage.

## Which Source To Use

- Package is a dependency of the current project: read locally.
- Package is outside the project's dependency set: fall back to Hackage.
- Prefer the `hoogle` skill for symbol or type lookup before fetching full documentation pages.

## One-Time Setup

If local docs are missing, enable documentation and build once after dependency changes:

```sh
cabal configure --enable-documentation
cabal build all
```

## Resolving A Dependency

Run the bundled resolver from the project root:

```sh
.codex/skills/haddock/scripts/locate-dep.sh <package>
```

The resolver regenerates the build plan, reads cabal's store and repo-cache paths, looks up the package's exact `UnitId` in `dist-newstyle/cache/plan.json`, and prints JSON:

```json
{
  "store_path": "<cabal-store>/<UnitId>",
  "doc_dir": "<store_path>/share/doc/html",
  "source": "<repo-cache>/.../<pkg>-<ver>.tar.gz"
}
```

- `store_path` is the resolved unit directory in the cabal store.
- `doc_dir` is the Haddock HTML directory, or `null` if docs were not built.
- `source` is a Hackage/Stackage source tarball path, source-repository-package metadata, or `null`.

Do not construct store paths from package names. Cabal abbreviates and hashes store entries, so use the resolved `UnitId`.

## Reading Docs

When `doc_dir` is non-null, module pages sit directly inside it. A module page is its dotted name with `.` replaced by `-` plus `.html`, for example `System.Random.SplitMix` becomes `System-Random-SplitMix.html`. Use `index.html` or list `*.html` files if the module name is uncertain.

## Reading Source

For a string tarball path, stream a single file without unpacking everything:

```sh
tar -tzf <source>
tar -xzOf <source> <pkg>-<ver>/<path/to/File.hs>
```

For a source-repository-package object, use its `{ type, location, tag, subdir }` metadata. Cabal checks the repo out under `dist-newstyle/src/` in a directory named from the repo plus a content hash, not from the tag. Locate the source with a glob such as:

```text
dist-newstyle/src/*/<subdir>/**/*.hs
```

Drop `/<subdir>` when absent or `"."`. If multiple checkouts match, pick the one whose `git rev-parse HEAD` matches `git rev-parse "<tag>^{commit}"` inside that checkout.

## Resolver Misses

If the resolver prints `null` or exits non-zero, the package is not in the cabal store. Common cases:

- GHC boot libraries such as `base`, `ghc-prim`, `containers`, and `template-haskell` ship docs with the compiler. Use `ghc-pkg-<ver> field <pkg> haddock-html`, deriving `<ver>` and the matching `ghc-pkg` path from `cabal path --output-format=json`.
- Local packages should be read directly from their path in `cabal.project`.

## Hackage Fallback

Use Hackage only when local lookup is not available:

```text
https://hackage.haskell.org/package/<pkg>
https://hackage.haskell.org/package/<pkg>-<ver>/docs/<Module-With-Dashes>.html
```

If `doc_dir` is `null` for a real dependency, `documentation: True` is probably missing or `cabal build all` has not run since the dependency was added. Redo setup, then rerun the resolver.
