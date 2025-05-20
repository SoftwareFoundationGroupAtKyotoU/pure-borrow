# Development Guide

## Suggested Usage

Recommendation:

- Use `ghcup` to maintain Haskell tool chain.
- Haskell Language Server is handy for your development.
- Use the recent enough cabal-install (>= 3.12, or >= 3.14.2.0 if possible)

## Coding Style

- Code Formatter: use [Fourmolu](https://hackage.haskell.org/package/fourmolu) as a formatter.
  If you are using HLS, it comes with built-in fourmolu formatter so you can just configure haskellFormattingProvider
  config and no need to install the exes.
- Use `cabal-gild` >= 1.6 to format cabal files correctly.
  + You need to install `cabal-gild` locally, as HLS currently invokes external cabal-gild when formatting Cabal files.
  + Cabal gild provides `discover` pragma to automatically generate module list (it also supports --exclude and/or --include globs)
  + The module lists are updated whenever the file is formatted. It is recommended to run cabal-gild whenever you add/remove modules.

## CI

- Add freeze file downloaded from Stackage to `ci/configs`.
  + Run e.g.: `curl -s -L https://www.stackage.org/lts/cabal.config -o ci-configs/ghc-9.8.4.project` and add `import: ../../cabal.project` on top of it.
- CI script automatically enumerates the config files and generates matrix CI.
