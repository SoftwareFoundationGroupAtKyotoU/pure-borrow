# pure-borrow: Pure realization of Rust-style borrows in Linear Haskell

This is `pure-borrow`, a library that realizes Rust-style borrows in Linear
Haskell in a pure manner.
See the haddock or publication below for the more information.

## Supported GHC Versions

We support GHC 9.10.2+, but we recommend GHC 9.12.4+, due to subtle compiler bug in older GHC.

## Known Issues

Due to the bug of linear types in GHC, some program segfaults when evaluated in *interpreter* (see https://gitlab.haskell.org/ghc/ghc/-/issues/26565#note_645783).
Compiled programs just work as expected with GHC 9.10.2+, so this is issue will only affect you are trying to use GHCi or Eval Plugin of Haskell Language Server.

## Publication(s)

- Y. Matsushita and H. Ishii, *Pure Borrow: Linear Haskell Meets Rust-Style Borrowing*, 2026. 
  To appear in: PLDI 2026. Boulder, Colorado, USA, June 15-19. DOI: [10.1145/3808259](https://doi.org/10.1145/3808259). Extended Version: [arxiv:2604.15290](https://arxiv.org/abs/2604.15290).
- Y. Matsushita and H. Ishii, *Artifact for PLDI 2026 "Pure Borrow: Linear Haskell Meets Rust-Style Borrowing"*, 2026. Zenodo: https://zenodo.org/records/19622061.
