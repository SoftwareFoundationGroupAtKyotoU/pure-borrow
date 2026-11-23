# Further reading

- The Haddock of `Control.Monad.Borrow.Pure` in the installed version of the package: a tutorial with runnable examples, and the reference for anything this skill leaves out.
- Y. Matsushita and H. Ishii, *Pure Borrow: Linear Haskell Meets Rust-Style Borrowing*, PLDI 2026 ([DOI 10.1145/3808259](https://doi.org/10.1145/3808259); extended version [arXiv:2604.15290](https://arxiv.org/abs/2604.15290)): the design, the lifetime algebra, and the soundness argument.
- Spiwack, Kiss, Bernardy, Wu, and Eisenberg, *Linearly Qualified Types: Generic Inference for Capabilities and Uniqueness*, ICFP 2022 ([arXiv:2103.06127](https://arxiv.org/abs/2103.06127)): the linear constraints (`Linearly %1 =>`) that the `Linearly` token simulates.
- Tweag, [Linear constraints: the problem with scopes](https://www.tweag.io/blog/2023-03-23-linear-constraints-linearly/): why allocation takes a `Linearly` token instead of a continuation.
- H. Ishii, [Linear Haskell in 2023](https://zenn.dev/konn/articles/2023-10-01-linear-haskell-in-2023) (Japanese), section on allocating several resources: the `Linearly` token, `linearly`, and `besides`.
- H. Ishii, [pure, parallel, in-place FFT](https://zenn.dev/konn/articles/2023-12-14-pure-parallel-fft-in-linear-haskell) (Japanese) and the English [experience report](https://discourse.haskell.org/t/experience-report-linear-haskell-enables-pure-parallel-and-in-place-fast-fourier-transformation/8256): token-based slicing and pure parallelism, the precursor of pure-borrow's `Mut`/`Lend` and `parBO`.
