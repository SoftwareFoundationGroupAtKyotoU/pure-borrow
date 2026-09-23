# Further reading

Prefer this skill where these sources disagree with it: several predate linear `let` (GHC 9.10) and describe restrictions that no longer apply.

- GHC User's Guide, [Linear types](https://downloads.haskell.org/~ghc/9.12.4/docs/users_guide/exts/linear_types.html) (the 9.14 guide adds record-field multiplicities).
- The [linear-base](https://hackage.haskell.org/package/linear-base) and [linear-generics](https://hackage.haskell.org/package/linear-generics) Haddock.
- Bernardy, Boespflug, Newton, Peyton Jones, and Spiwack, *Linear Haskell: Practical Linearity in a Higher-Order Polymorphic Language*, POPL 2018 ([arXiv:1710.09756](https://arxiv.org/abs/1710.09756)).
- Spiwack, Kiss, Bernardy, Wu, and Eisenberg, *Linearly Qualified Types: Generic Inference for Capabilities and Uniqueness*, ICFP 2022 ([arXiv:2103.06127](https://arxiv.org/abs/2103.06127)): the linear constraints that `Linearly` tokens simulate.
- Tweag, [A tale of two functors](https://www.tweag.io/blog/2020-01-16-data-vs-control/) (data vs. control functors) and [Linear constraints: the problem with scopes](https://www.tweag.io/blog/2023-03-23-linear-constraints-linearly/) (why allocation takes a `Linearly` token).
- H. Ishii, [Experience report: Linear Haskell enables pure, parallel, and in-place FFT](https://discourse.haskell.org/t/experience-report-linear-haskell-enables-pure-parallel-and-in-place-fast-fourier-transformation/8256) (Haskell Discourse), on token-based APIs and on why pure destructive code needs `unsafePerformIO` rather than `unsafeDupablePerformIO`.
- H. Ishii, two articles in Japanese: [Linear Haskell in 2023](https://zenn.dev/konn/articles/2023-10-01-linear-haskell-in-2023), which introduces the `Linearly` token and its `besides` combinator, and [pure, parallel, in-place FFT](https://zenn.dev/konn/articles/2023-12-14-pure-parallel-fft-in-linear-haskell), on splitting arrays with tokens.
