---
name: haskell-format
description: |
  Format Haskell source files: *.hs, *.lhs, and *.hsig, with the project's configured formatter: fourmolu, ormolu, or stylish-haskell. Use before compiling, after source edits, and when applying project style.
---

# Format Haskell Source

Format Haskell source files with the project's formatter. `.cabal` and `cabal.project*` files are handled by the `haskell-cabal-gild` skill. Always format before compiling.

## Choosing The Formatter

Walk up from the file's directory and choose by project config:

1. `fourmolu.yaml` or `.fourmolu.yaml` selects `fourmolu`.
2. `.stylish-haskell.yaml` selects `stylish-haskell`.
3. Otherwise use the first available command in this order: `fourmolu`, `ormolu`, `stylish-haskell`.

This project has `fourmolu.yaml`, so prefer `fourmolu` when available.

## Format In Place

All supported formatters accept `-i`:

```sh
fourmolu -i <file>
ormolu -i <file>
stylish-haskell -i <file>
```

## Project Automation

This repository has shared `PostToolUse` hooks under `.agents/hooks/`, wired from
Claude/Codex configuration. The Haskell formatting hook formats changed `.hs`, `.lhs`, and
`.hsig` files. Re-read files if the hook reports that it changed them.

## Companion

Format `.cabal` and `cabal.project*` files with the `haskell-cabal-gild` skill.
