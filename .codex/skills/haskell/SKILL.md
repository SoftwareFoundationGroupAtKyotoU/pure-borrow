---
name: haskell
description: |
  Super-skill for developing Haskell projects with cabal nix-style builds. Orchestrates HLS diagnostics, Haddock, Hoogle, source formatting, and cabal-gild into one workflow. Use when writing, editing, refactoring, building, typechecking, fixing compiler errors, renaming symbols, looking up APIs, or changing dependencies in Haskell projects. Treat cabal.project as the source of truth, prefer HLS before full builds, and prefer local docs over remote lookup.
---

# Haskell Development Super-Skill

This is the entry point for Haskell development in this repository. It coordinates the
Codex skills with the agent-agnostic repository policy in `AGENTS.md`:

- Hoogle lookup maps to `.codex/skills/hoogle`.
- Haddock dependency documentation lookup maps to `.codex/skills/haddock`.
- Formatting maps to `.codex/skills/haskell-format`, `.codex/skills/haskell-cabal-gild`,
  and the shared hooks under `.agents/hooks/`.
- Prefer HLS when available before falling back to full Cabal builds.

## Ground Rules

- Use cabal nix-style builds. `cabal.project` and `cabal.project.local` are the source of truth.
- Do not use `stack` or invoke `ghc` directly for normal validation.
- In a multi-package project, work on and build one package at a time with `cabal build <pkg>`. Get it green before moving to another package.
- Format changed `.hs`, `.lhs`, and `.hsig` files with the `haskell-format` skill before compiling.
- Format changed `.cabal`, `cabal.project`, `cabal.project.local`, and `cabal.project.freeze` files with the `haskell-cabal-gild` skill before compiling.
- Prefer `(<>)` over `(++)` for all concatenation, including lists and strings.
- There is no `package.yaml`/hpack in this repository; edit `.cabal` files directly.

## Workflow

1. Inspect the relevant package and existing conventions before editing.
2. Edit the smallest reasonable surface and format changed files.
3. Use HLS diagnostics, hover/types, go-to-definition, references, or rename when available before running a full build.
4. Look up APIs as needed, preferring local information:
   - Use the `haddock` skill for local Haddock docs and dependency source.
   - Use the `hoogle` skill for name or type-signature search.
   - Use remote Hoogle or Hackage only when local search is insufficient.
5. Build with `cabal build <pkg>` once quick diagnostics are clean or unavailable.
6. Test with `cabal test <pkg>` or the specific test target relevant to the change.

## Dependency Changes

- Edit `.cabal` files or `cabal.project` according to the project layout.
- Reformat cabal/project files with `haskell-cabal-gild`.
- Run `cabal build <pkg>` for affected packages.
- Run `cabal build all` only when a dependency or build-plan change needs broad validation or refreshed local docs.

## Companion Tools

| Need | Use |
| --- | --- |
| Typecheck, types, definitions, references, rename | HLS when available |
| Format `.hs`, `.lhs`, `.hsig` sources | `haskell-format` |
| Format `.cabal` and `cabal.project*` files | `haskell-cabal-gild` |
| Read dependency docs or source | `haddock` |
| Find a function by name or type | `hoogle` |
