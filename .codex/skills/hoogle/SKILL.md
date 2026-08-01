---
name: hoogle
description: |
  Search Haskell APIs using Hoogle. Use when working with Haskell projects to look up function signatures, find functions by type, discover library documentation, or identify which module exports an item.
---

# Hoogle API Search

Hoogle is a Haskell API search engine. Use it to find functions by name or by type signature.

## When To Use Hoogle

Use Hoogle when:

- Working with Haskell code and needing to find a function.
- Looking up a type signature.
- Searching for functions that match a type signature, such as `a -> b -> a`.
- Finding which module exports a particular function.
- Looking up documentation for Haskell functions.

## Search Methods

Search by function name:

```sh
.codex/skills/hoogle/scripts/hoogle-search.sh "map"
```

Search by type signature:

```sh
.codex/skills/hoogle/scripts/hoogle-search.sh "a -> b -> a"
```

Search with a package filter:

```sh
.codex/skills/hoogle/scripts/hoogle-search.sh "+base map"
.codex/skills/hoogle/scripts/hoogle-search.sh "+containers Data.Map.lookup"
```

Get detailed information for the first result:

```sh
.codex/skills/hoogle/scripts/hoogle-search.sh "foldl" --info
```

Remote search is available when local search is unavailable or insufficient:

```sh
.codex/skills/hoogle/scripts/hoogle-remote.sh "map"
```

## Result Shape

The search scripts return JSON:

```json
{
  "results": [
    {
      "url": "https://hackage.haskell.org/package/base/docs/Prelude.html#v:map",
      "module": { "name": "Prelude", "url": "..." },
      "package": { "name": "base", "url": "..." },
      "item": "map :: (a -> b) -> [a] -> [b]",
      "docs": "..."
    }
  ],
  "query": "map",
  "count": 10
}
```

Key fields are `item`, `docs`, `module.name`, and `package.name`.

## Database Initialization

Before local searching, ensure the Hoogle database exists:

```sh
.codex/skills/hoogle/scripts/hoogle-init-db.sh
```

For project-specific searches, generate a local database from Haddock docs:

```sh
.codex/skills/hoogle/scripts/hoogle-init-db.sh --local /path/to/haddock/docs
```

## Common Search Patterns

| Goal | Query Example |
| --- | --- |
| Find function by name | `filter` |
| Find by exact type | `(a -> Bool) -> [a] -> [a]` |
| Find in specific package | `+lens view` |
| Find class methods | `Monad m => m a -> m b` |
| Find by partial type | `Map k v -> k -> Maybe v` |

## Error Handling

If searches fail with database errors, regenerate the database:

```sh
.codex/skills/hoogle/scripts/hoogle-init-db.sh --force
```

If local search cannot work, use remote search or Hoogle's JSON endpoint:

```text
https://hoogle.haskell.org/?hoogle=<URL_ENCODED_QUERY>&mode=json
```
