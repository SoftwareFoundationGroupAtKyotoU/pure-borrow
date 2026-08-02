# Revision history for pure-borrow

## Unreleased

- Added a Robin Hood hash table with backward-shift deletion.
  `Data.HashMap.RobinHood.Mutable.Linear` is the owned table, whose operations are ordinary linear functions; `Data.HashMap.RobinHood.Mutable.Linear.Borrow` keeps one behind a linear `Ref` so that it can be mutated, and grown, through a `Mut` borrow.
  Its keys and values are GC-owned, and it caches a fingerprint per slot so that a key with a cheap hash and an expensive equality is rejected without a full comparison.
  This adds a dependency on `hashable`.

## 0.0.0.0 -- 2026-05-05

This is the first release on Hackage :tada:
Please refer to our paper for details.
Besides the parts covered by the paper, we are providing the following experimental features:

- Bulk borrows by `Borrows` heterogeneous list.
- `Reborrowable` type class for abstraction over reborrowable borrow-like objects.
- Looping structure.
- Record splitting.
