#!/usr/bin/env bash
set -euo pipefail

# Run after cabal build all, using the project's configured compiler and package environment.
# Forkable dictionaries are erased, so deferred runtime errors cannot reliably test their absence.
world_root="$(CDPATH= cd -- "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$world_root"
world_cabal=(cabal "$@")
world_ghc="$("${world_cabal[@]}" path -v0 --output-format=json --compiler-info | python3 -c 'import json, sys; print(json.load(sys.stdin)["compiler"]["path"])')"
world_tmp="$(mktemp -d "${TMPDIR:-/tmp}/pure-borrow-world-types.XXXXXX")"
trap 'rm -rf "$world_tmp"' EXIT

world_compile() {
  "${world_cabal[@]}" exec -v0 -- "$world_ghc" \
    -fno-code -XGHC2021 -XLinearTypes -fdiagnostics-color=never \
    -package pure-borrow -outputdir "$world_tmp" "$1"
}

cat > "$world_tmp/WorldPositive.hs" <<'HASKELL'
{-# LANGUAGE DataKinds #-}
module WorldPositive where
import Control.Concurrent.DivideConquer.Linear qualified as DivideConquer
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.BO
import Data.Vector qualified as Vector
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as BorrowVector
import System.Random (mkStdGen)

pureParallel :: BO Static ((), ())
pureParallel = parBO (Control.pure ()) (Control.pure ())

ioParallel :: BIO Static ((), ())
ioParallel = parBO (Control.pure ()) (Control.pure ())

ioApplicative :: BIO Static ()
ioApplicative = runPar (Control.pure ())

ioScheduler :: Mut α (BorrowVector.Vector Vector.Vector Int) %1 -> BIO α (Mut α (BorrowVector.Vector Vector.Vector Int))
ioScheduler = DivideConquer.qsortDC (mkStdGen 42) 2 2

data ThreadBound

customSequential :: Mut α (BorrowVector.Vector Vector.Vector Int) %1 -> BO' ThreadBound α (Mut α (BorrowVector.Vector Vector.Vector Int))
customSequential = DivideConquer.sequentialDivideAndConquer (DivideConquer.qsortDC' 2)
HASKELL

if ! world_compile "$world_tmp/WorldPositive.hs" > "$world_tmp/positive.log" 2>&1; then
  cat "$world_tmp/positive.log"
  printf '%s\n' 'FAIL: positive world controls did not compile.' >&2
  exit 1
fi
printf '%s\n' 'PASS: Pure/BIO parallel controls and non-Forkable sequential control compile.'

world_reject() {
  local fixture="$1"
  local diagnostic="$2"
  local log="$world_tmp/$fixture.log"
  if world_compile "test/typing-fail/Worlds/$fixture.hs" > "$log" 2>&1; then
    printf 'FAIL: %s unexpectedly compiled.\n' "$fixture" >&2
    exit 1
  fi
  if ! grep -Fq "$diagnostic" "$log"; then
    cat "$log"
    printf 'FAIL: %s failed for an unrelated reason.\n' "$fixture" >&2
    exit 1
  fi
  printf 'PASS: %s was rejected for the expected reason.\n' "$fixture"
}

world_reject ParWithoutForkable 'Forkable ThreadBound'
world_reject ParApplicativeWithoutForkable 'Forkable ThreadBound'
world_reject SchedulerWithoutForkable 'Forkable ThreadBound'
world_reject ParForbiddenWorld 'ThreadBound actions must remain on their original thread'
