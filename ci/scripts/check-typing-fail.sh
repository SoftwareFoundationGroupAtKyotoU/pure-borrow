#!/usr/bin/env bash
# Compile every fixture under test/typing-fail/ against the built library and
# check that each one is rejected with the diagnostic it declares.
#
# A fixture declares its diagnostic in one or more `-- EXPECT: <text>` lines,
# and every such text must occur in the compiler's output.
# Keep the texts free of GHC's quotation marks, whose rendering depends on the
# locale.
#
# Run it after `cabal build all`, with the cabal invocation that built the
# library, so that the fixtures see that build and its compiler:
#
#   bash ci/scripts/check-typing-fail.sh
#   bash ci/scripts/check-typing-fail.sh cabal --project-file=ci/configs/ghc-9.12.4.project
set -euo pipefail

if (($# == 0)); then
  set -- cabal
fi

root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"

out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

failures=0
checked=0
while IFS= read -r -d '' fixture; do
  checked=$((checked + 1))
  expected=()
  while IFS= read -r line; do
    expected+=("$line")
  done < <(sed -n 's/^-- EXPECT: //p' "$fixture")
  if ((${#expected[@]} == 0)); then
    echo "FAIL $fixture: no '-- EXPECT:' line"
    failures=$((failures + 1))
    continue
  fi

  if log=$("$@" exec -- ghc -fno-code -outputdir "$out" "$fixture" 2>&1); then
    echo "FAIL $fixture: it typechecks, but must not"
    failures=$((failures + 1))
    continue
  fi
  # A fixture that cannot see the library fails to compile for the wrong reason.
  if grep -qE 'Could not (load|find) module' <<<"$log"; then
    echo "FAIL $fixture: the library is not visible; build it with the same cabal invocation first"
    echo "$log"
    failures=$((failures + 1))
    continue
  fi

  missing=()
  for text in "${expected[@]}"; do
    if ! grep -qF -- "$text" <<<"$log"; then
      missing+=("$text")
    fi
  done
  if ((${#missing[@]} == 0)); then
    echo "ok   $fixture"
  else
    echo "FAIL $fixture: rejected, but the output lacks:"
    printf '       %s\n' "${missing[@]}"
    echo "$log"
    failures=$((failures + 1))
  fi
done < <(find test/typing-fail -name '*.hs' -print0 | sort -z)

if ((checked == 0)); then
  echo "FAIL no fixtures found under test/typing-fail"
  exit 1
fi
echo "$checked fixtures, $failures failures"
((failures == 0))
