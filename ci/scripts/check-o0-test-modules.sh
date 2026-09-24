#!/usr/bin/env bash
# Every module of pure-borrow-test compiled at -O0 must also pass -fno-ignore-interface-pragmas; see AGENTS.md.
# Without it, the first such module compiled hides the library's unfoldings from every -O2 module compiled after it in the same session.
set -euo pipefail
cd "$(dirname "$0")/../.."
status=0
while IFS= read -r file; do
  if ! grep -qE 'OPTIONS_GHC.*-fno-ignore-interface-pragmas' "$file"; then
    echo "$file: compiled at -O0 without -fno-ignore-interface-pragmas" >&2
    status=1
  fi
done < <(grep -rlE 'OPTIONS_GHC.*-O0' test --include='*.hs' || true)
if [ "$status" -eq 0 ]; then
  echo "every -O0 module of test/ passes -fno-ignore-interface-pragmas"
fi
exit "$status"
