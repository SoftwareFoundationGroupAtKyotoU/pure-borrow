#!/usr/bin/env bash
# Shared PostToolUse hook: format Haskell source files touched by agent edits.
set -u

script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
input="$(cat)"
command -v jq >/dev/null 2>&1 || exit 0

files="$(printf '%s' "$input" | "$script_dir/collect-touched-files.sh")"
[ -n "$files" ] || exit 0

fmt=""
pick_formatter() {
  local file="$1"
  local dir
  dir="$(cd "$(dirname "$file")" 2>/dev/null && pwd)" || return 1

  while :; do
    if { [ -f "$dir/fourmolu.yaml" ] || [ -f "$dir/.fourmolu.yaml" ]; } && command -v fourmolu >/dev/null 2>&1; then
      fmt="fourmolu"; return 0
    fi
    if [ -f "$dir/.stylish-haskell.yaml" ] && command -v stylish-haskell >/dev/null 2>&1; then
      fmt="stylish-haskell"; return 0
    fi
    [ "$dir" = "/" ] && break
    dir="$(dirname "$dir")"
  done

  for f in fourmolu ormolu stylish-haskell; do
    command -v "$f" >/dev/null 2>&1 && { fmt="$f"; return 0; }
  done
  return 1
}

changed=""
while IFS= read -r file; do
  case "$(basename "$file")" in
    *.hs | *.lhs | *.hsig ) ;;
    * ) continue ;;
  esac
  [ -f "$file" ] || continue
  fmt=""
  pick_formatter "$file" || continue

  before="$(shasum "$file" 2>/dev/null | awk '{print $1}')"
  "$fmt" -i "$file" >/dev/null 2>&1 || true
  after="$(shasum "$file" 2>/dev/null | awk '{print $1}')"
  [ "$before" != "$after" ] && changed="${changed}${changed:+, }$file"
done <<EOF
$files
EOF

if [ -n "$changed" ]; then
  jq -cn --arg changed "$changed" '{
    hookSpecificOutput: {
      hookEventName: "PostToolUse",
      additionalContext: ("Formatted Haskell source on disk: " + $changed + ". Re-read changed files before further edits.")
    }
  }'
fi

exit 0
