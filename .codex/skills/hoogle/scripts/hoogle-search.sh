#!/usr/bin/env bash
# hoogle-search.sh - search local Hoogle and return JSON results.
set -euo pipefail

COUNT=10
INFO=false
QUERY=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --count)
      COUNT="$2"
      shift 2
      ;;
    --info)
      INFO=true
      shift
      ;;
    -h | --help)
      echo "Usage: $0 QUERY [--count N] [--info]"
      exit 0
      ;;
    -*)
      echo "Unknown option: $1" >&2
      exit 1
      ;;
    *)
      if [[ -z "${QUERY}" ]]; then
        QUERY="$1"
      else
        QUERY="${QUERY} $1"
      fi
      shift
      ;;
  esac
done

if [[ -z "${QUERY}" ]]; then
  echo '{"error": "missing_query", "message": "No search query provided"}' >&2
  exit 1
fi

command -v hoogle >/dev/null 2>&1 || {
  echo '{"error": "hoogle_not_found", "message": "Hoogle is not installed or not in PATH"}' >&2
  exit 1
}

command -v jq >/dev/null 2>&1 || {
  echo '{"error": "jq_not_found", "message": "jq is not installed or not in PATH"}' >&2
  exit 1
}

HOOGLE_ARGS=(search "${QUERY}" --json --count "${COUNT}")
if [[ "${INFO}" == "true" ]]; then
  HOOGLE_ARGS+=(--info)
fi

OUTPUT=""
EXIT_CODE=0
if OUTPUT=$(hoogle "${HOOGLE_ARGS[@]}" 2>&1); then
  EXIT_CODE=0
else
  EXIT_CODE=$?
fi

if [[ ${EXIT_CODE} -ne 0 ]]; then
  if echo "${OUTPUT}" | grep -q "corrupt"; then
    echo '{"error": "database_corrupt", "message": "Hoogle database is corrupt. Run hoogle-init-db.sh to regenerate."}' >&2
  elif echo "${OUTPUT}" | grep -q "does not exist"; then
    echo '{"error": "database_missing", "message": "Hoogle database not found. Run hoogle-init-db.sh to generate."}' >&2
  else
    ESCAPED_OUTPUT=$(echo "${OUTPUT}" | sed 's/\\/\\\\/g; s/"/\\"/g; s/\t/\\t/g' | tr '\n' ' ')
    echo '{"error": "hoogle_error", "message": "'"${ESCAPED_OUTPUT}"'"}' >&2
  fi
  exit 1
fi

if [[ -z "${OUTPUT}" ]] || [[ "${OUTPUT}" == "[]" ]] || [[ "${OUTPUT}" == "No results found" ]]; then
  jq -cn --arg query "${QUERY}" '{results: [], query: $query, count: 0}'
  exit 0
fi

if ! echo "${OUTPUT}" | jq . >/dev/null 2>&1; then
  ESCAPED_OUTPUT=$(echo "${OUTPUT}" | sed 's/\\/\\\\/g; s/"/\\"/g; s/\t/\\t/g' | tr '\n' ' ')
  echo '{"error": "invalid_output", "message": "'"${ESCAPED_OUTPUT}"'"}' >&2
  exit 1
fi

jq -cn --argjson results "${OUTPUT}" --arg query "${QUERY}" '{results: $results, query: $query, count: ($results | length)}'
