#!/usr/bin/env bash
# hoogle-remote.sh - search remote Hoogle and return JSON results.
set -euo pipefail

COUNT=10
BASE_URL="https://hoogle.haskell.org"
QUERY=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --count)
      COUNT="$2"
      shift 2
      ;;
    --url)
      BASE_URL="$2"
      shift 2
      ;;
    -h | --help)
      echo "Usage: $0 QUERY [--count N] [--url URL]"
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

command -v curl >/dev/null 2>&1 || {
  echo '{"error": "curl_not_found", "message": "curl is not installed or not in PATH"}' >&2
  exit 1
}

command -v jq >/dev/null 2>&1 || {
  echo '{"error": "jq_not_found", "message": "jq is not installed or not in PATH"}' >&2
  exit 1
}

ENCODED_QUERY=$(printf '%s' "${QUERY}" | jq -sRr @uri)
URL="${BASE_URL}?mode=json&format=text&hoogle=${ENCODED_QUERY}&start=1&count=${COUNT}"

EXIT_CODE=0
RESPONSE=$(curl -sS -w "\n%{http_code}" "${URL}" 2>&1) || EXIT_CODE=$?
if [[ ${EXIT_CODE} -ne 0 ]]; then
  ESCAPED_OUTPUT=$(echo "${RESPONSE}" | sed 's/\\/\\\\/g; s/"/\\"/g; s/\t/\\t/g' | tr '\n' ' ')
  echo '{"error": "network_error", "message": "'"${ESCAPED_OUTPUT}"'"}' >&2
  exit 1
fi

HTTP_CODE=$(echo "${RESPONSE}" | tail -n1)
OUTPUT=$(echo "${RESPONSE}" | sed '$d')

if [[ "${HTTP_CODE}" != "200" ]]; then
  echo '{"error": "http_error", "message": "HTTP '"${HTTP_CODE}"' from '"${BASE_URL}"'"}' >&2
  exit 1
fi

if [[ -z "${OUTPUT}" ]] || [[ "${OUTPUT}" == "[]" ]]; then
  jq -cn --arg query "${QUERY}" --arg source "${BASE_URL}" '{results: [], query: $query, count: 0, source: $source}'
  exit 0
fi

if ! echo "${OUTPUT}" | jq . >/dev/null 2>&1; then
  ESCAPED_OUTPUT=$(echo "${OUTPUT}" | sed 's/\\/\\\\/g; s/"/\\"/g; s/\t/\\t/g' | tr '\n' ' ')
  echo '{"error": "invalid_json", "message": "'"${ESCAPED_OUTPUT}"'"}' >&2
  exit 1
fi

jq -cn --argjson results "${OUTPUT}" --arg query "${QUERY}" --arg source "${BASE_URL}" '{results: $results, query: $query, count: ($results | length), source: $source}'
