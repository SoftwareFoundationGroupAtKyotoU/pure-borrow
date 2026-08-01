#!/usr/bin/env bash
# hoogle-init-db.sh - check and initialize a Hoogle database.
set -euo pipefail

HOOGLE_DIR="${HOME}/.hoogle"
MIN_DB_SIZE=1000
LOCAL_PATH=""
FORCE=false

while [[ $# -gt 0 ]]; do
  case $1 in
    --local)
      LOCAL_PATH="$2"
      shift 2
      ;;
    --force)
      FORCE=true
      shift
      ;;
    -h | --help)
      echo "Usage: $0 [--local PATH] [--force]"
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      exit 1
      ;;
  esac
done

check_hoogle() {
  if ! command -v hoogle >/dev/null 2>&1; then
    echo '{"error": "hoogle_not_found", "message": "Hoogle is not installed or not in PATH"}' >&2
    exit 1
  fi
  echo "Found hoogle at: $(command -v hoogle)" >&2
}

find_database() {
  local version
  version=$(hoogle --numeric-version 2>/dev/null || echo "unknown")
  echo "${HOOGLE_DIR}/default-haskell-${version}.hoo"
}

check_database() {
  local db_file="$1"
  if [[ ! -f "${db_file}" ]]; then
    echo "Database file not found: ${db_file}" >&2
    return 1
  fi

  local size
  size=$(stat -c%s "${db_file}" 2>/dev/null || stat -f%z "${db_file}" 2>/dev/null || echo "0")
  if [[ "${size}" -lt "${MIN_DB_SIZE}" ]]; then
    echo "Database file is too small (${size} bytes), likely corrupt or incomplete" >&2
    return 1
  fi

  if ! hoogle search "" --count=1 >/dev/null 2>&1; then
    echo "Database validation failed - cannot perform search" >&2
    return 1
  fi

  echo "Database is valid: ${db_file} (${size} bytes)" >&2
  return 0
}

generate_database() {
  echo "Generating Hoogle database..." >&2
  mkdir -p "${HOOGLE_DIR}"

  if [[ -n "${LOCAL_PATH}" ]]; then
    echo "Generating from local path: ${LOCAL_PATH}" >&2
    hoogle generate --local="${LOCAL_PATH}"
  else
    echo "Generating from Stackage (this may take a while)..." >&2
    hoogle generate
  fi

  echo "Database generation complete" >&2
}

check_hoogle
db_file=$(find_database)

if [[ "${FORCE}" == "true" ]]; then
  echo "Force regeneration requested" >&2
  generate_database
elif ! check_database "${db_file}"; then
  echo "Database needs to be generated" >&2
  generate_database
else
  echo "Database is ready" >&2
fi

jq -cn --arg database "${db_file}" '{status: "ready", database: $database}'
