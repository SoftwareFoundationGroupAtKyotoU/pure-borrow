#!/usr/bin/env bash
# locate-dep.sh - resolve local Haddock docs and source for a cabal dependency.
#
# Usage: locate-dep.sh <package-name>
# Run from the project root so cabal and dist-newstyle/cache/plan.json resolve.
set -euo pipefail

PACKAGE="${1:-}"
if [[ -z "${PACKAGE}" ]]; then
  echo "usage: locate-dep.sh <package-name>" >&2
  exit 2
fi

for tool in cabal jq; do
  command -v "${tool}" >/dev/null 2>&1 || {
    echo "error: '${tool}' not found on PATH" >&2
    exit 2
  }
done

cabal build -v0 --dry-run all

PATH_JSON=$(cabal path --output-format=json)
STORE_DIR=$(jq -crM '.compiler | ."store-path"' <<< "${PATH_JSON}")
REMOTE_REPO_CACHE=$(jq -crM '."remote-repo-cache"' <<< "${PATH_JSON}")

PLAN_JSON=dist-newstyle/cache/plan.json
QUERY=".\"install-plan\" | .[] | select(.\"pkg-name\" == \"${PACKAGE}\")"

COMPONENT=""
PLAN_COMPONENTS=$(jq -crM "${QUERY}" "${PLAN_JSON}")

while read -r CUR_COMPONENT; do
  ID=$(jq -rcM '.id' <<< "${CUR_COMPONENT}")
  CUR_PKG_STORE_PATH="${STORE_DIR}/${ID}"
  if [[ -d "${CUR_PKG_STORE_PATH}" ]]; then
    COMPONENT="${CUR_COMPONENT}"
    break
  fi
done < <(echo "${PLAN_COMPONENTS}")

if [[ -z "${COMPONENT}" ]]; then
  echo "null"
  exit 1
fi

PKG_STORE_PATH="${STORE_DIR}/${ID}"
PKG_SRC=$(jq -rcM '."pkg-src"' <<< "${COMPONENT}")
PKG_VERSION=$(jq -rcM '."pkg-version"' <<< "${COMPONENT}")
PKG_TYPE=$(jq -rcM '."type"' <<< "${PKG_SRC}")

SOURCE=""

case "${PKG_TYPE}" in
  "repo-tar")
    URI=$(jq -rc '."repo"."uri"' <<< "${PKG_SRC}")
    REPO_HOST=$(echo "${URI}" | awk -F/ '{print $3}')
    SOURCE_TARBALL="${REMOTE_REPO_CACHE}/${REPO_HOST}/${PACKAGE}/${PKG_VERSION}/${PACKAGE}-${PKG_VERSION}.tar.gz"
    cabal fetch -v0 "${PACKAGE}-${PKG_VERSION}"
    if [[ -f "${SOURCE_TARBALL}" ]]; then
      SOURCE=$(jq -rcM --raw-input '@json' <<< "${SOURCE_TARBALL}")
    else
      SOURCE="null"
    fi
    ;;
  "source-repo")
    SOURCE=$(jq -rcM '."source-repo"' <<< "${PKG_SRC}")
    ;;
  *)
    SOURCE="null"
    ;;
esac

PKG_DOC_DIR="${PKG_STORE_PATH}/share/doc/html"
if [[ -d "${PKG_DOC_DIR}" ]]; then
  PKG_DOC_DIR=$(jq -rcM --raw-input '@json' <<< "${PKG_DOC_DIR}")
else
  PKG_DOC_DIR="null"
fi

PKG_STORE_PATH_ESCAPED="$(jq -rcM --raw-input '@json' <<< "${PKG_STORE_PATH}")"

jq -nrcM "{store_path: ${PKG_STORE_PATH_ESCAPED}, doc_dir: ${PKG_DOC_DIR}, source: ${SOURCE}}"
