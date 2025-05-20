#!/usr/bin/env bash
set -euo pipefail
DEST="$(pwd)/${1}"

echo "[*] Create directory tree"
TESTS="${DEST}"/tests
TESTS_LIST="${DEST}"/tests.list
EXES="${DEST}"/exes
EXES_LIST="${DEST}"/exes.list
BENCHS="${DEST}"/benchs
BENCHS_LIST="${DEST}"/benchs.list
set -x
mkdir -p "${TESTS}" "${BENCHS}" "${EXES}"
touch "${TESTS_LIST}"
touch "${EXES_LIST}"
touch "${BENCHS_LIST}"
set +x

echo "[*] Setting-up cabal-plan"
if command -v cabal-plan; then
    echo "cabal-plan found."
    CABAL_PLAN="$(command -v cabal-plan)"
else
    echo "No cabal-plan found."
    set -x
    case "$(uname)" in
        Darwin)
            CABAL_PLAN_URL=https://github.com/konn/cabal-plan/releases/download/v0.7.2.3/cabal-plan-0.7.2.3-macOS-x86_64.xz
        ;;
        *)
            CABAL_PLAN_URL=https://github.com/haskell-hvr/cabal-plan/releases/download/v0.6.2.0/cabal-plan-0.6.2.0-x86_64-linux.xz
        ;;
    esac
    
    wget "${CABAL_PLAN_URL}" -O cabal-plan.xz
    
    xz -d <./cabal-plan.xz >cabal-plan
    chmod +x cabal-plan
    set +x
    CABAL_PLAN="$(pwd)/cabal-plan"
fi

echo "[*] Places artifacts into the correct place"

local_pkgs=()
while read -r TARG; do
    local_pkgs+=("-e" "$(basename "${TARG%*.cabal}")");
done < <(find . -type f -not -path '*/dist-newstyle/*' -name '*.cabal')

${CABAL_PLAN} list-bins | grep "${local_pkgs[@]}" | while read -r TARG; do
    COMPONENT=$(echo "${TARG}" | awk '{ print $1 }')
    BIN=$(echo "${TARG}" | awk '{ print $2 }')
    TYPE=$(echo "${COMPONENT}" | cut -d':' -f2)
    echo "---"
    echo "- Comp: ${COMPONENT}"
    echo "- Path: ${BIN}"
    echo "- Type: ${TYPE}"
    case "${TYPE}" in
        exe)
            COPY_DEST=${EXES}
            LIST=${EXES_LIST}
        ;;
        test)
            COPY_DEST=${TESTS}
            LIST=${TESTS_LIST}
        ;;
        *)
            COPY_DEST=${BENCHS}
            LIST=${BENCHS_LIST}
        ;;
    esac
    if [ -f "${BIN}" ]; then
        echo "Copying to ${COPY_DEST}"
        cp "${BIN}" "${COPY_DEST}/"
        basename "${BIN}" >> "${LIST}"
    else
        echo "Binary not found. Skipped."
    fi
done
echo "[*] Collected. Compressing..."

TARBALL="${DEST}.tar.zst"
tar --use-compress-program="zstdmt -8" -caf "${TARBALL}" "./${1}"
echo "[*] Compressed as: ${TARBALL}"
