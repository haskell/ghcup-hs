#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}" "${CABAL_DIR}"

mkdir -p "$GHCUP_INSTALL_BASE_PREFIX/ghcup/bin"

CI_PROJECT_DIR=$(pwd)
curl -o ghcup.exe https://downloads.haskell.org/~ghcup/x86_64-mingw64-ghcup.exe
chmod +x ghcup.exe

./ghcup.exe install ${GHC_VERSION}
./ghcup.exe set ${GHC_VERSION}
./ghcup.exe install-cabal ${CABAL_VERSION}

rm ./ghcup.exe

exit 0
