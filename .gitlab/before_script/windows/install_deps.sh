#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}" "${CABAL_DIR}"

rm -rf /c/ghcup
mkdir -p /c/ghcup

CI_PROJECT_DIR=$(pwd)
curl -o ghcup.exe https://downloads.haskell.org/~ghcup/tmp/x86_64-mingw64-ghcup-5.exe
chmod +x ghcup.exe

./ghcup.exe install ${GHC_VERSION}
./ghcup.exe set ${GHC_VERSION}
./ghcup.exe install-cabal ${CABAL_VERSION}

rm ./ghcup.exe

exit 0
