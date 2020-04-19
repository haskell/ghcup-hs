#!/bin/sh

set -eux

# pkg install --force --yes --no-repo-update curl gcc gmp gmake ncurses perl5 libffi libiconv

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

curl -sSfL https://downloads.haskell.org/~ghcup/x86_64-portbld-freebsd-ghcup > ./ghcup-bin
chmod +x ghcup-bin

mkdir -p "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin
# ./ghcup-bin install ${GHC_VERSION}
# ./ghcup-bin install-cabal ${CABAL_VERSION}
# ./ghcup-bin set ${GHC_VERSION}

# install cabal-3.2.0.0
curl -sSfL -o cabal-install-3.2.0.0-x86_64-portbld-freebsd.tar.xz 'https://hasufell.de/d/d3e215db133e4fcaa61e/files/?p=/cabal-install-3.2.0.0-x86_64-portbld-freebsd.tar.xz&dl=1'
tar xf cabal-install-3.2.0.0-x86_64-portbld-freebsd.tar.xz
cp cabal "${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin/cabal"
chmod +x "${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin/cabal"

exit 0
