#!/bin/sh

set -eux

# pkg install --force --yes --no-repo-update curl gcc gmp gmake ncurses perl5 libffi libiconv

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}"

curl -sSfL https://downloads.haskell.org/~ghcup/x86_64-portbld-freebsd-ghcup > ./ghcup-bin
chmod +x ghcup-bin

./ghcup-bin upgrade -i -f
./ghcup-bin -s file://$(pwd)/ghcup-${JSON_VERSION}.yaml install ${GHC_VERSION}
./ghcup-bin -s file://$(pwd)/ghcup-${JSON_VERSION}.yaml set ${GHC_VERSION}
./ghcup-bin -s file://$(pwd)/ghcup-${JSON_VERSION}.yaml install-cabal ${CABAL_VERSION}

exit 0
