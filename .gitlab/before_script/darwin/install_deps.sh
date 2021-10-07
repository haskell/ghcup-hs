#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}"

if [ $ARCH = 'ARM64' ] ; then
	curl -sSfL https://downloads.haskell.org/~ghcup/aarch64-apple-darwin-ghcup > ./ghcup-bin
	chmod +x ghcup-bin
else
	curl -sSfL https://downloads.haskell.org/~ghcup/x86_64-apple-darwin-ghcup > ./ghcup-bin
	chmod +x ghcup-bin
	./ghcup-bin upgrade -i -f
fi

./ghcup-bin install ${GHC_VERSION}
./ghcup-bin set ${GHC_VERSION}
./ghcup-bin install-cabal ${CABAL_VERSION}

if [ $ARCH = 'ARM64' ] ; then
	cabal update
	mkdir vendored
	cd vendored
	curl -O https://downloads.haskell.org/~ghcup/tmp/network-3.1.2.3.tar.gz
	tar xf network-3.1.2.3.tar.gz
	rm network-3.1.2.3.tar.gz
	cd ..
fi

exit 0
