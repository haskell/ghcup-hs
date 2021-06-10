#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}"

if [ $ARCH = 'ARM64' ] ; then
	curl -L -O https://downloads.haskell.org/~ghc/8.10.5/ghc-8.10.5-aarch64-apple-darwin.tar.xz
	tar -xf ghc-*.tar.*
	cd ghc-*
	./configure --prefix="${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/ghc/8.10.5
	make install
	for i in "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/ghc/8.10.5/bin/*-8.10.5 ; do
		ln -s "${i}" "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin/${i##*/}
	done
	for x in "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin/*-8.10.5 ; do
		ln -s ${x##*/} ${x%-8.10.5}
	done
	unset x i
	cd ..
	rm -rf ghc-8.10.5 ghc-*.tar.*

	curl -L -O https://github.com/haskell/cabal/files/6617482/cabal-install-3.5-arm64-darwin-11.4-bootstrapped.tar.gz
	tar xf cabal-install-*
	mv cabal "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin/cabal
	rm -rf cabal-install
else
	curl -sSfL https://downloads.haskell.org/~ghcup/x86_64-apple-darwin-ghcup > ./ghcup-bin
	chmod +x ghcup-bin

	./ghcup-bin upgrade -i -f
	./ghcup-bin install ${GHC_VERSION}
	./ghcup-bin set ${GHC_VERSION}
	./ghcup-bin install-cabal ${CABAL_VERSION}
fi

exit 0
