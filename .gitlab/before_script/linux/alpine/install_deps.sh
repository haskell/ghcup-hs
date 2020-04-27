#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../../ghcup_env"

apk add --no-cache \
	curl \
	gcc \
	g++ \
	gmp-dev \
	ncurses-dev \
	libffi-dev \
	make \
	xz \
	tar \
	perl

ln -s libncurses.so /usr/lib/libtinfo.so
ln -s libncursesw.so.6 /usr/lib/libtinfow.so.6
if [ "${BIT}" = "32" ] ; then
	curl -sSfL https://downloads.haskell.org/~ghcup/0.1.4/i386-linux-ghcup-0.1.4 > ./ghcup-bin
else
	curl -sSfL https://downloads.haskell.org/~ghcup/0.1.4/x86_64-linux-ghcup-0.1.4 > ./ghcup-bin
fi
chmod +x ghcup-bin
./ghcup-bin upgrade
./ghcup-bin install ${GHC_VERSION}
# ./ghcup-bin install-cabal ${CABAL_VERSION}
# install cabal-3.2.0.0
if [ "${BIT}" = "32" ] ; then
	curl -sSfL -o cabal-install-3.2.0.0-i386-alpine-linux-musl.tar.xz 'https://hasufell.de/d/d3e215db133e4fcaa61e/files/?p=/cabal-install-3.2.0.0-i386-alpine-linux-musl.tar.xz&dl=1'
	tar xf cabal-install-3.2.0.0-i386-alpine-linux-musl.tar.xz
	cp cabal-install-3.2.0.0-i386-alpine-linux-musl "${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin/cabal"
else
	curl -sSfL -o cabal-install-3.2.0.0-x86_64-alpine-linux-musl.tar.xz 'https://hasufell.de/d/d3e215db133e4fcaa61e/files/?p=/cabal-install-3.2.0.0-x86_64-alpine-linux-musl.tar.xz&dl=1'
	tar xf cabal-install-3.2.0.0-x86_64-alpine-linux-musl.tar.xz
	cp cabal-install-3.2.0.0-x86_64-alpine-linux-musl "${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin/cabal"
fi
chmod +x "${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin/cabal"


# utils
apk add --no-cache \
	bash \
	git

## Package specific
apk add --no-cache \
	zlib \
	zlib-dev \
	zlib-static \
	gmp \
	gmp-dev \
	openssl-dev \
	openssl-libs-static \
	xz \
	xz-dev



