#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../../ghcup_env"

mkdir -p "${TMPDIR}"

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

ln -sf libncurses.so /usr/lib/libtinfo.so
ln -sf libncursesw.so.6 /usr/lib/libtinfow.so.6
ln -sf libtinfow.so.6 /usr/lib/libtinfow.so

if [ "${BIT}" = "32" ] ; then
	curl -sSfL https://downloads.haskell.org/ghcup/i386-linux-ghcup > ./ghcup-bin
else
	curl -sSfL https://downloads.haskell.org/ghcup/x86_64-linux-ghcup > ./ghcup-bin
fi
chmod +x ghcup-bin
./ghcup-bin install ${GHC_VERSION}
./ghcup-bin install-cabal ${CABAL_VERSION}

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
	xz-dev \
	ncurses-static

ln -sf libncursesw.a /usr/lib/libtinfow.a

