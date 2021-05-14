#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../../ghcup_env"

mkdir -p "${TMPDIR}"

apk add --no-cache \
	curl \
	gcc \
	g++ \
	binutils \
	binutils-gold \
	bsd-compat-headers \
	gmp-dev \
	ncurses-dev \
	libffi-dev \
	make \
	xz \
	tar \
	perl

if [ "${ARCH}" = "32" ] ; then
	curl -sSfL https://downloads.haskell.org/ghcup/i386-linux-ghcup > ./ghcup-bin
else
	curl -sSfL https://downloads.haskell.org/ghcup/x86_64-linux-ghcup > ./ghcup-bin
fi
chmod +x ghcup-bin
./ghcup-bin upgrade -i -f
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
	bzip2 \
	bzip2-dev \
	bzip2-static \
	gmp \
	gmp-dev \
	openssl-dev \
	openssl-libs-static \
	xz \
	xz-dev \
	ncurses-static

