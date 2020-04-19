#!/bin/sh

set -eux

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
	perl \
	\
	cabal \
	ghc

# utils
apk add --no-cache \
	bash

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


. "$( cd "$(dirname "$0")" ; pwd -P )/../../../ghcup_env"

