FROM alpine:edge

# ghc and cabal
RUN apk add --no-cache \
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
RUN apk add --no-cache \
	bash

## Package specific
RUN apk add --no-cache \
	zlib \
	zlib-dev \
	zlib-static \
	gmp \
	gmp-dev \
	openssl-dev \
	openssl-libs-static \
	xz \
	xz-dev

RUN cabal v2-update

COPY . /app

WORKDIR /app

RUN chmod +x /app/docker/build.sh

