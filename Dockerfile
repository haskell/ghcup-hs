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
	libbz2 \
	bzip2-dev \
	bzip2-static \
	zlib \
	zlib-dev \
	zlib-static \
	gmp \
	gmp-dev \
	openssl-dev \
	openssl-libs-static \
	xz \
	xz-dev



COPY . /app

WORKDIR /app

RUN chmod +x /app/docker/build.sh

