FROM alpine:3.14.2

# install deps needed by GHC
RUN apk add --no-cache \
	curl \
	gcc \
	g++ \
	binutils \
	binutils-gold \
	gmp \
	ncurses \
	libffi \
	make \
	xz \
	tar \
	perl

ARG GHCUP_VERSION

# install ghcup
RUN if [ -n "$GHCUP_VERSION" ] ; \
	then curl -sSfL https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/x86_64-linux-ghcup-$GHCUP_VERSION > /usr/bin/ghcup ; \
	else curl -sSfL https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup ; \
	fi && \
    chmod +x /usr/bin/ghcup

ENV GHCUP_INSTALL_BASE_PREFIX=/usr/local
ENV PATH=/usr/local/.ghcup/bin:$PATH

RUN ghcup install ghc --set 8.10.7 && \
    ghcup install cabal latest && \
    ghcup install stack latest
