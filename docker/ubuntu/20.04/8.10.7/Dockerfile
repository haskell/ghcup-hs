FROM ubuntu:20.04

# install deps needed by GHC
RUN export DEBIAN_FRONTEND=noninteractive && \
	apt-get update -y && \
	apt-get install -y --no-install-recommends libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl && \
	apt-get clean && \
	rm -rf /var/cache/apt/archives && \
	rm -rf /var/lib/apt/lists/*

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
