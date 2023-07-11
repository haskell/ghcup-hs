FROM arm32v7/ubuntu:focal

ENV LANG C.UTF-8
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Asia/Singapore

COPY update_opt.sh /usr/bin/update_opt.sh
RUN chmod +x /usr/bin/update_opt.sh

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        dirmngr \
        g++ \
        git \
        gnupg \
        libsqlite3-dev \
        libtinfo-dev \
        libgmp-dev \
        make \
        netbase \
        openssh-client \
        xz-utils \
        zlib1g-dev \
		libnuma-dev libgmp10 libssl-dev liblzma-dev libbz2-dev wget lsb-release software-properties-common apt-transport-https gcc autoconf automake build-essential gzip patchelf tree \
		llvm-9 clang-9 && \
    rm -rf /var/lib/apt/lists/*

RUN update_opt.sh 9 1

ARG GHCUP_VERSION=0.1.19.4
ARG GPG_KEY=7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C

# install ghcup
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/armv7-linux-ghcup-$GHCUP_VERSION && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/SHA256SUMS && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/SHA256SUMS.sig && \
    gpg --verify SHA256SUMS.sig SHA256SUMS && \
    sha256sum -c --ignore-missing SHA256SUMS && \
    mv armv7-linux-ghcup-$GHCUP_VERSION /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup && \
    rm -rf SHA256SUMS SHA256SUMS.sig

ARG GHC=8.10.7
ARG CABAL_INSTALL=3.6.2.0
ARG STACK=2.9.1

ENV GHCUP_CURL_OPTS="--silent"
ENV NO_COLOR=1

# install haskell toolchain
RUN ghcup config set gpg-setting GPGStrict && \
	ghcup --verbose install ghc   --isolate=/usr     --force ${GHC} && \
	ghcup --verbose install cabal --isolate=/usr/bin --force ${CABAL_INSTALL} && \
	ghcup gc -s -c -t

ENV PATH /root/.cabal/bin:/root/.ghcup/bin:/root/.local/bin:$PATH

CMD ["ghci"]
