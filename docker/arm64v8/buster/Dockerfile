FROM arm64v8/debian:10

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
		llvm-11 clang-11 && \
    rm -rf /var/lib/apt/lists/*

RUN update_opt.sh 11 1

ARG GHCUP_VERSION=0.1.20.0
ARG GPG_KEY="7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C FFEB7CE81E16A36B3E2DED6F2DE04D4E97DB64AD 88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4 EAF2A9A722C0C96F2B431CA511AAD8CEDEE0CAEF"

# install ghcup
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/aarch64-linux-ghcup-$GHCUP_VERSION && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/SHA256SUMS && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/SHA256SUMS.sig && \
    gpg --verify SHA256SUMS.sig SHA256SUMS && \
    sha256sum -c --ignore-missing SHA256SUMS && \
    mv aarch64-linux-ghcup-$GHCUP_VERSION /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup && \
    rm -rf SHA256SUMS SHA256SUMS.sig

ARG GHC=9.4.8
ARG CABAL_INSTALL=3.10.2.0
ARG STACK=2.13.1

ENV GHCUP_CURL_OPTS="--silent"
ENV NO_COLOR=1

# install haskell toolchain
RUN ghcup config set gpg-setting GPGStrict && \
	ghcup --verbose install ghc   --isolate=/usr     --force ${GHC} && \
	ghcup --verbose install cabal --isolate=/usr/bin --force ${CABAL_INSTALL} && \
	ghcup gc -s -c -t

ENV PATH /root/.cabal/bin:/root/.ghcup/bin:/root/.local/bin:$PATH

CMD ["ghci"]
