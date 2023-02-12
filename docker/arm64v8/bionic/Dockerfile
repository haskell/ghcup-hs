FROM arm64v8/ubuntu:bionic

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

ARG GHCUP_VERSION=0.1.18.0
ARG GPG_KEY=7784930957807690A66EBDBE3786C5262ECB4A3F

# install libtinfo.so.6 for ghcup
RUN cd /tmp && \
	curl -sSfL -O https://ftp.gnu.org/pub/gnu/ncurses/ncurses-6.1.tar.gz && \
	tar xf ncurses-6.1.tar.gz && \
	cd ncurses-6.1/ && \
	./configure --prefix=/root/.local \
		--with-shared \
		--with-versioned-syms \
		--without-ada \
		--with-termlib && \
	make -j && \
	make install && \
	cd && \
	rm -rf /tmp/ncurses-6.1

# install ghcup
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/aarch64-linux-ghcup-$GHCUP_VERSION && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/SHA256SUMS && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/SHA256SUMS.sig && \
    gpg --verify SHA256SUMS.sig SHA256SUMS && \
    sha256sum -c --ignore-missing SHA256SUMS && \
    mv aarch64-linux-ghcup-$GHCUP_VERSION /usr/bin/ghcup.bin && \
    chmod +x /usr/bin/ghcup.bin && \
	echo '#!/bin/sh'                               >  /usr/bin/ghcup && \
	echo ''                                        >> /usr/bin/ghcup && \
	echo 'export LD_LIBRARY_PATH=/root/.local/lib' >> /usr/bin/ghcup && \
	echo 'exec /usr/bin/ghcup.bin "$@"'            >> /usr/bin/ghcup && \
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
	find "/usr/lib/ghc-${GHC}/" \( -name "*_p.a" -o -name "*.p_hi" \) -type f -delete && \
	rm -r "/usr/share/doc/ghc-${GHC}" && \
	rm -rf /tmp/ghcup* && \
	ghcup gc -p -s -c -t

ENV PATH /root/.cabal/bin:/root/.ghcup/bin:/root/.local/bin:$PATH

CMD ["ghci"]
