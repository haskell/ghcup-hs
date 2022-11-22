FROM i386/alpine:3.12

ENV LANG C.UTF-8

RUN apk add --no-cache \
            curl \
            gcc \
            g++ \
            binutils \
            binutils-gold \
			coreutils \
            bsd-compat-headers \
            gmp-dev \
            ncurses-dev \
            libffi-dev \
            make \
            xz \
            tar \
            perl \
            bash \
            diffutils \
            git \
			gzip \
			gnupg && \
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

ARG GHCUP_VERSION=0.1.18.0
ARG GPG_KEY=7784930957807690A66EBDBE3786C5262ECB4A3F

# install ghcup
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/i386-linux-ghcup-$GHCUP_VERSION && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/SHA256SUMS && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/SHA256SUMS.sig && \
    gpg --verify SHA256SUMS.sig SHA256SUMS && \
    sha256sum -c --ignore-missing SHA256SUMS && \
    mv i386-linux-ghcup-$GHCUP_VERSION /usr/bin/ghcup && \
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

ENV PATH /root/.cabal/bin:/root/.ghcup:/root/.local/bin:$PATH

