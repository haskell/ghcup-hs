#!/bin/sh

set -eux

mkdir -p "$HOME"/.local/bin
export PATH="$HOME/.local/bin:$PATH"

if [ "${RUNNER_OS}" = "macOS" ] ; then
	if ! command -v brew ; then
		git clone --depth=1 https://github.com/Homebrew/brew "$HOME/.brew"
		export PATH="$HOME/.brew/bin:$HOME/.brew/sbin:$PATH"
		brew update
	fi
	brew install git
fi

if [ "${RUNNER_OS}" = "Linux" ] ; then
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
            perl \
            bash \
            git

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
fi

if ! command -v ghcup ; then
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
  source ~/.ghcup/env
fi


if [ "${RUNNER_OS}" != "FreeBSD" ] ; then
	ghcup install ghc --set --isolate="$HOME/.local" --force "$GHC_VER"
	ghcup install cabal --isolate="$HOME/.local/bin" --force "$CABAL_VER"
	ghc --version
	cabal --version
fi

ecabal() {
	cabal "$@"
}

git describe --all

# build
ecabal update


if [ "${RUNNER_OS}" = "Linux" ] ; then
	if [ "${ARCH}" = "32" ] ; then
		ecabal build -w "ghc-${GHC_VER}" --ghc-options='-split-sections -optl-static' -ftui
	elif [ "${ARCH}" = "64" ] ; then
		ecabal build -w "ghc-${GHC_VER}" --ghc-options='-split-sections -optl-static' -ftui
	else
		ecabal build -w "ghc-${GHC_VER}" -ftui
	fi
elif [ "${RUNNER_OS}" = "FreeBSD" ] ; then
	ecabal build -w "ghc-${GHC_VER}" --ghc-options='-split-sections' --constraint="zlib +bundled-c-zlib" --constraint="zip +disable-zstd" -ftui
elif [ "${RUNNER_OS}" = "Windows" ] ; then
	ecabal build -w "ghc-${GHC_VER}" --constraint="zlib +bundled-c-zlib" --constraint="lzma +static"
else
	ecabal build -w "ghc-${GHC_VER}" --constraint="zlib +bundled-c-zlib" --constraint="lzma +static" -ftui
fi

mkdir out
binary=$(ecabal new-exec -w "ghc-${GHC_VER}" --verbose=0 --offline sh -- -c 'command -v ghcup')
ver=$("${binary}" --numeric-version)
if [ "${RUNNER_OS}" = "macOS" ] ; then
	strip "${binary}"
else
	if [ "${RUNNER_OS}" != "Windows" ] ; then
		strip -s "${binary}"
	fi
fi
cp "${binary}" "out/${ARTIFACT}-${ver}"

