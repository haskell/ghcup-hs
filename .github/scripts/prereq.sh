#!/bin/sh

if [ "${RUNNER_OS}" = "macOS" ] ; then
	mkdir -p "$HOME"/.local/bin
	ls -lah "$HOME"/.local/bin
	find "$HOME"/.local
	rm -rf "$HOME"/.local/bin
	rm -rf "$HOME"/.local/share
	rm -rf "$HOME"/.local/lib
	mkdir -p "$HOME"/.local/bin
else
	mkdir -p "$HOME"/.local/bin
fi

export OS="$RUNNER_OS"
export PATH="$HOME/.local/bin:$PATH"

if [ "${RUNNER_OS}" = "Windows" ] ; then
	# on windows use pwd to get unix style path
	CI_PROJECT_DIR="$(pwd)"
	export CI_PROJECT_DIR
    export GHCUP_INSTALL_BASE_PREFIX="/c"
    export GHCUP_BIN="$GHCUP_INSTALL_BASE_PREFIX/ghcup/bin"
    export PATH="$GHCUP_BIN:$PATH"
else
	export CI_PROJECT_DIR="${GITHUB_WORKSPACE}"
    export GHCUP_INSTALL_BASE_PREFIX="$CI_PROJECT_DIR"
    export GHCUP_BIN="$GHCUP_INSTALL_BASE_PREFIX/.ghcup/bin"
    export PATH="$GHCUP_BIN:$PATH"
    export CABAL_DIR="$CI_PROJECT_DIR/cabal"
    export CABAL_CACHE="$CI_PROJECT_DIR/cabal-cache"
fi

if [ "${RUNNER_OS}" = "Linux" ] ; then
	if [ "${DISTRO}" = "Alpine" ] ; then
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
            diffutils \
            git \
			gzip

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
	elif [ "${DISTRO}" = "Ubuntu" ] ; then
		sudo apt-get update -y
		sudo apt-get install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl gzip
		if [ "${ARCH}" = "64" ] ; then
			:
		fi
	elif [ "${DISTRO}" = "Debian" ] ; then
		apt-get update -y
		apt-get install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc gzip
		if [ "${ARCH}" = "64" ] ; then
			:
		fi
	fi
elif [ "${RUNNER_OS}" = "macOS" ] ; then
	rm -rf "$HOME/.brew"
	if ! command -v brew ; then
		[ -e "$HOME/.brew" ] ||
			git clone --depth=1 https://github.com/Homebrew/brew "$HOME/.brew"
		export PATH="$HOME/.brew/bin:$HOME/.brew/sbin:$PATH"
		brew update
	fi
	if ! command -v git ; then
		brew install git
	fi
	if ! command -v realpath ; then
		brew install coreutils
	fi

    if [ "${ARCH}" = "ARM64" ] ; then
		brew install llvm@11 autoconf automake
		export PATH="$HOME/.brew/opt/llvm@11/bin:$PATH"
		export CC=$HOME/.brew/opt/llvm@11/bin/clang
		export CXX=$HOME/.brew/opt/llvm@11/bin/clang++
		export LD=ld
		export AR=$HOME/.brew/opt/llvm@11/bin/llvm-ar
		export RANLIB=$HOME/.brew/opt/llvm@11/bin/llvm-ranlib
    elif [ "${ARCH}" = "64" ] ; then
		:
	fi
elif [ "${RUNNER_OS}" = "Windows" ] ; then
    if [ "${ARCH}" = "64" ] ; then
		:
	fi
fi

