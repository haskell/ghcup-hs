#!/bin/sh

mkdir -p "$HOME"/.local/bin

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
	elif [ "${DISTRO}" = "Ubuntu" ] ; then
		sudo apt-get update -y
		sudo apt-get install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl
	elif [ "${DISTRO}" = "Debian" ] ; then
		apt-get update -y
		apt-get install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc
	fi
elif [ "${RUNNER_OS}" = "macOS" ] ; then
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
fi

