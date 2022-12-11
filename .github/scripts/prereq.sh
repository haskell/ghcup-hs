#!/bin/sh

mkdir -p "$HOME"/.local/bin

export OS="$RUNNER_OS"
export PATH="$HOME/.local/bin:$PATH"
: "${APT_GET:=apt-get}"

if [ "${RUNNER_OS}" = "Windows" ] ; then
	# on windows use pwd to get unix style path
	CI_PROJECT_DIR="$(pwd)"
	export CI_PROJECT_DIR
    export GHCUP_INSTALL_BASE_PREFIX="/c"
    export GHCUP_BIN="$GHCUP_INSTALL_BASE_PREFIX/ghcup/bin"
    export PATH="$GHCUP_BIN:$PATH"
	export CABAL_DIR="C:\\Users\\runneradmin\\AppData\\Roaming\\cabal"
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
		:
	elif [ "${DISTRO}" = "Ubuntu" ] ; then
		export DEBIAN_FRONTEND=noninteractive
		export TZ=Asia/Singapore
		if [ "${ARCH}" = "ARM64" ] || [ "${ARCH}" = "ARM" ] ; then
			:
		else
			${APT_GET} install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl gzip
		fi
	elif [ "${DISTRO}" = "Debian" ] ; then
		export DEBIAN_FRONTEND=noninteractive
		export TZ=Asia/Singapore
		${APT_GET} install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc gzip
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

    if [ "${ARCH}" = "ARM64" ] ; then
		brew install llvm@11 autoconf automake
		export PATH="$HOME/.brew/opt/llvm@11/bin:$PATH"
		export CC="$HOME/.brew/opt/llvm@11/bin/clang"
		export CXX="$HOME/.brew/opt/llvm@11/bin/clang++"
		export LD=ld
		export AR="$HOME/.brew/opt/llvm@11/bin/llvm-ar"
		export RANLIB="$HOME/.brew/opt/llvm@11/bin/llvm-ranlib"
	fi
fi

