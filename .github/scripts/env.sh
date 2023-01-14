#!/bin/sh

if [ "${RUNNER_OS}" = "Windows" ] ; then
	ext=".exe"
else
	ext=''
fi

export DEBIAN_FRONTEND=noninteractive
export TZ=Asia/Singapore

export OS="$RUNNER_OS"
export PATH="$HOME/.local/bin:$PATH"

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
