#!/bin/sh

set -eux

. .github/scripts/prereq.sh

if ! command -v ghcup && [ "${RUNNER_OS}" != "FreeBSD" ] ; then
	find "$GHCUP_INSTALL_BASE_PREFIX"
	mkdir -p "$GHCUP_BIN"
	mkdir -p "$GHCUP_BIN"/../cache
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
fi

if [ "${RUNNER_OS}" != "FreeBSD" ] ; then
	ghcup install ghc --set --isolate="$HOME/.local"     --force "$GHC_VER"
	ghcup install cabal     --isolate="$HOME/.local/bin" --force "$CABAL_VER"
	ghc --version
	cabal --version
	GHC="ghc-${GHC_VER}"
else
	GHC="ghc"
fi

git describe --all

ecabal() {
	cabal "$@"
}

# build
ecabal update


if [ "${RUNNER_OS}" = "Linux" ] ; then
	if [ "${ARCH}" = "32" ] ; then
		ecabal build -w "${GHC}" --ghc-options='-split-sections -optl-static' -ftui
	elif [ "${ARCH}" = "64" ] ; then
		ecabal build -w "${GHC}" --ghc-options='-split-sections -optl-static' -ftui
	else
		ecabal build -w "${GHC}" -ftui
	fi
elif [ "${RUNNER_OS}" = "FreeBSD" ] ; then
	ecabal build -w "${GHC}" --ghc-options='-split-sections' --constraint="zlib +bundled-c-zlib" --constraint="zip +disable-zstd" -ftui
elif [ "${RUNNER_OS}" = "Windows" ] ; then
	ecabal build -w "${GHC}" --constraint="zlib +bundled-c-zlib" --constraint="lzma +static"
else
	ecabal build -w "${GHC}" --constraint="zlib +bundled-c-zlib" --constraint="lzma +static" -ftui
fi

mkdir out
binary=$(ecabal new-exec -w "${GHC}" --verbose=0 --offline sh -- -c 'command -v ghcup')
ver=$("${binary}" --numeric-version)
if [ "${RUNNER_OS}" = "macOS" ] ; then
	strip "${binary}"
else
	if [ "${RUNNER_OS}" != "Windows" ] ; then
		strip -s "${binary}"
	fi
fi
cp "${binary}" "out/${ARTIFACT}-${ver}"
cp ./dist-newstyle/cache/plan.json "out/${ARTIFACT}.plan.json"

