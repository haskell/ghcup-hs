#!/bin/sh

set -eux

mkdir -p "$HOME"/.local/bin
echo "$HOME/.local/bin" >> $GITHUB_PATH
export PATH="$HOME/.local/bin:$PATH"

ghcup install ghc --set --isolate="$HOME/.local" --force $GHC_VER
ghcup install cabal --isolate="$HOME/.local/bin" --force $CABAL_VER
ghc --version
cabal --version

ecabal() {
	cabal "$@"
}

git describe --all

# build
ecabal update


if [ "${RUNNER_OS}" = "Linux" ] ; then
	if [ "${ARCH}" = "32" ] ; then
		ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections -optl-static' -ftui
	elif [ "${ARCH}" = "64" ] ; then
		ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections -optl-static' -ftui
	else
		ecabal build -w ghc-${GHC_VERSION} -ftui
	fi
elif [ "${RUNNER_OS}" = "FREEBSD" ] ; then
	ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections' --constraint="zlib +bundled-c-zlib" --constraint="zip +disable-zstd" -ftui
elif [ "${RUNNER_OS}" = "Windows" ] ; then
	ecabal build -w ghc-${GHC_VERSION} --constraint="zlib +bundled-c-zlib" --constraint="lzma +static"
else
	ecabal build -w ghc-${GHC_VERSION} --constraint="zlib +bundled-c-zlib" --constraint="lzma +static" -ftui
fi

mkdir out
binary=$(ecabal new-exec -w ghc-${GHC_VERSION} --verbose=0 --offline sh -- -c 'command -v ghcup')
ver=$("${binary}" --numeric-version)
if [ "${RUNNER_OS}" = "macOS" ] ; then
	strip "${binary}"
else
	strip -s "${binary}"
fi
cp "${binary}" out/${ARTIFACT}-${ver}

