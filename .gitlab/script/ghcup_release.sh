#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

ecabal() {
	cabal "$@"
}

git describe

# build
ecabal update


if [ "${OS}" = "LINUX" ] ; then
	if [ "${ARCH}" = "32" ] ; then
		ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections -optl-static' -ftui -ftar
	elif [ "${ARCH}" = "64" ] ; then
		ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections -optl-static' -ftui
	else
		ecabal build -w ghc-${GHC_VERSION} -ftui
	fi
elif [ "${OS}" = "FREEBSD" ] ; then
	ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections' --constraint="zlib +bundled-c-zlib" --constraint="zip +disable-zstd" -ftui
elif [ "${OS}" = "WINDOWS" ] ; then
	ecabal build -w ghc-${GHC_VERSION} --constraint="zlib +bundled-c-zlib" --constraint="lzma +static"
else
	ecabal build -w ghc-${GHC_VERSION} --constraint="zlib +bundled-c-zlib" --constraint="lzma +static" -ftui
fi

mkdir out
binary=$(ecabal new-exec -w ghc-${GHC_VERSION} --verbose=0 --offline sh -- -c 'command -v ghcup')
ver=$("${binary}" --numeric-version)
if [ "${OS}" = "DARWIN" ] ; then
	strip "${binary}"
else
	strip -s "${binary}"
fi
cp "${binary}" out/${ARTIFACT}-${ver}

