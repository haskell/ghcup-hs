#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

ecabal() {
	cabal --store-dir="$(pwd)"/.store "$@"
}

git describe

# build
ecabal update

if [ "${OS}" = "LINUX" ] ; then
	if [ "${BIT}" = "32" ] ; then
		rm -r 3rdparty/libarchive
		ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections -optl-static'
	else
		ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections -optl-static' -ftui
	fi
elif [ "${OS}" = "FREEBSD" ] ; then
	ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections' --constraint="zlib static"
else
	ecabal build -w ghc-${GHC_VERSION} --constraint="zlib static" --constraint="lzma static" -ftui
fi

mkdir out
cp "$(ecabal new-exec -w ghc-${GHC_VERSION} --verbose=0 --offline sh -- -c 'command -v ghcup')" .
ver=$(./ghcup --numeric-version)
if [ "${OS}" = "DARWIN" ] ; then
	strip ./ghcup
else
	strip -s ./ghcup
fi
cp ghcup out/${ARTIFACT}-${ver}

