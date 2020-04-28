#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

ecabal() {
	cabal --store-dir="$(pwd)"/.store "$@"
}

# build
ecabal update

if [ "${OS}" = "LINUX" ] ; then
	ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections -optl-static'
elif [ "${OS}" = "FREEBSD" ] ; then
	ecabal build -w ghc-${GHC_VERSION} --ghc-options='-split-sections'
else
	ecabal build -w ghc-${GHC_VERSION}
fi

mkdir out
cp "$(ecabal new-exec -w ghc-${GHC_VERSION} --verbose=0 --offline sh -- -c 'command -v ghcup')" .
ver=$(./ghcup --numeric-version)
cp ghcup out/${ARTIFACT}-${ver}

