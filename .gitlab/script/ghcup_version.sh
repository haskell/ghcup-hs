#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

ecabal() {
	cabal --store-dir="$(pwd)"/.store "$@"
}


# build
ecabal update

if [ "${OS}" = "DARWIN" ] ; then
	ecabal build -fcurl
else
	ecabal build
fi

cp "$(ecabal new-exec --enable-tests --verbose=0 --offline sh -- -c 'command -v ghcup')" .
cp "$(ecabal new-exec --enable-tests --verbose=0 --offline sh -- -c 'command -v ghcup-gen')" .


# testing

cp ./ghcup "$CI_PROJECT_DIR"/.local/bin/ghcup
cp ./ghcup-gen "$CI_PROJECT_DIR"/.local/bin/ghcup-gen
rm -rf "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup

ghcup-gen check -f ghcup-${JSON_VERSION}.json

ghcup numeric-version

ghcup -v -c install ${GHC_VERSION}
ghcup -v -c set ${GHC_VERSION}
ghcup -v -c install-cabal

cabal --version

ghcup -v -c debug-info

ghcup -v -c list
ghcup -v -c list -t ghc
ghcup -v -c list -t cabal

ghc --version
ghci --version
ghc-$(ghc --numeric-version) --version
ghci-$(ghc --numeric-version) --version

ghcup -v upgrade
ghcup -v upgrade -f

ghcup -v rm $(ghc --numeric-version)

