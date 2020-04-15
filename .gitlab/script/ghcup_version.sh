#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

ecabal() {
	cabal --store-dir="$(pwd)"/.store "$@"
}

eghcup() {
	ghcup -v -c -s file://$(pwd)/ghcup-${JSON_VERSION}.json "$@"
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

eghcup numeric-version

eghcup install ${GHC_VERSION}
eghcup set ${GHC_VERSION}
eghcup install-cabal

cabal --version

eghcup debug-info

eghcup list
eghcup list -t ghc
eghcup list -t cabal

ghc --version
ghci --version
ghc-$(ghc --numeric-version) --version
ghci-$(ghc --numeric-version) --version

eghcup upgrade
eghcup upgrade -f

eghcup rm $(ghc --numeric-version)

