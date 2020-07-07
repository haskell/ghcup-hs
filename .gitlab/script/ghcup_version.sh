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

git describe --always

### build

ecabal update

if [ "${OS}" = "DARWIN" ] ; then
	ecabal build -w ghc-${GHC_VERSION} -ftui
else
	ecabal build -w ghc-${GHC_VERSION} -finternal-downloader -ftui
fi

cp "$(ecabal new-exec --enable-tests --verbose=0 --offline sh -- -c 'command -v ghcup')" .
cp "$(ecabal new-exec --enable-tests --verbose=0 --offline sh -- -c 'command -v ghcup-gen')" .

cp ./ghcup "$CI_PROJECT_DIR"/.local/bin/ghcup
cp ./ghcup-gen "$CI_PROJECT_DIR"/.local/bin/ghcup-gen

### cleanup

rm -rf "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup


### manual cli based testing


ghcup-gen check -f ghcup-${JSON_VERSION}.json

eghcup --numeric-version

# TODO: rm once we have tarballs
if [ "${OS}"  = "FREEBSD" ] ; then
	GHC_VERSION=8.6.3
	CABAL_VERSION=2.4.1.0
fi

eghcup install ${GHC_VERSION}
eghcup set ${GHC_VERSION}
eghcup install-cabal ${CABAL_VERSION}

cabal --version

eghcup debug-info

eghcup list
eghcup list -t ghc
eghcup list -t cabal

ghc_ver=$(ghc --numeric-version)
ghc --version
ghci --version
ghc-$(ghc --numeric-version) --version
ghci-$(ghc --numeric-version) --version


# test installing new ghc doesn't mess with currently set GHC
# https://gitlab.haskell.org/haskell/ghcup-hs/issues/7
if [ "${OS}" = "LINUX" ] ; then
	eghcup --downloader=wget install 8.4.4
else # test wget a bit
	eghcup install 8.4.4
fi
[ "$(ghc --numeric-version)" = "${ghc_ver}" ]
eghcup set 8.4.4
eghcup set 8.4.4
[ "$(ghc --numeric-version)" = "8.4.4" ]
eghcup set ${GHC_VERSION}
[ "$(ghc --numeric-version)" = "${ghc_ver}" ]
eghcup rm 8.4.4
[ "$(ghc --numeric-version)" = "${ghc_ver}" ]

eghcup rm $(ghc --numeric-version)

eghcup upgrade
eghcup upgrade -f

