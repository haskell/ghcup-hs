#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

ecabal() {
	cabal --store-dir="$CI_PROJECT_DIR"/.store "$@"
}

eghcup() {
	ghcup -v -c -s file://$CI_PROJECT_DIR/ghcup-${JSON_VERSION}.yaml "$@"
}

git describe --always

### build

ecabal update

(
	cd /tmp
	ecabal install -w ghc-${GHC_VERSION} --installdir="$CI_PROJECT_DIR"/.local/bin hspec-discover
)

if [ "${OS}" = "DARWIN" ] ; then
	ecabal build -w ghc-${GHC_VERSION} -ftui
	ecabal test -w ghc-${GHC_VERSION} -ftui ghcup-test
elif [ "${OS}" = "LINUX" ] ; then
	if [ "${ARCH}" = "32" ] ; then
		ecabal build -w ghc-${GHC_VERSION} -finternal-downloader -ftui -ftar
		ecabal test -w ghc-${GHC_VERSION} -finternal-downloader -ftui -ftar ghcup-test
	else
		ecabal build -w ghc-${GHC_VERSION} -finternal-downloader -ftui
		ecabal test -w ghc-${GHC_VERSION} -finternal-downloader -ftui ghcup-test
	fi
else
	ecabal build -w ghc-${GHC_VERSION} -finternal-downloader -ftui
	ecabal test -w ghc-${GHC_VERSION} -finternal-downloader -ftui ghcup-test
fi

ecabal haddock -w ghc-${GHC_VERSION} -ftar

cp "$(ecabal new-exec -w ghc-${GHC_VERSION} --verbose=0 --offline sh -- -c 'command -v ghcup')" .
cp "$(ecabal new-exec -w ghc-${GHC_VERSION} --verbose=0 --offline sh -- -c 'command -v ghcup-gen')" .

cp ./ghcup "$CI_PROJECT_DIR"/.local/bin/ghcup
cp ./ghcup-gen "$CI_PROJECT_DIR"/.local/bin/ghcup-gen

### cleanup

rm -rf "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup


### manual cli based testing


ghcup-gen check -f ghcup-${JSON_VERSION}.yaml

eghcup --numeric-version

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
	eghcup --downloader=wget install 8.10.3
else # test wget a bit
	eghcup install 8.10.3
fi
[ "$(ghc --numeric-version)" = "${ghc_ver}" ]
eghcup set 8.10.3
eghcup set 8.10.3
[ "$(ghc --numeric-version)" = "8.10.3" ]
eghcup set ${GHC_VERSION}
[ "$(ghc --numeric-version)" = "${ghc_ver}" ]
eghcup rm 8.10.3
[ "$(ghc --numeric-version)" = "${ghc_ver}" ]

if [ "${OS}" = "DARWIN" ] ; then
	eghcup install hls
	haskell-language-server-wrapper --version

	eghcup install stack
	stack --version
elif [ "${OS}" = "LINUX" ] ; then
	if [ "${ARCH}" = "64" ] ; then
		eghcup install hls
		haskell-language-server-wrapper --version

		eghcup install stack
		stack --version
	fi
fi


eghcup rm $(ghc --numeric-version)

# https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/116
if [ "${OS}" = "LINUX" ] ; then
	if [ "${ARCH}" = "64" ] ; then
		eghcup install cabal -u https://oleg.fi/cabal-install-3.4.0.0-rc4/cabal-install-3.4.0.0-x86_64-ubuntu-16.04.tar.xz 3.4.0.0-rc4
		eghcup rm cabal 3.4.0.0-rc4
	fi
fi

eghcup upgrade
eghcup upgrade -f

