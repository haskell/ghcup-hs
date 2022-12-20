#!/bin/sh

set -eux

. .github/scripts/prereq.sh
. .github/scripts/common.sh


# ensure ghcup
if ! command -v ghcup ; then
	install_ghcup
fi

if [ "${RUNNER_OS}" = "macOS" ] ; then
	rm -rf "${CABAL_DIR}"/store/*
fi


# ensure ghc
ghcup install ghc --set 8.10.7
ghcup install cabal --set recommended

ghc --version
cabal --version

cabal update

git clone --single-branch --branch main https://github.com/haskell-works/cabal-cache.git
cd cabal-cache

if [ "${DISTRO}" = "Alpine" ] ; then
	cabal build --ghc-options='-split-sections -optl-static'
else
	cabal build
fi

binary=$(cabal list-bin cabal-cache)
cd ..

mkdir -p out
strip_binary "${binary}"
cp "${binary}" "out/${ARTIFACT}"

