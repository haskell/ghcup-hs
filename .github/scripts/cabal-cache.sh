#!/bin/sh

set -eux

. .github/scripts/prereq.sh
. .github/scripts/common.sh


# ensure ghcup
if ! command -v ghcup ; then
	install_ghcup
fi


# ensure ghc
ghcup install ghc --set 8.10.7
ghcup install cabal --set recommended

ghc --version
cabal --version

cabal update

git clone --single-branch --branch main https://github.com/hasufell/cabal-cache.git
cd cabal-cache

cabal build
binary=$(cabal list-bin cabal-cache)
cd ..

mkdir -p out
strip_binary "${binary}"
cp "${binary}" "out/${ARTIFACT}"

