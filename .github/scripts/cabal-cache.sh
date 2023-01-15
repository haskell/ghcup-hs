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
ghcup install ghc --set "${GHC_VER}"
ghcup install cabal --set "${CABAL_VER}"

ghc --version
cabal --version

cabal update

cd /tmp
cabal unpack cabal-install-3.8.1.0
cd cabal-install-3.8.1.0

if [ "${DISTRO}" = "Alpine" ] ; then
	cabal build --constraint='lukko -ofd-locking' --ghc-options='-split-sections -optl-static' cabal-install:exe:cabal
elif  [ "${DISTRO}" = "Ubuntu" ] ; then
	cabal build --constraint='lukko -ofd-locking' cabal-install:exe:cabal
else
	cabal build cabal-install:exe:cabal
fi

binary=$(cabal list-bin cabal-install:exe:cabal)
cd ..

mkdir -p "${CI_PROJECT_DIR}/out"
strip_binary "${binary}"
cp "${binary}" "${CI_PROJECT_DIR}/out/${ARTIFACT}"

