#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

ecabal() {
	cabal --store-dir="$(pwd)"/.store "$@"
}

git describe

ecabal update
ecabal install -w ghc-${GHC_VERSION} --installdir="$CI_PROJECT_DIR"/.local/bin hlint

hlint -r app/ lib/ test/ 

