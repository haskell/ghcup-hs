#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

ecabal() {
	cabal --store-dir="$(pwd)"/.store "$@"
}

eghcup() {
	ghcup -v -c -s file://$(pwd)/ghcup-${JSON_VERSION}.yaml "$@"
}

git describe --always

### build

ecabal update

export BOOTSTRAP_HASKELL_NONINTERACTIVE=yes
export BOOTSTRAP_HASKELL_GHC_VERSION=$GHC_VERSION
export BOOTSTRAP_HASKELL_CABAL_VERSION=$CABAL_VERSION

./bootstrap-haskell

