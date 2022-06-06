#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

git describe --always

### build

export BOOTSTRAP_HASKELL_NONINTERACTIVE=yes
export BOOTSTRAP_HASKELL_GHC_VERSION=$GHC_VERSION
export BOOTSTRAP_HASKELL_CABAL_VERSION=$CABAL_VERSION

./scripts/bootstrap/bootstrap-haskell

[ "$(ghc --numeric-version)" = "${GHC_VERSION}" ]

