#!/usr/bin/env bash

set -eux

. .github/scripts/prereq.sh

mkdir -p "$CI_PROJECT_DIR"/.local/bin

git describe --always

### build

./scripts/bootstrap/bootstrap-haskell

[ "$(ghc --numeric-version)" = "${BOOTSTRAP_HASKELL_GHC_VERSION}" ]

