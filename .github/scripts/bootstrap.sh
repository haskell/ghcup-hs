#!/usr/bin/env bash

set -eux

. .github/scripts/env.sh

mkdir -p "$CI_PROJECT_DIR"/.local/bin

git describe --always

### build

./scripts/bootstrap/bootstrap-haskell

[ "$(ghc --numeric-version)" = "${BOOTSTRAP_HASKELL_GHC_VERSION}" ]
# https://github.com/actions/runner-images/issues/7061
[ "$(ghcup config | grep --color=never meta-mode)" = "meta-mode: Lax" ]

