#!/usr/bin/env bash
set -eo pipefail

export GHCUP_INSTALL_BASE_PREFIX=$RUNNER_TEMP/foobarbaz

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

source $GHCUP_INSTALL_BASE_PREFIX/.ghcup/env || source ~/.bashrc

ghcup --version
which ghcup | grep foobarbaz

ghcup -v --url-source=file:$METADATA_FILE install ghc --set $GHC_VERSION

ghc --version
echo 'main = print $ 1 + 1' > main.hs
ghc main.hs
[[ $(./main) -eq 2 ]]
