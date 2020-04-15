#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

curl -sSfL https://gitlab.haskell.org/haskell/ghcup/-/raw/master/ghcup > ./ghcup-legacy
chmod +x ghcup-legacy

./ghcup-legacy install ${GHC_VERSION}
./ghcup-legacy set ${GHC_VERSION}
./ghcup-legacy install-cabal

exit 0
