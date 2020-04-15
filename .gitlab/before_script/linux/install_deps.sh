#!/bin/sh

set -eux

sudo apt-get update -y
sudo apt-get install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

curl -sSfL https://gitlab.haskell.org/haskell/ghcup/-/raw/master/ghcup > ./ghcup-legacy
chmod +x ghcup-legacy

./ghcup-legacy install ${GHC_VERSION}
./ghcup-legacy set ${GHC_VERSION}
./ghcup-legacy install-cabal

