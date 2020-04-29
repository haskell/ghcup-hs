#!/bin/sh

set -eux

sudo apt-get update -y
sudo apt-get install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev git wget

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

curl -sSfL https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > ./ghcup-bin
chmod +x ghcup-bin

./ghcup-bin install ${GHC_VERSION}
./ghcup-bin set ${GHC_VERSION}
./ghcup-bin install-cabal ${CABAL_VERSION}

