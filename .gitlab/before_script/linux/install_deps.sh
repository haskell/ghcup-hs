#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}"

sudo apt-get update -y
sudo apt-get install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget

curl -sSfL https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > ./ghcup-bin
chmod +x ghcup-bin

./ghcup-bin upgrade -i -f
./ghcup-bin install ${GHC_VERSION}
./ghcup-bin set ${GHC_VERSION}
./ghcup-bin install-cabal ${CABAL_VERSION}

