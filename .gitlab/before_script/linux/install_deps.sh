#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}"

sudo apt-get update -y
sudo apt-get install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential

if [ "${CROSS}" = "arm-linux-gnueabihf" ] ; then
	sudo apt-get install -y gcc-arm-linux-gnueabihf
	sudo dpkg --add-architecture armhf
	sudo apt-get update -y
	sudo apt-get install -y libncurses-dev:armhf
fi

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION=$GHC_VERSION
export BOOTSTRAP_HASKELL_CABAL_VERSION=$CABAL_VERSION
export BOOTSTRAP_HASKELL_VERBOSE=1

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

rm "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin/ghcup

