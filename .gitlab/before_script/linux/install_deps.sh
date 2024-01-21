#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}"

apt-get update -y
apt-get install -y \
    apt-transport-https \
    autoconf \
    automake \
    build-essential \
    curl \
    gcc \
    git \
    gnupg2 \
    libbz2-dev \
    libffi-dev \
    libffi7 \
    libgmp-dev \
    libgmp10 \
    liblzma-dev \
    libncurses-dev \
    libncurses5 \
    libnuma-dev \
    libssl-dev \
    libtinfo5 \
    lsb-release \
    pkg-config \
    software-properties-common \
    wget \
    zlib1g-dev


if [ "${CROSS}" = "arm-linux-gnueabihf" ] ; then
    apt-get install -y gcc-arm-linux-gnueabihf
    dpkg --add-architecture armhf
    apt-get update -y
    apt-get install -y libncurses-dev:armhf
fi

apt-get install -y libseccomp-dev
curl -L https://dev.exherbo.org/~alip/sydbox/sydbox-2.1.0.tar.bz2 | tar -xj
cd sydbox-2.1.0
./configure
make
make install
cd ..

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION=$GHC_VERSION
export BOOTSTRAP_HASKELL_CABAL_VERSION=$CABAL_VERSION
export BOOTSTRAP_HASKELL_VERBOSE=1

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

rm "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin/ghcup

