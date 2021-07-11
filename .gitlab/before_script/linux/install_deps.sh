#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}"

sudo apt-get update -y
sudo apt-get install -y libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https

case "${ARCH}" in
	ARM*)
		case "${ARCH}" in
			"ARM")
				ghc_url=https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-armv7-deb10-linux.tar.xz
				cabal_url=home.smart-cactus.org/~ben/cabal-install-${CABAL_VERSION}-armv7-linux-bootstrapped.tar.xz
				;;
			"ARM64")
				ghc_url=https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-aarch64-deb10-linux.tar.xz
				cabal_url=https://downloads.haskell.org/~cabal/cabal-install-${CABAL_VERSION}/cabal-install-${CABAL_VERSION}-aarch64-ubuntu-18.04.tar.xz
				;;
			*)
				exit 1 ;;
		esac

		mkdir -p "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin

		curl -O "${ghc_url}"
		tar -xf ghc-*.tar.*
		cd ghc-${GHC_VERSION}
		./configure --prefix="${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/ghc/${GHC_VERSION}
		make install
		for i in "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/ghc/${GHC_VERSION}/bin/*-${GHC_VERSION} ; do
			ln -s "${i}" "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin/${i##*/}
		done
		for x in "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin/*-${GHC_VERSION} ; do
			ln -s ${x##*/} ${x%-${GHC_VERSION}}
		done
		cd ..
		rm -rf ghc-${GHC_VERSION} ghc-*.tar.*
		unset x i

		mkdir cabal-install
		cd cabal-install
		curl -O "${cabal_url}"
		tar -xf cabal-install-*
		mv cabal "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin/cabal
		cd ..
		rm -rf cabal-install

		;;
	*) 
		url=https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup

		curl -sSfL "${url}" > ./ghcup-bin
		chmod +x ghcup-bin

		./ghcup-bin upgrade -i -f
		./ghcup-bin install ${GHC_VERSION}
		./ghcup-bin set ${GHC_VERSION}
		./ghcup-bin install-cabal ${CABAL_VERSION}

		;;
esac

