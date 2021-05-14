#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}"

ednf() {
	case "${ARCH}" in
		"ARM")
			sudo dnf -y --forcearch armv7hl "$@"
			;;
		"ARM64")
			sudo dnf -y --forcearch aarch64 "$@"
			;;
		*) exit 1 ;;
	esac
}

ednf update
ednf install gcc gcc-c++ gmp gmp-devel make ncurses ncurses-devel xz xz-devel perl zlib zlib-devel openssl-devel openssl-libs openssl libffi libffi-devel lbzip2 lbzip2-utils bzip2-devel
if [ "${ARCH}" = "ARM64" ] ; then
	ednf install numactl numactl-libs numactl-devel
fi
ednf install bash wget curl git tar
ednf install llvm9.0 llvm9.0-devel llvm9.0-libs llvm9.0-static

case "${ARCH}" in
	"ARM")
		ghc_url=https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-armv7-deb10-linux.tar.xz
		cabal_url=home.smart-cactus.org/~ben/cabal-install-${CABAL_VERSION}-armv7-linux-bootstrapped.tar.xz
		;;
	"ARM64")
		ghc_url=https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-aarch64-deb10-linux.tar.xz
		cabal_url=https://downloads.haskell.org/~cabal/cabal-install-${CABAL_VERSION}/cabal-install-${CABAL_VERSION}-aarch64-ubuntu-18.04.tar.xz
		;;
	*) exit 1 ;;
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
