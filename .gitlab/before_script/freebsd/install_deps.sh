#!/bin/sh

set -eux

# pkg install --force --yes --no-repo-update curl gcc gmp gmake ncurses perl5 libffi libiconv

. "$( cd "$(dirname "$0")" ; pwd -P )/../../ghcup_env"

mkdir -p "${TMPDIR}"

if freebsd-version | grep -E '^12.*' ; then
	freebsd_ver=12
elif freebsd-version | grep -E '^13.*' ; then
	freebsd_ver=13
else
	(>&2 echo "Unsupported FreeBSD version! Please report a bug at https://gitlab.haskell.org/haskell/ghcup-hs/-/issues")
	exit 1
fi
curl -sSfL https://downloads.haskell.org/~ghcup/x86_64-freebsd${freebsd_ver}-ghcup > ./ghcup-bin
chmod +x ghcup-bin

./ghcup-bin -v upgrade -i -f
./ghcup-bin -v install ${GHC_VERSION}
./ghcup-bin -v set ${GHC_VERSION}
./ghcup-bin -v install-cabal ${CABAL_VERSION}

exit 0
