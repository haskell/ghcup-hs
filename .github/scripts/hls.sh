#!/usr/bin/env bash

# TODO: allow local execution

set -eux

. .github/scripts/common.sh

mkdir -p "$GITHUB_WORKSPACE"/.local/bin

### build



if [ "${OS}" = "Windows" ] ; then
	export GHCUP_INSTALL_BASE_PREFIX=/c
	GHCUP_DIR="${GHCUP_INSTALL_BASE_PREFIX}"/ghcup
	ext=".exe"
else
	export GHCUP_INSTALL_BASE_PREFIX=$(pwd)
	GHCUP_DIR="${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup
	ext=""
fi

GHCUP_BINDIR="${GHCUP_DIR}/bin"
export PATH="${GHCUP_BINDIR}:$PATH"

rm -rf "${GHCUP_DIR}"
mkdir -p "${GHCUP_BINDIR}"

ls -lah out
find out
cp "out/${ARTIFACT}"-* "$GHCUP_BINDIR/ghcup${ext}"
export GHCUP_BIN="$GHCUP_BINDIR/ghcup${ext}"
chmod +x "$GHCUP_BINDIR/ghcup${ext}"
echo "$PATH"

"$GHCUP_BINDIR/ghcup${ext}" --version
eghcup --version
sha_sum "$GHCUP_BINDIR/ghcup${ext}"
sha_sum "$(raw_eghcup --offline whereis ghcup)"

git_describe

eghcup install ghc "${GHC_VERSION}"
eghcup install cabal "${CABAL_VERSION}"

ecabal update

if ! command -v cabal-cache ; then
	download_cabal_cache "$HOME/.local/bin/cabal-cache"
fi

eghcup debug-info

(
	cd /tmp
	git clone --depth 1 --branch "${HLS_TARGET_VERSION}" \
		https://github.com/haskell/haskell-language-server.git \
		"haskell-language-server-${HLS_TARGET_VERSION}"
	cd "haskell-language-server-${HLS_TARGET_VERSION}/"
	ecabal configure -w "ghc-${GHC_VERSION}" --disable-profiling --disable-tests --jobs="$(nproc)"
	ecabal build --dependencies-only -w "ghc-${GHC_VERSION}" --disable-profiling --disable-tests --jobs="$(nproc)" --dry-run
	sync_from
	ecabal build --dependencies-only -w "ghc-${GHC_VERSION}" --disable-profiling --disable-tests --jobs="$(nproc)" || sync_to
	sync_to
)

eghcup -v compile hls -j "$(nproc)" -g "${HLS_TARGET_VERSION}" --ghc "${GHC_VERSION}"

[ "$($(eghcup whereis hls "${HLS_TARGET_VERSION}") --numeric-version)" = "${HLS_TARGET_VERSION}" ] ||
	[ "$($(eghcup whereis hls "${HLS_TARGET_VERSION}") --numeric-version | sed 's/.0$//')" = "${HLS_TARGET_VERSION}" ]

# nuke
eghcup nuke
[ ! -e "${GHCUP_DIR}" ]

