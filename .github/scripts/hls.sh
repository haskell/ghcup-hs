#!/usr/bin/env bash

set -eux

. .github/scripts/prereq.sh
. .github/scripts/common.sh

mkdir -p "$CI_PROJECT_DIR"/.local/bin

### build



if [ "${OS}" = "Windows" ] ; then
	GHCUP_DIR="${GHCUP_INSTALL_BASE_PREFIX}"/ghcup
else
	GHCUP_DIR="${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup
fi

rm -rf "${GHCUP_DIR}"
mkdir -p "${GHCUP_BIN}"

ls -lah out
find out
cp "out/${ARTIFACT}"-* "$GHCUP_BIN/ghcup${ext}"
chmod +x "$GHCUP_BIN/ghcup${ext}"
echo "$PATH"

"$GHCUP_BIN/ghcup${ext}" --version
eghcup --version
sha_sum "$GHCUP_BIN/ghcup${ext}"
sha_sum "$(raw_eghcup --offline whereis ghcup)"

git_describe

eghcup install ghc "${GHC_VERSION}"
eghcup install cabal

ecabal update

if ! command -v cabal-cache ; then
	download_cabal_cache "$HOME/.local/bin/cabal-cache"
fi

if ! cabal-cache version ; then
	build_cabal_cache "$HOME/.local/bin"
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

