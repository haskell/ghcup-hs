#!/usr/bin/env bash

set -eux

. .github/scripts/prereq.sh

mkdir -p "$CI_PROJECT_DIR"/.local/bin

### build

ecabal() {
	cabal "$@"
}

raw_eghcup() {
	"$GHCUP_BIN/ghcup${ext}" -v -c "$@"
}

eghcup() {
	if [ "${OS}" = "Windows" ] ; then
		"$GHCUP_BIN/ghcup${ext}" -v -c -s file:/$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml "$@"
	else
		"$GHCUP_BIN/ghcup${ext}" -v -c -s file://$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml "$@"
	fi
}

sha_sum() {
	if [ "${OS}" = "FreeBSD" ] ; then
		sha256 "$@"
	else
		sha256sum "$@"
	fi

}

if [ "${OS}" = "Windows" ] ; then
	GHCUP_DIR="${GHCUP_INSTALL_BASE_PREFIX}"/ghcup
else
	GHCUP_DIR="${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup
fi

rm -rf "${GHCUP_DIR}"
mkdir -p "${GHCUP_BIN}"

if [ "${OS}" = "Windows" ] ; then
	ext=".exe"
else
	ext=''
fi
ls -lah out
find out
cp "out/${ARTIFACT}"-* "$GHCUP_BIN/ghcup${ext}"
chmod +x "$GHCUP_BIN/ghcup${ext}"
echo "$PATH"

"$GHCUP_BIN/ghcup${ext}" --version
eghcup --version
sha_sum "$GHCUP_BIN/ghcup${ext}"
sha_sum "$(raw_eghcup --offline whereis ghcup)"

git describe --always

eghcup install ghc "${GHC_VERSION}"
eghcup install cabal

ecabal update

eghcup debug-info

eghcup compile hls -j $(nproc) -g ${HLS_TARGET_VERSION} --ghc ${GHC_VERSION}

[ `$(eghcup whereis hls ${HLS_TARGET_VERSION}) --numeric-version` = "${HLS_TARGET_VERSION}" ] || [ `$(eghcup whereis hls ${HLS_TARGET_VERSION}) --numeric-version | sed 's/.0$//'` = "${HLS_TARGET_VERSION}" ]

# nuke
eghcup nuke
[ ! -e "${GHCUP_DIR}" ]

