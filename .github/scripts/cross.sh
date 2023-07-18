#!/usr/bin/env bash

set -ex

. .github/scripts/common.sh

run() {
    "$@"
}

if [ "${OS}" = "Windows" ] ; then
	GHCUP_DIR="${GHCUP_INSTALL_BASE_PREFIX}"/ghcup
else
	GHCUP_DIR="${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup
fi

git_describe

rm -rf "${GHCUP_DIR}"
mkdir -p "${GHCUP_BIN}"

cp "out/${ARTIFACT}"-* "$GHCUP_BIN/ghcup${ext}"
cp "out/test-${ARTIFACT}"-* "ghcup-test${ext}"
chmod +x "$GHCUP_BIN/ghcup${ext}"
chmod +x "ghcup-test${ext}"

"$GHCUP_BIN/ghcup${ext}" --version
eghcup --version
sha_sum "$GHCUP_BIN/ghcup${ext}"
sha_sum "$(raw_eghcup --offline whereis ghcup)"


### cross build

eghcup --numeric-version

eghcup install ghc "${GHC_VER}"
eghcup set ghc "${GHC_VER}"
eghcup install cabal "${CABAL_VER}"

cabal --version

eghcup debug-info

ecabal update

"${WRAPPER}" "$GHCUP_BIN/ghcup${ext}" -c -s "file://$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml" -v \
	compile ghc \
	$(if [ -n "${HADRIAN_FLAVOUR}" ] ; then printf "%s" "--flavour=${HADRIAN_FLAVOUR}" ; else true ; fi) \
	-j "$(nproc)" \
	-v "${GHC_TARGET_VERSION}" \
	-b "${GHC_VER}" \
	-x "${CROSS}" \
	-- ${BUILD_CONF_ARGS}
eghcup set ghc "${CROSS}-${GHC_TARGET_VERSION}"

[ "$($(eghcup whereis ghc "${CROSS}-${GHC_TARGET_VERSION}") --numeric-version)" = "${GHC_TARGET_VERSION}" ]

# test that doing fishy symlinks into GHCup dir doesn't cause weird stuff on 'ghcup nuke'
mkdir no_nuke/
mkdir no_nuke/bar
echo 'foo' > no_nuke/file
echo 'bar' > no_nuke/bar/file
ln -s "$CI_PROJECT_DIR"/no_nuke/ "${GHCUP_DIR}"/cache/no_nuke
ln -s "$CI_PROJECT_DIR"/no_nuke/ "${GHCUP_DIR}"/logs/no_nuke

# nuke
eghcup nuke
[ ! -e "${GHCUP_DIR}" ]

# make sure nuke doesn't resolve symlinks
[ -e "$CI_PROJECT_DIR"/no_nuke/file ]
[ -e "$CI_PROJECT_DIR"/no_nuke/bar/file ]

