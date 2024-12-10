#!/bin/sh

set -eux

. .github/scripts/common.sh

git_describe

# ensure ghcup
install_ghcup

# ensure cabal-cache
download_cabal_cache "$HOME/.local/bin/cabal-cache"

# install toolchain (if necessary)
ghcup -v install ghc --set --force "$GHC_VER"
ghcup -v install cabal --force "$CABAL_VER"
ghc --version
cabal --version
GHC="ghc-${GHC_VER}"

# build
ecabal update
build_with_cache --project-file=cabal.project.release -w "${GHC}" --enable-tests "$@"

# set up artifacts
mkdir -p out
binary=$(cabal --project-file=cabal.project.release list-bin ghcup)
binary_test=$(cabal --project-file=cabal.project.release list-bin ghcup-test)
binary_opttest=$(cabal --project-file=cabal.project.release list-bin ghcup-optparse-test)
ver=$("${binary}" --numeric-version)
strip_binary "${binary}"
cp "${binary}" "out/${ARTIFACT}-${ver}${ext}"
cp "${binary_test}" "out/test-${ARTIFACT}-${ver}${ext}"
cp "${binary_opttest}" "out/test-optparse-${ARTIFACT}-${ver}${ext}"
cp ./dist-newstyle/cache/plan.json "out/${ARTIFACT}.plan.json"

