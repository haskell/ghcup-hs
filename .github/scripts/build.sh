#!/bin/sh

set -eux

. .github/scripts/prereq.sh
. .github/scripts/common.sh


# ensure ghcup
if ! command -v ghcup ; then
	install_ghcup
fi

# ensure cabal-cache
if ! cabal-cache version ; then
	download_cabal_cache "$HOME/.local/bin/cabal-cache"
fi

# ensure ghc
if [ "${RUNNER_OS}" != "FreeBSD" ] ; then
	if [ "${DISTRO}" != "Debian" ] ; then # ! armv7 or aarch64 linux
		if ! "ghc-${GHC_VER}" --numeric-version ; then
			ghcup -v install ghc --set --force "$GHC_VER"
		fi
		if [ "$(cabal --numeric-version || true)" != "${CABAL_VER}" ] ; then
			ghcup -v install cabal --force "$CABAL_VER"
		fi
		ghc --version
		cabal --version
		GHC="ghc-${GHC_VER}"
	else
		if [ "$(cabal --numeric-version || true)" != "${CABAL_VER}" ] ; then
			ghcup -v install cabal --force "$CABAL_VER"
		fi
		cabal --version
		GHC="ghc"
	fi
else
	ghc --version
	cabal --version
	GHC="ghc"
fi

git_describe


# build
ecabal update

if [ "${RUNNER_OS}" = "Linux" ] ; then
	if [ "${ARCH}" = "32" ] ; then
		build_with_cache -w "${GHC}" --ghc-options='-split-sections -optl-static' -ftui --enable-tests
	elif [ "${ARCH}" = "64" ] ; then
		build_with_cache -w "${GHC}" --ghc-options='-split-sections -optl-static' -ftui --enable-tests
	else
		build_with_cache -w "${GHC}" -ftui --enable-tests
	fi
elif [ "${RUNNER_OS}" = "FreeBSD" ] ; then
	build_with_cache -w "${GHC}" --ghc-options='-split-sections -pgmc clang++14' --constraint="zlib +bundled-c-zlib" --constraint="zip +disable-zstd" -ftui --enable-tests
elif [ "${RUNNER_OS}" = "Windows" ] ; then
	build_with_cache -w "${GHC}" --constraint="zlib +bundled-c-zlib" --constraint="lzma +static" --constraint="text -simdutf" --enable-tests
else
	build_with_cache -w "${GHC}" --constraint="zlib +bundled-c-zlib" --constraint="lzma +static" -ftui --enable-tests
fi


# set up artifacts
mkdir -p out
binary=$(cabal list-bin ghcup)
binary_test=$(cabal list-bin ghcup-test)
ver=$("${binary}" --numeric-version)
strip_binary "${binary}"
cp "${binary}" "out/${ARTIFACT}-${ver}${ext}"
cp "${binary_test}" "out/test-${ARTIFACT}-${ver}${ext}"
cp ./dist-newstyle/cache/plan.json "out/${ARTIFACT}.plan.json"

