#!/bin/sh

set -eux

. .github/scripts/prereq.sh

if ! command -v ghcup ; then
	find "$GHCUP_INSTALL_BASE_PREFIX"
	mkdir -p "$GHCUP_BIN"
	mkdir -p "$GHCUP_BIN"/../cache

	if [ "${RUNNER_OS}" = "FreeBSD" ] ; then
		curl -o ghcup https://downloads.haskell.org/ghcup/tmp/x86_64-portbld-freebsd-ghcup-0.1.18.1
		chmod +x ghcup
		mv ghcup "$HOME/.local/bin/ghcup"
	else
		curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
	fi
fi

if [ "${RUNNER_OS}" != "FreeBSD" ] ; then
	if [ "${DISTRO}" != "Debian" ] ; then # ! armv7 or aarch64 linux
		ghcup -v install ghc       --isolate="$HOME/.local"     --force 8.10.7
		ghcup -v install ghc --set --isolate="$HOME/.local"     --force "$GHC_VER"
		ghcup -v install cabal     --isolate="$HOME/.local/bin" --force "$CABAL_VER"
		ghc --version
		cabal --version
		GHC="ghc-${GHC_VER}"
		GHC_CABAL_CACHE="ghc-8.10.7"
	else
		ghcup -v install cabal --isolate="$HOME/.local/bin" --force "$CABAL_VER"
		cabal --version
		GHC="ghc"
		GHC_CABAL_CACHE="ghc"
	fi
else
	ghcup -v install ghc --isolate="$HOME/.local" --force 8.10.7
	ghc --version
	cabal --version
	GHC="ghc"
	GHC_CABAL_CACHE="ghc-8.10.7"
fi

git describe --all

ecabal() {
	cabal "$@"
}

sync_from() {
	cabal_store_path="$(dirname "$(cabal help user-config | tail -n 1 | xargs)")/store"

	cabal-cache sync-from-archive \
		--host-name-override=s3.us-west-004.backblazeb2.com \
		--host-port-override=443 \
		--host-ssl-override=True \
		--region us-west-2 \
		--store-path="$cabal_store_path" \
		--archive-uri s3://ghcup-hs
}

sync_to() {
	cabal_store_path="$(dirname "$(cabal help user-config | tail -n 1 | xargs)")/store"

	cabal-cache sync-to-archive \
		--host-name-override=s3.us-west-004.backblazeb2.com \
		--host-port-override=443 \
		--host-ssl-override=True \
		--region us-west-2 \
		--store-path="$cabal_store_path" \
		--archive-uri s3://ghcup-hs
}

build_with_cache() {
	ecabal configure "$@"
	sync_from || true
	ecabal build --dependencies-only "$@" || { sync_to || true ; }
	sync_to || true
	ecabal build "$@"
	sync_to || true
}

# build
ecabal update


# make sure cabal-cache is available
if ! command -v cabal-cache ; then
	(
	url=""
	exe=""
	cd /tmp
	case "${RUNNER_OS}" in
		"Linux")
			case "${DISTRO}" in
				"Alpine")
					case "${ARCH}" in
						"32") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.1/i386-linux-alpine-cabal-cache-1.0.5.1
							;;
						"64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.1/x86_64-linux-alpine-cabal-cache-1.0.5.1
							;;
					esac
					;;
				*)
					case "${ARCH}" in
						"64") url=https://github.com/haskell-works/cabal-cache/releases/download/v1.0.5.1/cabal-cache-x86_64-linux.gz
					esac
					;;
			esac
			;;
		"FreeBSD")
			;;
		"Windows")
			exe=".exe"
			url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.1/x86_64-mingw64-cabal-cache-1.0.5.1.exe
			;;
		"macOS")
			case "${ARCH}" in
				"ARM64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.1/aarch64-apple-darwin-cabal-cache-1.0.5.1
					;;
				"64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.1/x86_64-apple-darwin-cabal-cache-1.0.5.1
					;;
			esac
			;;
	esac

	if [ -n "${url}" ] ; then
		case "${url##*.}" in
			"gz")
				curl -L -o - "${url}" | gunzip > cabal-cache${exe}
				;;
			*)
				curl -o cabal-cache${exe} -L "${url}"
				;;
		esac
		chmod +x cabal-cache${exe}
		cp "cabal-cache${exe}" "$HOME/.local/bin/cabal-cache${exe}"
	else
		ecabal install -w "${GHC_CABAL_CACHE}" --overwrite-policy=always --install-method=copy --installdir="$HOME/.local/bin" cabal-cache
		mkdir -p "${CI_PROJECT_DIR}/out"
		cp "$HOME/.local/bin/cabal-cache" "${CI_PROJECT_DIR}/out/cabal-cache-${RUNNER_OS}-${DISTRO}-${ARCH}"
	fi
	)
fi


if [ "${RUNNER_OS}" = "Linux" ] ; then
	if [ "${ARCH}" = "32" ] ; then
		build_with_cache -w "${GHC}" --ghc-options='-split-sections -optl-static' -ftui
	elif [ "${ARCH}" = "64" ] ; then
		build_with_cache -w "${GHC}" --ghc-options='-split-sections -optl-static' -ftui
	else
		build_with_cache -w "${GHC}" -ftui
	fi
elif [ "${RUNNER_OS}" = "FreeBSD" ] ; then
	build_with_cache -w "${GHC}" --ghc-options='-split-sections' --constraint="zlib +bundled-c-zlib" --constraint="zip +disable-zstd" -ftui
elif [ "${RUNNER_OS}" = "Windows" ] ; then
	build_with_cache -w "${GHC}" --constraint="zlib +bundled-c-zlib" --constraint="lzma +static"
else
	build_with_cache -w "${GHC}" --constraint="zlib +bundled-c-zlib" --constraint="lzma +static" -ftui
fi


mkdir -p out
binary=$(ecabal new-exec -w "${GHC}" --verbose=0 --offline sh -- -c 'command -v ghcup')
ver=$("${binary}" --numeric-version)
if [ "${RUNNER_OS}" = "macOS" ] ; then
	strip "${binary}"
else
	if [ "${RUNNER_OS}" != "Windows" ] ; then
		strip -s "${binary}"
	fi
fi
cp "${binary}" "out/${ARTIFACT}-${ver}"
cp ./dist-newstyle/cache/plan.json "out/${ARTIFACT}.plan.json"

