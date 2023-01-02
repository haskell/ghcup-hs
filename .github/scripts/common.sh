#!/bin/sh

if [ "${RUNNER_OS}" = "Windows" ] ; then
	ext=".exe"
else
	ext=''
fi

ecabal() {
	cabal "$@"
}

sync_from_retry() {
	if [ "${RUNNER_OS}" != "Windows" ] ; then
		cabal_store_path="$(dirname "$(cabal help user-config | tail -n 1 | xargs)")/store"
	else
		cabal_store_path="${CABAL_DIR}/store"
	fi

    sync_from || { sleep 9 ; rm -rf "${cabal_store_path:?}"/* ; sync_from || { sleep 20 ; rm -rf "${cabal_store_path:?}"/* ; sync_from ; } }
}

sync_from() {
	if [ "${RUNNER_OS}" != "Windows" ] ; then
		cabal_store_path="$(dirname "$(cabal help user-config | tail -n 1 | xargs)")/store"
	fi

	cabal-cache sync-from-archive \
		--host-name-override=${S3_HOST} \
		--host-port-override=443 \
		--host-ssl-override=True \
		--region us-west-2 \
		$([ "${RUNNER_OS}" != "Windows" ] && echo --store-path="$cabal_store_path") \
		--archive-uri "s3://ghcup-hs/${RUNNER_OS}-${ARCH}-${DISTRO}"
}

sync_to_retry() {
    sync_to || { sleep 9 ; sync_to || { sleep 20 ; sync_to ; } }
}

sync_to() {
	if [ "${RUNNER_OS}" != "Windows" ] ; then
		cabal_store_path="$(dirname "$(cabal help user-config | tail -n 1 | xargs)")/store"
	fi

	cabal-cache sync-to-archive \
		--host-name-override=${S3_HOST} \
		--host-port-override=443 \
		--host-ssl-override=True \
		--region us-west-2 \
		$([ "${RUNNER_OS}" != "Windows" ] && echo --store-path="$cabal_store_path") \
		--archive-uri "s3://ghcup-hs/${RUNNER_OS}-${ARCH}-${DISTRO}"
}

raw_eghcup() {
	"$GHCUP_BIN/ghcup${ext}" -v -c "$@"
}

eghcup() {
	if [ "${OS}" = "Windows" ] ; then
		"$GHCUP_BIN/ghcup${ext}" -c -s "file:/$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml" "$@"
	else
		"$GHCUP_BIN/ghcup${ext}" -c -s "file://$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml" "$@"
	fi
}

sha_sum() {
	if [ "${OS}" = "FreeBSD" ] ; then
		sha256 "$@"
	else
		sha256sum "$@"
	fi

}

git_describe() {
	git config --global --get-all safe.directory | grep '^\*$' || git config --global --add safe.directory "*"
	git describe --always
}

download_cabal_cache() {
	(
	set -e
	dest="$HOME/.local/bin/cabal-cache"
    url=""
	exe=""
	cd /tmp
	case "${RUNNER_OS}" in
		"Linux")
			case "${ARCH}" in
				"32") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental4/i386-linux-cabal-cache
					;;
				"64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental4/x86_64-linux-cabal-cache
					;;
				"ARM64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental4/aarch64-linux-cabal-cache
					;;
				"ARM") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental4/armv7-linux-cabal-cache
					;;
			esac
			;;
		"FreeBSD")
			url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental4/x86_64-portbld-freebsd-cabal-cache
			;;
		"Windows")
			exe=".exe"
			url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental4/x86_64-mingw64-cabal-cache
			;;
		"macOS")
			case "${ARCH}" in
				"ARM64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental4/aarch64-apple-darwin-cabal-cache
					;;
				"64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental4/x86_64-apple-darwin-cabal-cache
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
		sha_sum cabal-cache${exe}
		mv "cabal-cache${exe}" "${dest}${exe}"
		chmod +x "${dest}${exe}"
	fi
    )
}

build_with_cache() {
	ecabal configure "$@"
	ecabal build --dependencies-only "$@" --dry-run
	sync_from_retry
	ecabal build --dependencies-only "$@" || sync_to_retry
	sync_to_retry
	ecabal build "$@"
	sync_to_retry
}

install_ghcup() {
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
}

strip_binary() {
	(
	set -e
	local binary=$1
	case "$(uname -s)" in
		"Darwin"|"darwin")
			;;
		MSYS_*|MINGW*)
			;;
		*)
			strip -s "${binary}"
		   ;;
   esac
	)
}
