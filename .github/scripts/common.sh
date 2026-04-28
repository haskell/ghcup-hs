#!/bin/sh

run() {
	"$@"
}

ecabal() {
	cabal "$@"
}

nonfatal() {
	"$@" || "$* failed"
}

sync_from() {
	if [ "${OS}" != "Windows" ] ; then
		cabal_store_path="$(dirname "$(cabal help user-config | tail -n 1 | xargs)")/store"
	fi

	cabal-cache.sh sync-from-archive \
		--host-name-override=${S3_HOST} \
		--host-port-override=443 \
		--host-ssl-override=True \
		--region us-west-2 \
		$([ "${OS}" != "Windows" ] && echo --store-path="$cabal_store_path") \
		--archive-uri "s3://ghcup-hs/${OS}-${ARCH}-${DISTRO}"
}

sync_to() {
	if [ "${OS}" != "Windows" ] ; then
		cabal_store_path="$(dirname "$(cabal help user-config | tail -n 1 | xargs)")/store"
	fi

	cabal-cache.sh sync-to-archive \
		--host-name-override=${S3_HOST} \
		--host-port-override=443 \
		--host-ssl-override=True \
		--region us-west-2 \
		$([ "${OS}" != "Windows" ] && echo --store-path="$cabal_store_path") \
		--archive-uri "s3://ghcup-hs/${OS}-${ARCH}-${DISTRO}"
}

raw_eghcup() {
	"$GHCUP_BIN" -v -c "$@"
}

eghcup() {
	if [ "${OS}" = "Windows" ] ; then
		"$GHCUP_BIN" -c -s "file:$(cygpath -m "${PROJECT_DIR}/data/metadata/ghcup-${JSON_VERSION}.yaml")" "$@"
	else
		"$GHCUP_BIN" -c -s "file://${PROJECT_DIR}/data/metadata/ghcup-${JSON_VERSION}.yaml" "$@"
	fi
}

sha_sum() {
	if [ "${OS}" = "FreeBSD" ] || [ "${OS}" = "OpenBSD" ] ; then
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
	local curdir=$(pwd)
	(
	set -e
	mkdir -p "$HOME/.local/bin"
	dest="$HOME/.local/bin/cabal-cache"
    url=""
	exe=""
	cd /tmp
	case "${OS}" in
		"Linux")
			case "${ARCH}" in
				"X86") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental5/i386-linux-cabal-cache
					;;
				"X64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental5/x86_64-linux-cabal-cache
					;;
				"ARM64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental5/aarch64-linux-cabal-cache
					;;
				"ARM") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental5/armv7-linux-cabal-cache
					;;
			esac
			;;
		"FreeBSD")
			url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental5/x86_64-portbld-freebsd-cabal-cache
			;;
		"Windows")
			exe=".exe"
			url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental5/x86_64-mingw64-cabal-cache
			;;
		"macOS")
			case "${ARCH}" in
				"ARM64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental5/aarch64-apple-darwin-cabal-cache
					;;
				"X64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/experimental5/x86_64-apple-darwin-cabal-cache
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

	# install shell wrapper
	cp "${curdir}"/.github/scripts/cabal-cache.sh "$HOME"/.local/bin/
	chmod +x "$HOME"/.local/bin/cabal-cache.sh
    )
}

build_with_cache() {
	ecabal configure "$@"
	ecabal build --dependencies-only "$@" --dry-run
	sync_from
	ecabal build --dependencies-only "$@" || sync_to
	sync_to
	ecabal build "$@"
	sync_to
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

mktempdir() {
    if test "${OS}" = "macOS"; then
        mktemp -d -t ghcup.XXXXXXX
    else
        mktemp -d
    fi
}

get_os() {
	my_uname="$(uname -s)"
	case "${my_uname}" in
		"Darwin"|"darwin")
			printf "%s" "macOS"
			;;
		MSYS_*|MINGW*)
			printf "%s" "Windows"
			;;
		*)
			printf "%s" "${my_uname}"
		   ;;
	esac
	unset my_uname
}

get_distro() {
    if [ -f /etc/os-release ]; then
        # freedesktop.org and systemd
        # shellcheck disable=SC1091
        . /etc/os-release
        printf "%s" "$NAME"
    elif command_exists lsb_release ; then
        # linuxbase.org
        printf "%s" "$(lsb_release -si)"
    elif [ -f /etc/lsb-release ]; then
        # For some versions of Debian/Ubuntu without lsb_release command
        # shellcheck disable=SC1091
        . /etc/lsb-release
        printf "%s" "$DISTRIB_ID"
    elif [ -f /etc/redhat-release ]; then
        case "$(cat /etc/redhat-release)" in
        # Older CentOS releases didn't have a /etc/centos-release file
        "CentOS release "*)
            printf "CentOS"
            ;;
        "CentOS Linux release "*)
            printf "CentOS Linux"
            ;;
        "Fedora release "*)
            printf "Fedora"
            ;;
        # Fallback to uname
        *)
            printf "%s" "$(uname -s)"
            ;;
        esac
    elif [ -f /etc/debian_version ]; then
        # Older Debian/Ubuntu/etc.
        printf "Debian"
    else
        # Fall back to uname, e.g. "Linux <version>", also works for BSD, etc.
        printf "%s" "$(uname -s)"
    fi
}

get_os() {
	my_uname="$(uname -s)"
	case "${my_uname}" in
		"Darwin"|"darwin")
			printf "%s" "macOS"
			;;
		MSYS_*|MINGW*)
			printf "%s" "Windows"
			;;
		*)
			printf "%s" "${my_uname}"
		   ;;
	esac
	unset my_uname
}

get_arch() {
    myarch=$(uname -m)

    case "${myarch}" in
    x86_64|amd64)
        printf "%s" "X64"
        ;;
    i*86)
        printf "%s" "X86"
        ;;
    aarch64|arm64)
        printf "%s" "ARM64"
        ;;
	armv7*|*armv8l*)
        printf "%s" "ARM"
        ;;
    *)
		printf "%s" "${myarch}"
		;;
    esac

    unset myarch
}
