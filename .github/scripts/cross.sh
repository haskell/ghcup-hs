#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

. "${SCRIPT_DIR}/common.sh"

usage() {
    (>&2 echo "cross.sh
GHCup cross integration test script

USAGE:
    cross.sh [FLAGS]

FLAGS:
    -h, --help              Prints help information
	-v, --verbose           Verbosity (e.g. for CI)
    --ghcup-binary          Path to the ghcup binary (the one we want to test)
	--cross                 The cross target (required)
    --os                    The operating system (Linux/Windows/macOS/FreeBSD/OpenBSD)
    --arch                  The architecture (X86/X64/ARM/ARM64)
    --distro                The linux distribution, if any (Alpine/Debian/...)
    --ghcup-dir             The base directory of ghcup (this will be destroyed during the test
                            and should not contain the ghcup binary)
    --project-dir           Project directory
    --ghc-version           The main GHC version to test installation of
    --ghc-target-version    The target GHC version to test installation of
    --cabal-version         The main cabal version to test installation of
    --json-version          The json version of the metadata
	--patch                 A source patch to be applied (can be specified multiple times)
")
    exit 1
}

PATCHES=( )

while [ $# -gt 0 ] ; do
    case $1 in
    -h|--help)
        usage;;
    --v|--verbose)
		VERBOSE=true
        shift 1;;
    --cross)
		CROSS=$2
        shift 2;;
    --ghcup-binary)
		GHCUP_BIN=$2
        shift 2;;
    --os)
		OS=$2
        shift 2;;
    --arch)
		ARCH=$2
        shift 2;;
    --distro)
		DISTRO=$2
        shift 2;;
    --ghcup-dir)
		GHCUP_INSTALL_BASE_PREFIX=$2
        shift 2;;
    --project-dir)
		PROJECT_DIR=$2
        shift 2;;
	--ghc-version)
		GHC_VER=$2
        shift 2;;
	--ghc-target-version)
		GHC_TARGET_VERSION=$2
        shift 2;;
	--cabal-version)
		CABAL_VER=$2
        shift 2;;
	--json-version)
		JSON_VERSION=$2
        shift 2;;
	--patch)
		PATCHES+=( "$2" )
        shift 2;;
	*)
		echo "Unknown option $1"
		exit 1
		;;
	esac
done

# defaults
[ -z "${VERBOSE}" ] && VERBOSE=false
[ -z "${GHCUP_BIN}" ] && GHCUP_BIN=$(cabal list-bin ghcup:exe:ghcup)
[ -z "${OS}" ] && OS=$(get_os)
[ -z "${ARCH}" ] && ARCH=$(get_arch)
[ -z "${DISTRO}" ] && DISTRO=$(get_distro)
[ -z "${GHCUP_INSTALL_BASE_PREFIX}" ] && GHCUP_INSTALL_BASE_PREFIX=$(mktempdir)
[ -z "${PROJECT_DIR}" ] && PROJECT_DIR=$(pwd)
[ -z "${GHC_VER}" ] && GHC_VER=9.6.7
[ -z "${GHC_TARGET_VERSION}" ] && GHC_TARGET_VERSION=8.10.7
[ -z "${CABAL_VER}" ] && CABAL_VER=3.14.2.0
[ -z "${JSON_VERSION}" ] && JSON_VERSION=0.0.9
[ -z "${WRAPPER}" ] && WRAPPER=run

export GHCUP_INSTALL_BASE_PREFIX

if [ "${OS}" = "Windows" ] ; then
	ext=".exe"
	GHCUP_DIR=${GHCUP_INSTALL_BASE_PREFIX}/ghcup
else
	ext=''
	GHCUP_DIR=${GHCUP_INSTALL_BASE_PREFIX}/.ghcup
fi

GHCUP_BINDIR=${GHCUP_DIR}/bin

export PATH="${GHCUP_BINDIR}:$PATH"

echo "===== Test config ====="
echo "GHCUP_BIN:          ${GHCUP_BIN}"
echo "OS:                 ${OS}"
echo "ARCH:               ${ARCH}"
echo "DISTRO:             ${DISTRO}"
echo "GHCUP_DIR:          ${GHCUP_DIR}"
echo "PROJECT_DIR:        ${PROJECT_DIR}"
echo "GHC_VER:            ${GHC_VER}"
echo "CROSS:              ${CROSS}"
echo "GHC_TARGET_VERSION: ${GHC_TARGET_VERSION}"
echo "CABAL_VER:          ${CABAL_VER}"
echo "JSON_VERSION:       ${JSON_VERSION}"
echo "WRAPPER:            ${WRAPPER}"
echo "BUILD_CONF_ARGS:    ${BUILD_CONF_ARGS}"
echo "PATCHES:            ${PATCHES[@]}"
echo "======================="


set -eux

if ${VERBOSE} ; then
    echo "GHCUP_INSTALL_BASE_PREFIX: ${GHCUP_INSTALL_BASE_PREFIX}"
	env
	git_describe
fi


chmod +x "$GHCUP_BIN"
"$GHCUP_BIN" --version
eghcup --version
sha_sum "$GHCUP_BIN"
sha_sum "$(raw_eghcup --offline whereis ghcup)"


### cross build

eghcup --numeric-version

eghcup install ghc "${GHC_VER}"
eghcup set ghc "${GHC_VER}"
eghcup install cabal "${CABAL_VER}"

cabal --version

eghcup debug-info

ecabal update

"${WRAPPER}" "$GHCUP_BIN" -c -s "file://$PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml" --verbosity=2 --keep=errors \
	compile ghc \
	$(if [ -n "${HADRIAN_FLAVOUR}" ] ; then printf "%s" "--flavour=${HADRIAN_FLAVOUR}" ; else true ; fi) \
	-j "$(nproc)" \
	-v "${GHC_TARGET_VERSION}" \
	-b "${GHC_VER}" \
	-x "${CROSS}" \
	$( for p in ${PATCHES[@]} ; do echo "--patch ${p}" ; done ) \
	-- \
	$(if [ -n "${BUILD_CONF_ARGS}" ] ; then printf "%s" "${BUILD_CONF_ARGS}" ; else true ; fi)

eghcup --verbosity=2 set ghc "${CROSS}-${GHC_TARGET_VERSION}"

find "${GHCUP_DIR}"/db
cat "${GHCUP_DIR}"/db/ghc/${CROSS}-${GHC_TARGET_VERSION}.spec || true
cat "${GHCUP_DIR}"/db/ghc/${CROSS}.set || true

[ "$($(eghcup whereis ghc "${CROSS}-${GHC_TARGET_VERSION}") --numeric-version)" = "${GHC_TARGET_VERSION}" ]


# test that doing fishy symlinks into GHCup dir doesn't cause weird stuff on 'ghcup nuke'
mkdir no_nuke/
mkdir no_nuke/bar
echo 'foo' > no_nuke/file
echo 'bar' > no_nuke/bar/file
ln -s "$PROJECT_DIR"/no_nuke/ "${GHCUP_DIR}"/cache/no_nuke
ln -s "$PROJECT_DIR"/no_nuke/ "${GHCUP_DIR}"/logs/no_nuke

# nuke
eghcup --verbosity=2 nuke
[ ! -e "${GHCUP_DIR}" ]

# make sure nuke doesn't resolve symlinks
[ -e "$PROJECT_DIR"/no_nuke/file ]
[ -e "$PROJECT_DIR"/no_nuke/bar/file ]

