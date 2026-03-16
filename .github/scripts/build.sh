#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

. "${SCRIPT_DIR}/common.sh"

usage() {
    (>&2 echo "test.sh
GHCup integration test script

USAGE:
    test.sh [FLAGS]

FLAGS:
    -h, --help              Prints help information
    -v, --verbose           Verbosity (e.g. for CI)
    --ghc                   Path to the GHC compiler (default: ghc)
    --os                    The operating system (Linux/Windows/macOS/FreeBSD/OpenBSD)
    --check-linking         Whether to check linking (default: false)
    --strip                 Whether to strip binary (default: false)
    --no-cabal-update       Don't update cabal db (default: false)
    --use-cabal-cache       Use cabal-cache to pull dependencies (default: false)
    --output-directory      Where to put the built artifacts (default: out/)
")
    exit 1
}

while [ $# -gt 0 ] ; do
    case $1 in
    -h|--help)
        usage;;
    --v|--verbose)
        VERBOSE=true
        shift 1;;
    --ghc)
        GHC=$2
        shift 2;;
    --os)
        OS=$2
        shift 2;;
    --check-linking)
        CHECK_LINKING=true
        shift 1;;
    --strip)
        STRIP=true
        shift 1;;
    --no-cabal-update)
        NO_CABAL_UPDATE=true
        shift 1;;
    --use-cabal-cache)
        USE_CABAL_CACHE=true
        shift 1;;
    --output-directory)
        OUTPUT_DIRECTORY=$2
        shift 2;;
    --)
        shift 1
        break
        ;;
    *)
		echo "Unknown option $1"
		exit 1
        ;;
    esac
done

# defaults
[ -z "${VERBOSE}" ] && VERBOSE=false
[ -z "${GHC}" ] && GHC=ghc
[ -z "${OS}" ] && OS=$(get_os)
[ -z "${CHECK_LINKING}" ] && CHECK_LINKING=false
[ -z "${STRIP}" ] && STRIP=false
[ -z "${NO_CABAL_UPDATE}" ] && NO_CABAL_UPDATE=false
[ -z "${USE_CABAL_CACHE}" ] && USE_CABAL_CACHE=false
[ -z "${OUTPUT_DIRECTORY}" ] && OUTPUT_DIRECTORY=out
[ -z "${ARTIFACT}" ] && ARTIFACT=ghcup

if [ "${OS}" = "Windows" ] ; then
    ext=".exe"
else
    ext=''
fi

echo "===== Build config ====="
echo "OS:                ${OS}"
echo "GHC:               ${GHC}"
echo "CHECK_LINKING:     ${CHECK_LINKING}"
echo "STRIP:             ${STRIP}"
echo "NO_CABAL_UPDATE:   ${NO_CABAL_UPDATE}"
echo "USE_CABAL_CACHE:   ${USE_CABAL_CACHE}"
echo "OUTPUT_DIRECTORY:  ${OUTPUT_DIRECTORY}"
echo "ARTIFACT:          ${ARTIFACT}"
echo "========================"

set -eux

if ${VERBOSE} ; then
    env
    git_describe
    ${GHC} --numeric-version
    cabal --numeric-version
fi

if ${USE_CABAL_CACHE} ; then
    # ensure cabal-cache
    download_cabal_cache "$HOME/.local/bin/cabal-cache"
fi

# build
if ! ${NO_CABAL_UPDATE} ; then
    ecabal update
fi

if ${USE_CABAL_CACHE} ; then
    build_with_cache --project-file=cabal.project.release -w "${GHC}" --enable-tests "$@"
else
    ecabal build --project-file=cabal.project.release -w "${GHC}" --enable-tests "$@"
fi

# set up artifacts
mkdir -p "${OUTPUT_DIRECTORY}"
binary=$(cabal --project-file=cabal.project.release list-bin ghcup)
binary_test=$(cabal --project-file=cabal.project.release list-bin ghcup-test)
binary_opttest=$(cabal --project-file=cabal.project.release list-bin ghcup-optparse-test)
ver=$("${binary}" --numeric-version)

if ${STRIP} ; then
    strip_binary "${binary}"
fi

# linking info
if ${CHECK_LINKING} ; then
    if [ "${OS}" = "macOS" ] ; then
        otool -L "${binary}"
        otool -L "${binary}" | grep libyaml && { echo "undesired libyaml linking" ; exit 5 ; }
    else
        ldd "${binary}" || true
    fi
fi

cp "${binary}" "${OUTPUT_DIRECTORY}/${ARTIFACT}-${ver}${ext}"
cp "${binary_test}" "${OUTPUT_DIRECTORY}/test-${ARTIFACT}-${ver}${ext}"
cp "${binary_opttest}" "${OUTPUT_DIRECTORY}/test-optparse-${ARTIFACT}-${ver}${ext}"
cp ./dist-newstyle/cache/plan.json "${OUTPUT_DIRECTORY}/${ARTIFACT}.plan.json"

