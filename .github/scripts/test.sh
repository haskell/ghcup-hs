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
    --ghcup-binary          Path to the ghcup binary (the one we want to test)
    --test-binary           Path to the test binary
    --test-optparse-binary  Path to the optparse test binary
    --os                    The operating system (Linux/Windows/macOS/FreeBSD/OpenBSD)
    --arch                  The architecture (X86/X64/ARM/ARM64)
    --distro                The linux distribution, if any (Alpine/Debian/...)
    --ghcup-dir             The base directory of ghcup (this will be destroyed during the test
                            and should not contain the ghcup binary)
    --project-dir           Project directory
    --ghc-version           The main GHC version to test installation of
    --cabal-version         The main cabal version to test installation of
    --json-version          The json version of the metadata
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
    --ghcup-binary)
		GHCUP_BIN=$2
        shift 2;;
    --test-binary)
		TEST_BIN=$2
        shift 2;;
    --test-optparse-binary)
		TEST_OPTPARSE_BIN=$2
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
	--cabal-version)
		CABAL_VER=$2
        shift 2;;
	--json-version)
		JSON_VERSION=$2
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
[ -z "${TEST_BIN}"  ] && TEST_BIN=$(cabal list-bin ghcup:test:ghcup-test)
[ -z "${TEST_OPTPARSE_BIN}" ] && TEST_OPTPARSE_BIN=$(cabal list-bin ghcup:test:ghcup-optparse-test)
[ -z "${OS}" ] && OS=$(get_os)
[ -z "${ARCH}" ] && ARCH=$(get_arch)
[ -z "${DISTRO}" ] && DISTRO=$(get_distro)
[ -z "${GHCUP_INSTALL_BASE_PREFIX}" ] && GHCUP_INSTALL_BASE_PREFIX=$(mktempdir)
[ -z "${PROJECT_DIR}" ] && PROJECT_DIR=$(pwd)
[ -z "${GHC_VER}" ] && GHC_VER=9.6.7
[ -z "${CABAL_VER}" ] && CABAL_VER=3.14.2.0
[ -z "${JSON_VERSION}" ] && JSON_VERSION=0.1.0

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
echo "GHCUP_BIN:         ${GHCUP_BIN}"
echo "TEST_BIN:          ${TEST_BIN}"
echo "TEST_OPTPARSE_BIN: ${TEST_OPTPARSE_BIN}"
echo "OS:                ${OS}"
echo "ARCH:              ${ARCH}"
echo "DISTRO:            ${DISTRO}"
echo "GHCUP_DIR:         ${GHCUP_DIR}"
echo "PROJECT_DIR:       ${PROJECT_DIR}"
echo "GHC_VER:           ${GHC_VER}"
echo "CABAL_VER:         ${CABAL_VER}"
echo "JSON_VERSION:      ${JSON_VERSION}"
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

### Haskell test suite

chmod +x "$TEST_BIN"
chmod +x "$TEST_OPTPARSE_BIN"
"${TEST_BIN}"
"${TEST_OPTPARSE_BIN}"

if [ "${OS}" = "OpenBSD" ] ; then
	exit 0
fi

### manual cli based testing

eghcup --numeric-version

# test PATH on windows wrt msys2
# https://github.com/haskell/ghcup-hs/pull/992/checks
if [ "${OS}" = "Windows" ] ; then
	eghcup run -m -- sh -c 'echo $PATH' | sed 's/:/\n/' | grep '^/clang64/bin$'
fi

eghcup install ghc "${GHC_VER}"
eghcup set ghc "${GHC_VER}"
eghcup unset ghc
ls -lah "$(eghcup whereis -d ghc "${GHC_VER}")"
[ "$($(eghcup whereis ghc "${GHC_VER}") --numeric-version)" = "${GHC_VER}" ]
[ "$(eghcup run -q --ghc "${GHC_VER}" -- ghc --numeric-version)" = "${GHC_VER}" ]
[ "$(ghcup run -q --ghc "${GHC_VER}" -- ghc -e 'Control.Monad.join (Control.Monad.fmap System.IO.putStr System.Environment.getExecutablePath)')" = "$($(ghcup whereis ghc "${GHC_VER}") -e 'Control.Monad.join (Control.Monad.fmap System.IO.putStr System.Environment.getExecutablePath)')" ]
eghcup set ghc "${GHC_VER}"
eghcup install cabal "${CABAL_VER}"
[ "$($(eghcup whereis cabal "${CABAL_VER}") --numeric-version)" = "${CABAL_VER}" ]
eghcup unset cabal
"$GHCUP_BINDIR"/cabal --version && exit 1 || echo yes

# make sure no cabal is set when running 'ghcup run' to check that PATH propagages properly
# https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/375
[ "$(eghcup run -q --cabal "${CABAL_VER}" -- cabal --numeric-version)" = "${CABAL_VER}" ]
eghcup set cabal "${CABAL_VER}"

[ "$($(eghcup whereis cabal "${CABAL_VER}") --numeric-version)" = "${CABAL_VER}" ]


run_tmp_dir=$(mktempdir)

if [ "${OS}" != "FreeBSD" ] ; then
	if [ "${ARCH}" = "X64" ] && [ "${DISTRO}" != "Alpine" ] ; then
		eghcup run --ghc 8.10.7 --cabal 3.4.1.0 --hls 1.6.1.0 --stack 2.7.3 --install --bindir "${run_tmp_dir}/.bin"
		if [ "${OS}" = "Windows" ] ; then
			cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup-run.files.windows" | sort > expected.txt
		elif [ "${DISTRO}" = "Alpine" ] ; then
			cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup-run.files.alpine" | sort > expected.txt
		else
			cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup-run.files" | sort > expected.txt
		fi
		(cd "${run_tmp_dir}/.bin" && find . | sort) > actual.txt
		diff --strip-trailing-cr -w -u actual.txt expected.txt
		rm actual.txt expected.txt
	fi
fi

cabal --version

eghcup debug-info

# also test etags
eghcup list
eghcup list -t ghc
eghcup list -t cabal

ghc_ver=$(ghc --numeric-version)
ghc --version
"ghc-${ghc_ver}" --version
if [ "${OS}" != "Windows" ] ; then
		ghci --version
		"ghci-${ghc_ver}" --version
fi


if [ "${OS}" = "macOS" ] && [ "${ARCH}" = "ARM64" ] ; then
	# missing bindists
	echo
elif [ "${OS}" = "FreeBSD" ] ; then
	# not enough space
	echo
elif [ "${OS}" = "Linux" ] && [ "${ARCH}" = "ARM64" ] && [ "${DISTRO}" = "Alpine" ]; then
	# missing bindists
	echo
else
	# test installing new ghc doesn't mess with currently set GHC
	# https://gitlab.haskell.org/haskell/ghcup-hs/issues/7
	if [ "${OS}" = "Linux" ] ; then
		eghcup --downloader=wget prefetch ghc 8.10.3
		eghcup --offline install ghc 8.10.3
		if [ "${ARCH}" = "X64" ] ; then
		    if [ "${DISTRO}" = "Alpine" ] ; then
				(cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghc-8.10.3-linux.alpine.files" | sort) > expected.txt
			elif [[ "${DISTRO}" =~ "openSUSE" ]] ; then
				(cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghc-8.10.3-linux.opensuse.files" | sort) > expected.txt
			else
				(cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghc-8.10.3-linux.files" | sort) > expected.txt
			fi
			(cd "${GHCUP_DIR}/ghc/8.10.3/" && find . | sort) > actual.txt
			# ignore docs
		    sed -i '/share\/doc/d' actual.txt
		    sed -i '/share\/doc/d' expected.txt
			diff --strip-trailing-cr -w -u actual.txt expected.txt
			rm actual.txt expected.txt
		fi
	elif [ "${OS}" = "Windows" ] ; then
		eghcup prefetch ghc 8.10.3
		eghcup --offline install ghc 8.10.3
		(cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghc-8.10.3-windows.files" | sort) > expected.txt
		(cd "${GHCUP_DIR}/ghc/8.10.3/" && find . | sort) > actual.txt
		diff --strip-trailing-cr -w -u actual.txt expected.txt
		rm actual.txt expected.txt
	else
		eghcup prefetch ghc 8.10.3
		eghcup --offline install ghc 8.10.3
	fi
	[ "$(ghc --numeric-version)" = "${ghc_ver}" ]
	eghcup --offline set ghc 8.10.3
	eghcup set ghc 8.10.3
	[ "$(ghc --numeric-version)" = "8.10.3" ]
	eghcup set ghc "${GHC_VER}"
	[ "$(ghc --numeric-version)" = "${ghc_ver}" ]
	eghcup unset ghc
    "$GHCUP_BINDIR"/ghc --numeric-version && exit 1 || echo yes
	eghcup set ghc "${GHC_VER}"
	eghcup --offline rm ghc 8.10.3
	[ "$(ghc --numeric-version)" = "${ghc_ver}" ]


	ls -lah "$GHCUP_BINDIR"

	if [ "${OS}" = "macOS" ] ; then
		eghcup install hls
		$(eghcup whereis hls) --version

		eghcup install stack
		$(eghcup whereis stack) --version
	elif [ "${OS}" = "Linux" ] ; then
		if [ "${ARCH}" = "X64" ] && [ "${DISTRO}" != "Alpine" ] ; then
			eghcup install hls
			haskell-language-server-wrapper --version
			eghcup unset hls
			"$GHCUP_BINDIR"/haskell-language-server-wrapper --version && exit 1 || echo yes

			eghcup install stack
			stack --version
			eghcup unset stack
			"$GHCUP_BINDIR"/stack --version && exit 1 || echo yes
		fi
	fi
fi



# check that lazy loading works for 'whereis'
echo '**' > "${GHCUP_DIR}/cache/ghcup-broken-yaml.yaml"
if [ "${OS}" = "Windows" ] ; then
	raw_eghcup -s "file:$(cygpath -m "${GHCUP_DIR}/cache/ghcup-broken-yaml.yaml")" whereis ghc "$(ghc --numeric-version)"
else
	raw_eghcup -s "file://${GHCUP_DIR}/cache/ghcup-broken-yaml.yaml" whereis ghc "$(ghc --numeric-version)"
fi

eghcup rm ghc "$(ghc --numeric-version)"

# https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/116
if [ "${OS}" = "Linux" ] ; then
	if [ "${ARCH}" = "X64" ] ; then
		eghcup install cabal -u https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal/3.7.0.0-pre20220407/cabal-install-3.7-x86_64-linux-alpine.tar.xz 3.4.0.0-rc4
		eghcup rm cabal 3.4.0.0-rc4
	fi
fi

eghcup gc -c

# test etags
rm -f "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml"
raw_eghcup -s "https://www.haskell.org/ghcup/data/ghcup-${JSON_VERSION}.yaml" list
# snapshot yaml and etags file
etag=$(cat "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml.etags")
sha=$(sha_sum "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml")
# invalidate access time timer, which is 5minutes, so we re-download
touch -a -m -t '199901010101' "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml"
# redownload same file with some newlines added
raw_eghcup -s https://raw.githubusercontent.com/haskell/ghcup-metadata/exp/ghcup-${JSON_VERSION}.yaml list
# snapshot new yaml and etags file
etag2=$(cat "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml.etags")
sha2=$(sha_sum "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml")
# compare
[ "${etag}" != "${etag2}" ]
[ "${sha}" != "${sha2}" ]
# invalidate access time timer, which is 5minutes, but don't expect a re-download
touch -a -m -t '199901010101' "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml"
# this time, we expect the same hash and etag
raw_eghcup -s https://raw.githubusercontent.com/haskell/ghcup-metadata/exp/ghcup-${JSON_VERSION}.yaml list
etag3=$(cat "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml.etags")
sha3=$(sha_sum "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml")
[ "${etag2}" = "${etag3}" ]
[ "${sha2}" = "${sha3}" ]

isolated_tmp_dir=$(mktempdir)

# test isolated installs
if [ "${DISTRO}" != "Alpine" ] ; then
	eghcup install ghc -i "${isolated_tmp_dir}/isolated" 8.10.5
	[ "$("${isolated_tmp_dir}"/isolated/bin/ghc --numeric-version)" = "8.10.5" ]
	! eghcup install ghc -i "${isolated_tmp_dir}/isolated" 8.10.5
	if [ "${ARCH}" = "X64" ] ; then
		if [ "${OS}" = "Linux" ] || [ "${OS}" = "Windows" ] ; then
			eghcup install cabal --force -i "${isolated_tmp_dir}/isolated" 3.4.0.0
			[ "$("${isolated_tmp_dir}"/isolated/cabal --numeric-version)" = "3.4.0.0" ]
			eghcup install stack --force -i "${isolated_tmp_dir}/isolated" 2.7.3
			[ "$("${isolated_tmp_dir}"/isolated/stack --numeric-version)" = "2.7.3" ]
			eghcup install hls --force -i "${isolated_tmp_dir}/isolated" 1.3.0
			[ "$("${isolated_tmp_dir}"/isolated/haskell-language-server-wrapper --numeric-version)" = "1.3.0" ] ||
				[ "$("${isolated_tmp_dir}"/isolated/haskell-language-server-wrapper --numeric-version)" = "1.3.0.0" ]

			# test that isolated installs don't clean up target directory
			cat <<EOF > "${GHCUP_BINDIR}/gmake"
#!/bin/bash
exit 1
EOF
			chmod +x "${GHCUP_BINDIR}/gmake"
			mkdir "${isolated_tmp_dir}"/isolated_tainted/
			touch "${isolated_tmp_dir}"/isolated_tainted/lol

			! eghcup install ghc -i "${isolated_tmp_dir}/isolated_tainted" 8.10.5 --force
			[ -e "${isolated_tmp_dir}/isolated_tainted/lol" ]
			rm "${GHCUP_BINDIR}/gmake"
		fi
	fi
fi

eghcup upgrade
eghcup upgrade -f

# restore old ghcup, because we want to test nuke
cp "${GHCUP_BIN}" "$GHCUP_BINDIR/ghcup${ext}"
chmod +x "$GHCUP_BINDIR/ghcup${ext}"

nuke_tmp_dir=$(mktempdir)

# test that doing fishy symlinks into GHCup dir doesn't cause weird stuff on 'ghcup nuke'
mkdir "$nuke_tmp_dir"/no_nuke/
mkdir "$nuke_tmp_dir"/no_nuke/bar
echo 'foo' > "$nuke_tmp_dir"/no_nuke/file
echo 'bar' > "$nuke_tmp_dir"/no_nuke/bar/file

ln -s "$nuke_tmp_dir"/no_nuke/ "${GHCUP_DIR}"/cache/no_nuke
ln -s "$nuke_tmp_dir"/no_nuke/ "${GHCUP_DIR}"/logs/no_nuke

# nuke
eghcup nuke
[ ! -e "${GHCUP_DIR}" ]

# make sure nuke doesn't resolve symlinks
[ -e "$nuke_tmp_dir"/no_nuke/file ]
[ -e "$nuke_tmp_dir"/no_nuke/bar/file ]

echo
echo "You may want to remove the following dirs:"
echo "  $GHCUP_INSTALL_BASE_PREFIX"
echo "  $run_tmp_dir"
echo "  $isolated_tmp_dir"
echo "  $nuke_tmp_dir"

