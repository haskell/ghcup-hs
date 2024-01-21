#!/usr/bin/env bash

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

CI_PROJECT_DIR=$(pwd)


ecabal() {
	cabal "$@"
}

raw_eghcup() {
	if command -v sydbox 1>/dev/null ; then
        sydbox \
            -m core/sandbox/read:deny \
            -m core/sandbox/write:deny \
            -m core/sandbox/network:allow \
            -m allowlist/read+/usr/lib/os-release \
            -m "allowlist/read+${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/***" \
            -m "allowlist/write+${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/***" \
            -m "allowlist/read+${TMPDIR}/***" \
            -m "allowlist/write+${TMPDIR}/***" \
            -m "allowlist/read+/usr/lib/***" \
            -m 'allowlist/read+/etc/ld.so.cache' \
            -m "allowlist/read+/lib/***" \
            -m 'allowlist/read+/etc/ssl/openssl.cnf' \
            -m 'allowlist/read+/proc/sys/crypto/fips_enabled' \
            -m 'allowlist/read+/etc/nsswitch.conf' \
            -m 'allowlist/read+/etc/host.conf' \
            -m 'allowlist/read+/etc/resolv.conf' \
            -m 'allowlist/read+/etc/hosts' \
            -m 'allowlist/read+/etc/gai.conf' \
            -m 'allowlist/read+/etc/ssl/certs/ca-certificates.crt' \
            -m 'allowlist/read+/usr/share/zoneinfo/Etc/UTC' \
            -m 'allowlist/read+/dev/urandom' \
            -m 'core/violation/decision:killall' \
		    -- ghcup -v -c "$@"
	else
		ghcup -v -c "$@"
	fi
}

eghcup() {
	if [ "${OS}" = "WINDOWS" ] ; then
        sydbox \
            -m core/sandbox/read:deny \
            -m core/sandbox/write:deny \
            -m core/sandbox/network:allow \
            -m allowlist/read+/usr/lib/os-release \
            -m "allowlist/read+${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/***" \
            -m "allowlist/write+${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/***" \
            -m "allowlist/read+${TMPDIR}/***" \
            -m "allowlist/write+${TMPDIR}/***" \
            -m "allowlist/read+/usr/lib/***" \
            -m 'allowlist/read+/etc/ld.so.cache' \
            -m "allowlist/read+/lib/***" \
            -m 'allowlist/read+/etc/ssl/openssl.cnf' \
            -m 'allowlist/read+/proc/sys/crypto/fips_enabled' \
            -m 'allowlist/read+/etc/nsswitch.conf' \
            -m 'allowlist/read+/etc/host.conf' \
            -m 'allowlist/read+/etc/resolv.conf' \
            -m 'allowlist/read+/etc/hosts' \
            -m 'allowlist/read+/etc/gai.conf' \
            -m 'allowlist/read+/etc/ssl/certs/ca-certificates.crt' \
            -m 'allowlist/read+/usr/share/zoneinfo/Etc/UTC' \
            -m 'allowlist/read+/dev/urandom' \
            -m 'core/violation/decision:killall' \
    
		ghcup -v -c -s file:/$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml "$@"
	else
		if command -v sydbox 1>/dev/null ; then
			ghcup -v -c -s file://$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml "$@"
		else
			ghcup -v -c -s file://$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml "$@"
		fi
	fi
}

eghcup_offline() {
	if command -v sydbox 1>/dev/null ; then
        sydbox \
            -m core/sandbox/read:deny \
            -m core/sandbox/write:deny \
            -m core/sandbox/network:deny \
            -m allowlist/read+/usr/lib/os-release \
            -m "allowlist/read+${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/***" \
            -m "allowlist/write+${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/***" \
            -m "allowlist/read+${TMPDIR}/***" \
            -m "allowlist/write+${TMPDIR}/***" \
            -m "allowlist/read+/usr/lib/***" \
            -m 'allowlist/read+/etc/ld.so.cache' \
            -m "allowlist/read+/lib/***" \
            -m 'allowlist/read+/etc/ssl/openssl.cnf' \
            -m 'allowlist/read+/proc/sys/crypto/fips_enabled' \
            -m 'allowlist/read+/etc/nsswitch.conf' \
            -m 'allowlist/read+/etc/host.conf' \
            -m 'allowlist/read+/etc/resolv.conf' \
            -m 'allowlist/read+/etc/hosts' \
            -m 'allowlist/read+/etc/gai.conf' \
            -m 'allowlist/read+/etc/ssl/certs/ca-certificates.crt' \
            -m 'allowlist/read+/usr/share/zoneinfo/Etc/UTC' \
            -m 'allowlist/read+/dev/urandom' \
            -m 'core/violation/decision:killall' \
			-- ghcup -v --offline "$@"
	else
		ghcup -v --offline "$@"
	fi
}

if [ "${OS}" = "WINDOWS" ] ; then
	GHCUP_DIR="${GHCUP_INSTALL_BASE_PREFIX}"/ghcup
else
	GHCUP_DIR="${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup
fi

git describe --always

### build

rm -rf "${GHCUP_DIR}"/share

ecabal update

if [ "${OS}" = "DARWIN" ] ; then
	ecabal build -w ghc-${GHC_VERSION} -ftui
	ecabal test -w ghc-${GHC_VERSION} -ftui ghcup-test
	ecabal haddock -w ghc-${GHC_VERSION} -ftui
elif [ "${OS}" = "LINUX" ] ; then
	if [ "${ARCH}" = "32" ] ; then
		ecabal build -w ghc-${GHC_VERSION} -finternal-downloader -ftui
		ecabal test -w ghc-${GHC_VERSION} -finternal-downloader -ftui ghcup-test
		ecabal haddock -w ghc-${GHC_VERSION} -finternal-downloader -ftui
	else
		ecabal build -w ghc-${GHC_VERSION} -finternal-downloader -ftui
		ecabal test -w ghc-${GHC_VERSION} -finternal-downloader -ftui ghcup-test
		ecabal haddock -w ghc-${GHC_VERSION} -finternal-downloader -ftui

		if [ "${ARCH}" = "64" ] ; then
			# doctest
			curl -sL https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-docspec/cabal-docspec-0.0.0.20210228_p1.tar.bz2 > cabal-docspec.tar.bz2
			echo '3a10f6fec16dbd18efdd331b1cef5d2d342082da42f5b520726d1fa6a3990d12  cabal-docspec.tar.bz2' | sha256sum -c -
			tar -xjf cabal-docspec.tar.bz2 cabal-docspec
			mv cabal-docspec "$CI_PROJECT_DIR"/.local/bin/cabal-docspec
			rm -f cabal-docspec.tar.bz2
			chmod a+x "$CI_PROJECT_DIR"/.local/bin/cabal-docspec

			cabal-docspec -XCPP -XTypeSynonymInstances -XOverloadedStrings -XPackageImports --check-properties
		fi
	fi
elif [ "${OS}" = "FREEBSD" ] ; then
	ecabal build -w ghc-${GHC_VERSION} -finternal-downloader -ftui --constraint="zip +disable-zstd"
	ecabal test -w ghc-${GHC_VERSION} -finternal-downloader -ftui --constraint="zip +disable-zstd" ghcup-test
	ecabal haddock -w ghc-${GHC_VERSION} -finternal-downloader -ftui --constraint="zip +disable-zstd"
elif [ "${OS}" = "WINDOWS" ] ; then
	ecabal build -w ghc-${GHC_VERSION}
	ecabal test -w ghc-${GHC_VERSION} ghcup-test
	ecabal haddock -w ghc-${GHC_VERSION}
else
	ecabal build -w ghc-${GHC_VERSION} -finternal-downloader -ftui
	ecabal test -w ghc-${GHC_VERSION} -finternal-downloader -ftui ghcup-test
	ecabal haddock -w ghc-${GHC_VERSION} -finternal-downloader -ftui
fi


if [ "${OS}" = "WINDOWS" ] ; then
	ext=".exe"
else
	ext=''
fi
	cp "$(ecabal new-exec -w ghc-${GHC_VERSION} --verbose=0 --offline sh -- -c 'command -v ghcup')" "$CI_PROJECT_DIR"/.local/bin/ghcup${ext}

### cleanup

rm -rf "${GHCUP_DIR}"

### manual cli based testing


eghcup --numeric-version

eghcup install ghc ${GHC_VERSION}
eghcup unset ghc ${GHC_VERSION}
ls -lah "$(eghcup whereis -d ghc ${GHC_VERSION})"
[ "`$(eghcup whereis ghc ${GHC_VERSION}) --numeric-version`" = "${GHC_VERSION}" ]
[ "`eghcup run --ghc ${GHC_VERSION} -- ghc --numeric-version`" = "${GHC_VERSION}" ]
[ "`ghcup run --ghc ${GHC_VERSION} -- ghc -e 'Control.Monad.join (Control.Monad.fmap System.IO.putStr System.Environment.getExecutablePath)'`" = "`$(ghcup whereis ghc ${GHC_VERSION}) -e 'Control.Monad.join (Control.Monad.fmap System.IO.putStr System.Environment.getExecutablePath)'`" ]
eghcup set ghc ${GHC_VERSION}
eghcup install cabal ${CABAL_VERSION}
[ "`$(eghcup whereis cabal ${CABAL_VERSION}) --numeric-version`" = "${CABAL_VERSION}" ]
eghcup unset cabal
"$GHCUP_BIN"/cabal --version && exit 1 || echo yes

# make sure no cabal is set when running 'ghcup run' to check that PATH propagages properly
# https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/375
[ "`eghcup run --cabal ${CABAL_VERSION} -- cabal --numeric-version`" = "${CABAL_VERSION}" ]
eghcup set cabal ${CABAL_VERSION}

[ "`$(eghcup whereis cabal ${CABAL_VERSION}) --numeric-version`" = "${CABAL_VERSION}" ]

if [ "${OS}" != "FREEBSD" ] ; then
	if [ "${ARCH}" = "64" ] ; then
		eghcup run --ghc 8.10.7 --cabal 3.4.1.0 --hls 1.6.1.0 --stack 2.7.3 --install --bindir "$(pwd)/.bin"
		if [ "${OS}" == "WINDOWS" ] ; then
			expected=$(cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup-run.files.windows" | sort)
		else
			expected=$(cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup-run.files" | sort)
		fi
		actual=$(cd ".bin" && find . | sort)
		[ "${actual}" = "${expected}" ]
		unset actual expected
		rm -rf .bin
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
ghc-${ghc_ver} --version
if [ "${OS}" != "WINDOWS" ] ; then
		ghci --version
		ghci-${ghc_ver} --version
fi


if [ "${OS}" = "DARWIN" ] && [ "${ARCH}" = "ARM64" ] ; then
	echo
else
	# test installing new ghc doesn't mess with currently set GHC
	# https://gitlab.haskell.org/haskell/ghcup-hs/issues/7
	if [ "${OS}" = "LINUX" ] ; then
		eghcup --downloader=wget prefetch ghc 8.10.3
		eghcup_offline install ghc 8.10.3
		if [ "${ARCH}" = "64" ] ; then
			expected=$(cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghc-8.10.3-linux.files" | sort)
			actual=$(cd "${GHCUP_DIR}/ghc/8.10.3/" && find . | sort)
			[ "${actual}" = "${expected}" ]
			unset actual expected
		fi
	elif [ "${OS}" = "WINDOWS" ] ; then
		eghcup prefetch ghc 8.10.3
		eghcup_offline install ghc 8.10.3
		expected=$(cat "$( cd "$(dirname "$0")" ; pwd -P )/../ghc-8.10.3-windows.files" | sort)
		actual=$(cd "${GHCUP_DIR}/ghc/8.10.3/" && find . | sort)
		[ "${actual}" = "${expected}" ]
		unset actual expected
	else
		eghcup prefetch ghc 8.10.3
		eghcup_offline install ghc 8.10.3
	fi
	[ "$(ghc --numeric-version)" = "${ghc_ver}" ]
	eghcup_offline set 8.10.3
	eghcup set 8.10.3
	[ "$(ghc --numeric-version)" = "8.10.3" ]
	eghcup set ${GHC_VERSION}
	[ "$(ghc --numeric-version)" = "${ghc_ver}" ]
	eghcup unset ghc
    "$GHCUP_BIN"/ghc --numeric-version && exit 1 || echo yes
	eghcup set ${GHC_VERSION}
	eghcup_offline rm 8.10.3
	[ "$(ghc --numeric-version)" = "${ghc_ver}" ]


	ls -lah "$GHCUP_BIN"

	if [ "${OS}" = "DARWIN" ] ; then
		eghcup install hls
		$(eghcup whereis hls) --version

		eghcup install stack
		$(eghcup whereis stack) --version
	elif [ "${OS}" = "LINUX" ] ; then
		if [ "${ARCH}" = "64" ] ; then
			eghcup install hls
			haskell-language-server-wrapper --version
			eghcup unset hls
			"$GHCUP_BIN"/haskell-language-server-wrapper --version && exit 1 || echo yes

			eghcup install stack
			stack --version
			eghcup unset stack
			"$GHCUP_BIN"/stack --version && exit 1 || echo yes
		fi
	fi
fi



# check that lazy loading works for 'whereis'
cp "$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml" "$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml.bak"
echo '**' > "$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml"
eghcup whereis ghc $(ghc --numeric-version)
mv -f "$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml.bak" "$CI_PROJECT_DIR/data/metadata/ghcup-${JSON_VERSION}.yaml"

eghcup rm $(ghc --numeric-version)

# https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/116
if [ "${OS}" = "LINUX" ] ; then
	if [ "${ARCH}" = "64" ] ; then
		eghcup install cabal -u https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal/3.7.0.0-pre20220407/cabal-install-3.7-x86_64-linux-alpine.tar.xz 3.4.0.0-rc4
		eghcup rm cabal 3.4.0.0-rc4
	fi
fi

eghcup gc -c

sha_sum() {
	if [ "${OS}" = "FREEBSD" ] ; then
		sha256 "$@"
	else
		sha256sum "$@"
	fi

}

# test etags
rm -f "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml"
raw_eghcup -s https://www.haskell.org/ghcup/data/ghcup-${JSON_VERSION}.yaml list
# snapshot yaml and etags file
etag=$(cat "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml.etags")
sha=$(sha_sum "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml")
# invalidate access time timer, which is 5minutes, so we re-download
touch -a -m -t '199901010101' "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml"
# redownload same file with some newlines added
raw_eghcup -s https://www.haskell.org/ghcup/exp/ghcup-${JSON_VERSION}.yaml list
# snapshot new yaml and etags file
etag2=$(cat "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml.etags")
sha2=$(sha_sum "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml")
# compare
[ "${etag}" != "${etag2}" ]
[ "${sha}" != "${sha2}" ]
# invalidate access time timer, which is 5minutes, but don't expect a re-download
touch -a -m -t '199901010101' "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml"
# this time, we expect the same hash and etag
raw_eghcup -s https://www.haskell.org/ghcup/exp/ghcup-${JSON_VERSION}.yaml list
etag3=$(cat "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml.etags")
sha3=$(sha_sum "${GHCUP_DIR}/cache/ghcup-${JSON_VERSION}.yaml")
[ "${etag2}" = "${etag3}" ]
[ "${sha2}" = "${sha3}" ]

# test isolated installs
eghcup install ghc -i "$(pwd)/isolated" 8.10.5
[ "$(isolated/bin/ghc --numeric-version)" = "8.10.5" ]
! eghcup install ghc -i "$(pwd)/isolated" 8.10.5
if [ "${ARCH}" = "64" ] ; then
	if [ "${OS}" = "LINUX" ] || [ "${OS}" = "WINDOWS" ] ; then
		eghcup install cabal -i "$(pwd)/isolated" 3.4.0.0
		[ "$(isolated/cabal --numeric-version)" = "3.4.0.0" ]
		eghcup install stack -i "$(pwd)/isolated" 2.7.3
		[ "$(isolated/stack --numeric-version)" = "2.7.3" ]
		eghcup install hls -i "$(pwd)/isolated" 1.3.0
		[ "$(isolated/haskell-language-server-wrapper --numeric-version)" = "1.3.0" ] ||
			[ "$(isolated/haskell-language-server-wrapper --numeric-version)" = "1.3.0.0" ]

		# test that isolated installs don't clean up target directory
		cat <<EOF > "${GHCUP_BIN}/gmake"
#!/bin/bash
exit 1
EOF
		chmod +x "${GHCUP_BIN}/gmake"
		mkdir isolated_tainted/
		touch isolated_tainted/lol

		! eghcup install ghc -i "$(pwd)/isolated_tainted" 8.10.5 --force
		[ -e "$(pwd)/isolated_tainted/lol" ]
		rm "${GHCUP_BIN}/gmake"
	fi
fi

eghcup upgrade
eghcup upgrade -f

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


