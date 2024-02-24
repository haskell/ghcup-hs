#!/usr/bin/env bash
set -x
set -eo pipefail

. .github/workflows/common.sh

export GHCUP_INSTALL_BASE_PREFIX=$RUNNER_TEMP/foobarbaz

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

source "$GHCUP_INSTALL_BASE_PREFIX"/.ghcup/env || source "$HOME/.bashrc"

ghcup --version
which ghcup | grep foobarbaz

ghcup_fun() {
	ghcup -v --url-source="file:$METADATA_FILE" "$@"
}

case $TOOL in
	ghcup)
		ghcup_fun upgrade --force
		;;
	*) ghcup_fun install "$TOOL" --set "$VERSION"
		;;
esac

mkdir -p /tmp/install-bindist-ci
cp "$METADATA_FILE" /tmp/install-bindist-ci/
cd /tmp/install-bindist-ci

trap 'rm -rf -- /tmp/install-bindist-ci' EXIT

cat <<EOF > main.hs
{- cabal:
build-depends: base
-}

main = print $ 1 + 1
EOF

case $TOOL in
	ghcup)
		ghcup_fun list
		;;
	hls)
		ghcup_fun install cabal latest
		ghcup_fun install ghc --set recommended
		cabal update

		test_package="bytestring-0.11.1.0"
		test_module="Data/ByteString.hs"

		create_cradle() {
			echo "cradle:" > hie.yaml
			echo "  cabal:" >> hie.yaml
		}

		enter_test_package() {
			local tmp_dir
			tmp_dir=$(mktempdir)
			cd "$tmp_dir"
			cabal unpack "${test_package}"
			cd "${test_package}"
			cp "/tmp/install-bindist-ci/${METADATA_FILE}" .
		}

		# For all HLS GHC versions and the wrapper, run 'typecheck'
		# over the $test_module
		test_all_hls() {
			local bin
			local bin_noexe
			local bindir
			local hls
			bindir=$1

			for hls in "${bindir}/"haskell-language-server-* ; do
				bin=${hls##*/}
				bin_noexe=${bin/.exe/}
				if ! [[ "${bin_noexe}" =~ "haskell-language-server-wrapper" ]] && ! [[ "${bin_noexe}" =~ "~" ]] && ! [[ "${bin_noexe}" =~ ".shim" ]] ; then
					if ghcup_fun install ghc --set "${bin_noexe/haskell-language-server-/}" ; then
						"${hls}" typecheck "${test_module}" || fail "failed to typecheck with HLS for GHC ${bin_noexe/haskell-language-server-/}"
					else
						fail "GHCup failed to install GHC ${bin_noexe/haskell-language-server-/}"
					fi
					ghcup_fun rm ghc "${bin_noexe/haskell-language-server-/}"
				fi
			done
			"$bindir/haskell-language-server-wrapper${ext}" typecheck "${test_module}" || fail "failed to typecheck with HLS wrapper"
		}

        enter_test_package
        create_cradle
		case "$(uname -s)" in
			MSYS_*|MINGW*)
				test_all_hls "$(dirname "$(which ghcup)")"
				;;
			*)
				test_all_hls "$(ghcup whereis bindir)"
			   ;;
	    esac
		;;
    ghc)
		ghc --version
		ghc --info
		ghc -prof main.hs
		[[ $(./main +RTS -s) -eq 2 ]]
		ghcup_fun install cabal recommended
		cabal --version
		cabal update
		cabal install --lib --package-env=. clock
        # https://github.com/haskell/ghcup-hs/issues/966
		cabal install --lib --package-env=. hashable
		[[ -e "$(ghcup whereis --directory ghc "$VERSION")/../share/man/man1/ghc.1" ]]
		;;
    cabal)
		ghcup_fun install ghc --set "$(ghcup_fun list -t ghc -r -c available | tail -1 | awk '{ print $2 }')"
		cabal --version
		cabal update
		[[ $(cabal --verbose=0 run --enable-profiling ./main.hs -- +RTS -s) -eq 2 ]]
		;;
    *)
		$TOOL --version
		;;
esac
