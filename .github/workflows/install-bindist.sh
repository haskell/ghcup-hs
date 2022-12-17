#!/usr/bin/env bash
set -x
set -eo pipefail

export GHCUP_INSTALL_BASE_PREFIX=$RUNNER_TEMP/foobarbaz

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

source $GHCUP_INSTALL_BASE_PREFIX/.ghcup/env || source ~/.bashrc

ghcup --version
which ghcup | grep foobarbaz

# oh no
if [ "${TOOL}" = "hls" ] ; then
	ghcup -v --url-source=file:$METADATA_FILE install ghc --set 9.2.4
fi

ghcup -v --url-source=file:$METADATA_FILE install $TOOL --set $VERSION

mkdir /tmp/install-bindist-ci
cd /tmp/install-bindist-ci

cat <<EOF > main.hs
{- cabal:
build-depends: base
-}

main = print $ 1 + 1
EOF

case $TOOL in
	hls)
		haskell-language-server-wrapper --version
		haskell-language-server-wrapper typecheck main.hs
		;;
    ghc)
		ghc --version
		ghc --info
		ghc -prof main.hs
		[[ $(./main +RTS -s) -eq 2 ]]
		;;
    cabal)
		cabal --version
		cabal update
		[[ $(cabal --verbose=0 run --enable-profiling ./main.hs -- +RTS -s) -eq 2 ]]
		;;
    *)
		$TOOL --version
		;;
esac
