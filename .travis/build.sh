#!/bin/sh

set -ex

mkdir -p ~/.ghcup/bin
curl -sSfL https://downloads.haskell.org/~ghcup/x86_64-apple-darwin-ghcup > ~/.ghcup/bin/ghcup
chmod +x ~/.ghcup/bin/ghcup

export PATH="$HOME/.ghcup/bin:$PATH"

ghcup install 8.10.4
ghcup install-cabal 3.4.0.0
ghcup set 8.10.4


## install ghcup

cabal update
cabal build --constraint="zlib +static" --constraint="lzma +static" -ftui
cp "$(cabal new-exec --verbose=0 --offline sh -- -c 'command -v ghcup')" .
strip ./ghcup
cp ghcup "./${ARTIFACT}"
