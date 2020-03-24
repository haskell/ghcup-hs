#!/bin/sh

set -ex

cd /app

cabal v2-update

cabal v2-install \
	--install-method=copy \
	--overwrite-policy=always \
	--installdir="/bin" \
	--ghc-options='-optl-static'

