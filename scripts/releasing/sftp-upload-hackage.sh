#!/bin/bash

ver=$1
shift 1

die() {
    (>&2 printf "%s\\n" "$1")
    exit 2
}

[ -z $ver ] && die "no version set"

set -eux

cabal haddock --haddock-for-hackage --enable-doc --haddock-options=--quickjump
cabal upload --publish -d dist-newstyle/ghcup-${ver}-docs.tar.gz
