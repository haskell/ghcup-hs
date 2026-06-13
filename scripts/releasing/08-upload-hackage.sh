#!/bin/bash

ver=$1
shift 1

die() {
    (>&2 printf "%s\\n" "$1")
    exit 2
}

[ -z $ver ] && die "no version set"

set -eux

if [ ! -e "dist-newstyle/sdist/ghcup-${ver}.tar.gz" ] ; then
	cabal sdist all
fi

if [ "$(curl -I -s -o /dev/null -w "%{http_code}" https://hackage.haskell.org/package/ghcup-${ver})" != "200" ] ; then
	cabal upload --publish dist-newstyle/sdist/ghcup-${ver}.tar.gz
fi

if [ ! -e "dist-newstyle/ghcup-${ver}-docs.tar.gz" ] ; then
	cabal haddock --haddock-for-hackage --enable-doc --haddock-options=--quickjump lib:ghcup
fi
cabal upload --publish -d dist-newstyle/ghcup-${ver}-docs.tar.gz
