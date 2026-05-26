#!/bin/sh


ver=$1

if [ -z "$ver" ] ; then
	(>&2 echo "No version argument given!")
	exit 1
fi

set -eu

sed -E -i -e \
	"s/^([[:blank:]]*)version:([[:blank:]]+)[0-9]+(\\.[0-9]+)*/\1version:\2${ver}/" \
	ghcup.cabal
sed -E -i -e \
	"s/^([[:blank:]]*)GHCUP_VER:([[:blank:]]+)[0-9]+(\\.[0-9]+)*/\1GHCUP_VER:\2${ver}/" \
	.github/workflows/release.yaml
sed -E -i -e \
	"s/^([[:blank:]]*)GHCUP_VER:([[:blank:]]+)[0-9]+(\\.[0-9]+)*/\1GHCUP_VER:\2${ver}/" \
	.github/workflows/cross.yaml

(>&2 echo "After releasing, make sure to run:
  ./scripts/releasing/bump-bootstrap-ver.sh ${ver}")

