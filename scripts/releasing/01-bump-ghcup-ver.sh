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

(>&2 echo "Now tag ghcup and push:
  git tag -sf v${ver}
  git push origin v${ver}")

(>&2 echo "After CI succeeds, run:
  ./scripts/releasing/02-pull_release_artifacts.sh v${ver} <your-email>")

