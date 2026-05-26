#!/bin/sh


ver=$1

if [ -z "$ver" ] ; then
	(>&2 echo "No version argument given!")
	exit 1
fi

set -eu

sed -E -i -e \
	"s/ghver=\"[0-9]+(\\.[0-9]+)*\"/ghver=\"${ver}\"/" \
	scripts/bootstrap/bootstrap-haskell

