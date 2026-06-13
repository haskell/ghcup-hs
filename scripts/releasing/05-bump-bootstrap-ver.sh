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

(>&2 echo "Now commit and push the changes.

Then run:
  ./scripts/releasing/06-sftp-symlink-artifacts.sh <dl-user>@<dl-url> ${ver} -P <port>
")
