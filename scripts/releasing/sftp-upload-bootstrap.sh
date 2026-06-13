#!/bin/bash

url=$1
shift 1

die() {
    (>&2 printf "%s\\n" "$1")
    exit 2
}

[ -z $url ] && die "no url set"

cd scripts/bootstrap

sftp "$@" $url <<EOF
cd ghcup
cd sh

put bootstrap-haskell
put bootstrap-haskell.ps1
EOF

