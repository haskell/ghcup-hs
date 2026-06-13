#!/bin/bash

url=$1
shift 1
ver=$1
shift 1
artifacts_dir=$1
shift 1

die() {
    (>&2 printf "%s\\n" "$1")
    exit 2
}

[ -z $url ] && die "no url set"
[ -z $ver ] && die "no version set"
[ -z "${artifacts_dir}" ] && die "artifacts_dir not set"
[ -e "${artifacts_dir}" ] || die "artifacts_dir \"${artifacts_dir}\" does not exist"

cd "${artifacts_dir}"

sftp "$@" $url <<EOF
cd ghcup

mkdir ${ver}
cd ${ver}
put SHA256SUMS
put SHA256SUMS.sig
put aarch64-apple-darwin-ghcup-${ver}
put aarch64-apple-darwin-ghcup-${ver}.sig
put aarch64-apple-darwin-ghcup.plan.json
put aarch64-linux-ghcup-${ver}
put aarch64-linux-ghcup-${ver}.sig
put aarch64-linux-ghcup.plan.json
put i386-linux-ghcup-${ver}
put i386-linux-ghcup-${ver}.sig
put i386-linux-ghcup.plan.json
put x86_64-apple-darwin-ghcup-${ver}
put x86_64-apple-darwin-ghcup-${ver}.sig
put x86_64-apple-darwin-ghcup.plan.json
put x86_64-portbld-freebsd-ghcup-${ver}
put x86_64-portbld-freebsd-ghcup-${ver}.sig
put x86_64-portbld-freebsd-ghcup.plan.json
put x86_64-openbsd-ghcup-${ver}
put x86_64-openbsd-ghcup-${ver}.sig
put x86_64-openbsd-ghcup.plan.json
put x86_64-linux-ghcup-${ver}
put x86_64-linux-ghcup-${ver}.sig
put x86_64-linux-ghcup.plan.json
put x86_64-mingw64-ghcup-${ver}.exe
put x86_64-mingw64-ghcup-${ver}.exe.sig
put x86_64-mingw64-ghcup.plan.json
put ghcup-${ver}-src.tar.gz
put ghcup-${ver}-src.tar.gz.sig
EOF

curl -X PURGE https://downloads.haskell.org/~ghcup/${ver}/
curl -X PURGE https://downloads.haskell.org/ghcup/${ver}/

(>&2 echo "Now run:
  ./scripts/releasing/04-create-yaml-snippet.sh ${ver}

Then add the snippet to all the ghcup metadata (this is not automated),
replacing the existing GHCup version.
There may be some format differences you have to accomodate between
what the script dumps for the latest metadata version and older metadata
versions.
")

