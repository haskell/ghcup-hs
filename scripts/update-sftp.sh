#!/bin/bash

url=$1
ver=$2
artifacts_dir=$3

die() {
    (>&2 printf "%s\\n" "$1")
    exit 2
}

[ -z $url ] && die "no url set"
[ -z $ver ] && die "no version set"
[ -z "${artifacts_dir}" ] && die "artifacts_dir not set"
[ -e "${artifacts_dir}" ] && die "artifacts_dir \"${artifacts_dir}\" does not exist"

cd "${artifacts_dir}"

sftp $url <<EOF
cd ghcup

mkdir ${ver}
cd ${ver}
put SHA256SUMS
put SHA256SUMS.sig
put aarch64-apple-darwin-ghcup-${ver}
put aarch64-apple-darwin-ghcup-${ver}.sig
put aarch64-linux-ghcup-${ver}
put aarch64-linux-ghcup-${ver}.sig
put armv7-linux-ghcup-${ver}
put armv7-linux-ghcup-${ver}.sig
put i386-linux-ghcup-${ver}
put i386-linux-ghcup-${ver}.sig
put x86_64-apple-darwin-ghcup-${ver}
put x86_64-apple-darwin-ghcup-${ver}.sig
put x86_64-freebsd12-ghcup-${ver}
put x86_64-freebsd12-ghcup-${ver}.sig
put x86_64-freebsd13-ghcup-${ver}
put x86_64-freebsd13-ghcup-${ver}.sig
put x86_64-linux-ghcup-${ver}
put x86_64-linux-ghcup-${ver}.sig
put x86_64-mingw64-ghcup-${ver}.exe
put x86_64-mingw64-ghcup-${ver}.exe.sig
cd ..

rm aarch64-apple-darwin-ghcup
rm aarch64-linux-ghcup
rm armv7-linux-ghcup
rm i386-linux-ghcup
rm x86_64-apple-darwin-ghcup
rm x86_64-linux-ghcup
rm x86_64-mingw64-ghcup.exe
rm x86_64-freebsd12-ghcup
rm x86_64-freebsd13-ghcup

symlink ${ver}/aarch64-apple-darwin-ghcup-${ver} aarch64-apple-darwin-ghcup
symlink ${ver}/aarch64-linux-ghcup-${ver} aarch64-linux-ghcup
symlink ${ver}/armv7-linux-ghcup-${ver} armv7-linux-ghcup
symlink ${ver}/i386-linux-ghcup-${ver} i386-linux-ghcup
symlink ${ver}/x86_64-apple-darwin-ghcup-${ver} x86_64-apple-darwin-ghcup
symlink ${ver}/x86_64-freebsd12-ghcup-${ver} x86_64-freebsd12-ghcup
symlink ${ver}/x86_64-freebsd13-ghcup-${ver} x86_64-freebsd13-ghcup
symlink ${ver}/x86_64-linux-ghcup-${ver} x86_64-linux-ghcup
symlink ${ver}/x86_64-mingw64-ghcup-${ver}.exe x86_64-mingw64-ghcup.exe
EOF

curl -X PURGE https://downloads.haskell.org/~ghcup/
curl -X PURGE https://downloads.haskell.org/ghcup/
curl -X PURGE https://downloads.haskell.org/~ghcup/${ver}/
curl -X PURGE https://downloads.haskell.org/ghcup/${ver}/
