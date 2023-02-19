#!/bin/bash

url=$1
ver=$2

die() {
    (>&2 printf "%s\\n" "$1")
    exit 2
}

[ -z $url ] && die "no url set"
[ -z $ver ] && die "no version set"

sftp $url <<EOF
cd ghcup

rm aarch64-apple-darwin-ghcup
rm aarch64-linux-ghcup
rm armv7-linux-ghcup
rm i386-linux-ghcup
rm x86_64-apple-darwin-ghcup
rm x86_64-linux-ghcup
rm x86_64-mingw64-ghcup.exe
rm x86_64-portbld-freebsd-ghcup

symlink ${ver}/aarch64-apple-darwin-ghcup-${ver} aarch64-apple-darwin-ghcup
symlink ${ver}/aarch64-linux-ghcup-${ver} aarch64-linux-ghcup
symlink ${ver}/armv7-linux-ghcup-${ver} armv7-linux-ghcup
symlink ${ver}/i386-linux-ghcup-${ver} i386-linux-ghcup
symlink ${ver}/x86_64-apple-darwin-ghcup-${ver} x86_64-apple-darwin-ghcup
symlink ${ver}/x86_64-portbld-freebsd-ghcup-${ver} x86_64-portbld-freebsd-ghcup
symlink ${ver}/x86_64-linux-ghcup-${ver} x86_64-linux-ghcup
symlink ${ver}/x86_64-mingw64-ghcup-${ver}.exe x86_64-mingw64-ghcup.exe
EOF

curl -X PURGE https://downloads.haskell.org/~ghcup/
curl -X PURGE https://downloads.haskell.org/ghcup/
