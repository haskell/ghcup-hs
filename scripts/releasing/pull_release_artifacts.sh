#!/bin/bash

set -eu
set -o pipefail

shopt -s extglob

RELEASE=$1
SIGNER=$2

echo "RELEASE: $RELEASE"
echo "SIGNER: $SIGNER"

for com in gh gpg curl sha256sum ; do
	command -V ${com} >/dev/null 2>&1
done

[ ! -e "gh-release-artifacts/${RELEASE}" ]

mkdir -p "gh-release-artifacts/${RELEASE}"

git archive --format=tar.gz -o "gh-release-artifacts/${RELEASE}/ghcup-${RELEASE}-src.tar.gz" --prefix="ghcup-${RELEASE}/" HEAD

cd "gh-release-artifacts/${RELEASE}"

# github
gh release download "$RELEASE"

# cirrus
curl -L -o "x86_64-portbld-freebsd-ghcup-${RELEASE}" \
	"https://api.cirrus-ci.com/v1/artifact/github/haskell/ghcup-hs/build/binaries/out/x86_64-portbld-freebsd-ghcup-${RELEASE}?branch=${RELEASE}"

sha256sum ./*-ghcup-* > SHA256SUMS
gpg --detach-sign -u "${SIGNER}" SHA256SUMS

gh release upload "$RELEASE" "ghcup-${RELEASE}-src.tar.gz" "x86_64-portbld-freebsd-ghcup-${RELEASE}" SHA256SUMS SHA256SUMS.sig
