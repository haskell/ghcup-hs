#!/bin/bash

set -eu
set -o pipefail

RELEASE=$1

get_sha() {
    sha256sum "$1" | awk '{ print $1 }'
}

cd "gh-release-artifacts/v${RELEASE}"

cat <<EOF > /dev/stdout
  GHCup:
    ${RELEASE}:
      viTags:
      - Recommended
      - Latest
      viChangeLog: https://github.com/haskell/ghcup-hs/blob/master/CHANGELOG.md
      viSourceDL:
        dlUri: https://downloads.haskell.org/~ghcup/${RELEASE}/ghcup-${RELEASE}-src.tar.gz
        dlSubdir: ghcup-${RELEASE}
        dlHash: $(get_sha "ghcup-${RELEASE}-src.tar.gz")
      viArch:
        A_64:
          Linux_UnknownLinux:
            unknown_versioning: &ghcup-64
              dlUri: https://downloads.haskell.org/~ghcup/${RELEASE}/x86_64-linux-ghcup-${RELEASE}
              dlHash: $(get_sha "x86_64-linux-ghcup-${RELEASE}")
          Darwin:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~ghcup/${RELEASE}/x86_64-apple-darwin-ghcup-${RELEASE}
              dlHash: $(get_sha "x86_64-apple-darwin-ghcup-${RELEASE}")
          FreeBSD:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~ghcup/${RELEASE}/x86_64-portbld-freebsd-ghcup-${RELEASE}
              dlHash: $(get_sha "x86_64-portbld-freebsd-ghcup-${RELEASE}")
          Windows:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~ghcup/${RELEASE}/x86_64-mingw64-ghcup-${RELEASE}.exe
              dlHash: $(get_sha "x86_64-mingw64-ghcup-${RELEASE}.exe")
          Linux_Alpine:
            unknown_versioning: *ghcup-64
        A_32:
          Linux_UnknownLinux:
            unknown_versioning: &ghcup-32
              dlUri: https://downloads.haskell.org/~ghcup/${RELEASE}/i386-linux-ghcup-${RELEASE}
              dlHash: $(get_sha "i386-linux-ghcup-${RELEASE}")
          Linux_Alpine:
            unknown_versioning: *ghcup-32
        A_ARM64:
          Linux_UnknownLinux:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~ghcup/${RELEASE}/aarch64-linux-ghcup-${RELEASE}
              dlHash: $(get_sha "aarch64-linux-ghcup-${RELEASE}")
          Darwin:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~ghcup/${RELEASE}/aarch64-apple-darwin-ghcup-${RELEASE}
              dlHash: $(get_sha "aarch64-apple-darwin-ghcup-${RELEASE}")
        A_ARM:
          Linux_UnknownLinux:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~ghcup/${RELEASE}/armv7-linux-ghcup-${RELEASE}
              dlHash: $(get_sha "armv7-linux-ghcup-${RELEASE}")
EOF

