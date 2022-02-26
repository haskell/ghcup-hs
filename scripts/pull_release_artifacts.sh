
set -eu

tag=v$1
ver=$1

dest=$2
gpg_user=$3

mkdir -p "${dest}"

cd "${dest}"

base_url="https://gitlab.haskell.org/api/v4/projects/618/jobs/artifacts/${tag}/raw"

curl -f -o "x86_64-apple-darwin-ghcup-${ver}" \
	"${base_url}/out/x86_64-apple-darwin-ghcup-${ver}?job=release:darwin"

curl -f -o "aarch64-apple-darwin-ghcup-${ver}" \
	"${base_url}/out/aarch64-apple-darwin-ghcup-${ver}?job=release:darwin:aarch64"

curl -f -o "x86_64-freebsd12-ghcup-${ver}" \
	"${base_url}/out/x86_64-portbld-freebsd-ghcup-${ver}?job=release:freebsd12"

curl -f -o "x86_64-freebsd13-ghcup-${ver}" \
	"${base_url}/out/x86_64-portbld-freebsd-ghcup-${ver}?job=release:freebsd13"

curl -f -o "i386-linux-ghcup-${ver}" \
	"${base_url}/out/i386-linux-ghcup-${ver}?job=release:linux:32bit"

curl -f -o "x86_64-linux-ghcup-${ver}" \
	"${base_url}/out/x86_64-linux-ghcup-${ver}?job=release:linux:64bit"

curl -f -o "aarch64-linux-ghcup-${ver}" \
	"${base_url}/out/aarch64-linux-ghcup-${ver}?job=release:linux:aarch64"

curl -f -o "armv7-linux-ghcup-${ver}" \
	"${base_url}/out/armv7-linux-ghcup-${ver}?job=release:linux:armv7"

curl -f -o "x86_64-mingw64-ghcup-${ver}.exe" \
	"${base_url}/out/x86_64-mingw64-ghcup-${ver}.exe?job=release:windows"

rm -f *.sig
sha256sum *-ghcup-* > SHA256SUMS
gpg --detach-sign -u ${gpg_user} SHA256SUMS
for f in *-ghcup-* ; do gpg --detach-sign -u ${gpg_user} $f ; done



