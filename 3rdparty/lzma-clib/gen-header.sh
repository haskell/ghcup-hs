#!/bin/sh

set -xe

if [ -z "$1" ] ; then
	echo "No xz version specified, aborting..."
	exit 1
fi

XZ_VER="$1"
XZ_BALL="xz-${XZ_VER}.tar.bz2"
XZ_URL="https://tukaani.org/xz/${XZ_BALL}"

[ -f "${XZ_BALL}" ] || curl -LO "${XZ_URL}"

[ -d "xz-${XZ_VER}" ] || tar xf "${XZ_BALL}"

cd "xz-${XZ_VER}"
./configure

case "$(uname -s)" in
	"linux"|"Linux")
		cp config.h ../autoconf-linux/config.h ;;
	"Darwin"|"darwin")
		sed -i \
			-e '/define HAVE_CC_SHA256_CTX/d' \
			-e '/define HAVE_CC_SHA256_INIT/d' \
			-e '/define HAVE_CLOCK_GETTIME/d' \
			-e '/define HAVE_DECL_CLOCK_MONOTONIC/d' \
			config.h
		cp config.h ../autoconf-darwin/config.h ;;
	*) die "Unknown platform" ;;
esac

