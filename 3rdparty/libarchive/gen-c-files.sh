#!/bin/sh

set -xe

if [ -z "$1" ] ; then
	echo "No libarchive version specified, aborting..."
	exit 1
fi

LIBARCHIVE_VER="$1"
LIBARCHIVE_BALL="libarchive-${LIBARCHIVE_VER}.tar.gz"
LIBARCHIVE_URL="https://libarchive.org/downloads/${LIBARCHIVE_BALL}"

[ -f "${LIBARCHIVE_BALL}" ] || curl -LO "${LIBARCHIVE_URL}"

[ -d "libarchive-${LIBARCHIVE_VER}" ] || tar xf "${LIBARCHIVE_BALL}"

cd "libarchive-${LIBARCHIVE_VER}"

./configure \
	--without-openssl \
	--without-xml2 \
	--without-expat \
	--without-cng \
	--without-lzma \
	--without-zstd \
	--without-lz4 \
	--without-iconv \
	--without-libb2 \
	--without-bz2lib \
	--without-zlib \
	--without-libiconv-prefix \
	--disable-acl \
	--disable-xattr \
	--disable-posix-regex-lib \
	--disable-bsdcpio \
	--disable-bsdcat \
	--disable-bsdtar

case "$(uname -s)" in
	"linux"|"Linux")
		sed \
			-e '/define HAVE_LIBMD/d' \
			-e '/define ARCHIVE_CRYPTO_.*_LIBMD/d' \
			-e '/define HAVE_MD5_H/d' \
			-e '/define HAVE_RIPEMD_H/d' \
			-e '/define HAVE_SHA256_H/d' \
			-e '/define HAVE_SHA512_H/d' \
			-e '/define HAVE_SHA_H/d' \
			-e '/define HAVE_SYS_ACL_H/d' \
			-e '/define HAVE_EXT2FS_EXT2_FS_H/d' \
			config.h > ../c/autoconf-linux/config.h ;;
	"Darwin"|"darwin")
		sed \
			-e '/define HAVE_LIBMD/d' \
			-e '/define ARCHIVE_CRYPTO_.*_LIBMD/d' \
			-e '/define HAVE_MD5_H/d' \
			-e '/define HAVE_RIPEMD_H/d' \
			-e '/define HAVE_SHA256_H/d' \
			-e '/define HAVE_SHA512_H/d' \
			-e '/define HAVE_SHA_H/d' \
			-e '/define HAVE_SYS_ACL_H/d' \
			-e '/define HAVE_EXT2FS_EXT2_FS_H/d' \
			config.h > ../c/autoconf-darwin/config.h ;;
	"FreeBSD"|"freebsd")
		sed \
			-e '/define HAVE_LIBMD/d' \
			-e '/define ARCHIVE_CRYPTO_.*_LIBMD/d' \
			-e '/define HAVE_MD5_H/d' \
			-e '/define HAVE_RIPEMD_H/d' \
			-e '/define HAVE_SHA256_H/d' \
			-e '/define HAVE_SHA512_H/d' \
			-e '/define HAVE_SHA_H/d' \
			-e '/define HAVE_SYS_ACL_H/d' \
			-e '/define HAVE_EXT2FS_EXT2_FS_H/d' \
			config.h > ../c/autoconf-freebsd/config.h ;;

	# TODO: windows
	*) die "Unknown platform" ;;
esac

rm ../c/*.[ch]
cp libarchive/*.[ch] ../c/

