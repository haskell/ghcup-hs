#!/bin/sh

set -eux

BUILD_DIR=$CI_PROJECT_DIR
echo "Cleaning $BUILD_DIR"
cd $HOME
test -n "$BUILD_DIR"
shopt -s extglob
rm -Rf "$BUILD_DIR"/!(out)
if [ "${OS}" = "WINDOWS" ] ; then
	rm -Rf /c/ghcup
fi

exit 0
