#!/bin/sh

set -eux

. "$( cd "$(dirname "$0")" ; pwd -P )/../ghcup_env"

mkdir -p "$CI_PROJECT_DIR"/.local/bin

git describe --always

### build

curl -L -O https://get.haskellstack.org/stable/linux-x86_64.tar.gz
tar xf linux-x86_64.tar.gz
cp stack-*-linux-*/stack "$CI_PROJECT_DIR"/.local/bin/stack
chmod +x "$CI_PROJECT_DIR"/.local/bin/stack

mkdir -p "$CI_PROJECT_DIR"/.stack_root
export TAR_OPTIONS=--no-same-owner
stack --allow-different-user --stack-root "$CI_PROJECT_DIR"/.stack_root build
stack --allow-different-user --stack-root "$CI_PROJECT_DIR"/.stack_root test
