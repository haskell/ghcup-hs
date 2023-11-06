#!/bin/sh

# !! KEEP THIS SCRIPT POSIX COMPLIANT !!

# see https://docs.haskellstack.org/en/stable/yaml_configuration/#ghc-installation-customisation-experimental
# for documentation about hooks

set -eu

case $HOOK_GHC_TYPE in
    bindist)
        ghc_path=$(ghcup whereis ghc "$HOOK_GHC_VERSION" || { ghcup install ghc "$HOOK_GHC_VERSION" >/dev/null && ghcup whereis ghc "$HOOK_GHC_VERSION" ; }) || { >&2 echo "Installing $HOOK_GHC_VERSION via ghcup failed" exit 3 ;}
        printf "%s" "${ghc_path}"
        ;;
    git)
        # TODO: should be somewhat possible
        >&2 echo "Hook doesn't support installing from source"
        exit 1
        ;;
    *)
        >&2 echo "Unsupported GHC installation type: $HOOK_GHC_TYPE"
        exit 2
        ;;
esac
