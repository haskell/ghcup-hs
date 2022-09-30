#!/bin/sh

# !! KEEP THIS SCRIPT POSIX COMPLIANT !!

# see https://docs.haskellstack.org/en/stable/yaml_configuration/#ghc-installation-customisation-experimental
# for documentation about hooks

set -eu

case $HOOK_GHC_TYPE in
    bindist)
        ghcdir=$(ghcup whereis --directory ghc "$HOOK_GHC_VERSION" || ghcup run --ghc "$HOOK_GHC_VERSION" --install) || exit 3
        printf "%s/ghc" "${ghcdir}"
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
