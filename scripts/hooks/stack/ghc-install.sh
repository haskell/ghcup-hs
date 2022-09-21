#!/bin/bash

# see https://docs.haskellstack.org/en/stable/yaml_configuration/#ghc-installation-customisation-experimental
# for documentation about hooks

set -euo pipefail

case $HOOK_GHC_TYPE in
    bindist)
        echo "$(ghcup run --ghc "$HOOK_GHC_VERSION" --install)/ghc"
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
