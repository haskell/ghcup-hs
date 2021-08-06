#!/bin/sh

set -eu

case $HOOK_GHC_TYPE in
	bindist)
		ghcup --offline whereis ghc $HOOK_GHC_VERSION || {
			ghcup --cache install ghc $HOOK_GHC_VERSION && ghcup whereis ghc $HOOK_GHC_VERSION
		}
		;;
	git)
		>&2 echo "Hook doesn't support installing from source."
		>&2 echo "Consider enabling stack GHC installs for this project, via:"
		>&2 echo "    stack config set install-ghc true"
		exit 1
		;;
	*)
		>&2 echo "Unsupported GHC installation type: ${HOOK_GHC_TYPE}."
		>&2 echo "Consider enabling stack GHC installs for this project, via:"
		>&2 echo "    stack config set install-ghc true"
		exit 2
		;;
esac
