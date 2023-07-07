#!/usr/bin/env bash

case "$(uname -s)" in
	MSYS_*|MINGW*)
		ext=".exe"
		;;
	*)
		ext=""
	   ;;
esac

echo "cabal-cache disabled (CABAL_CACHE_DISABLE set)"

