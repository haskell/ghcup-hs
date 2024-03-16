#!/bin/sh

set -xue

rm -f cabal.*.project
rm -f cabal.*.project.freeze

for ghc_ver in "$@" ; do
	# shellcheck disable=SC3060
	project_file=cabal.ghc${ghc_ver//./}.project

	cp cabal.project "${project_file}"
	case "$(uname -s)" in
        MSYS*|MINGW*)
			cabal freeze --project-file="${project_file}" -w "ghc-${ghc_ver}" -ftui
			;;
		*)
			cabal freeze --project-file="${project_file}" -w "ghc-${ghc_ver}" -ftui -finternal-downloader
			;;
	esac


	echo "" >> "${project_file}"
	echo "with-compiler: ghc-${ghc_ver}" >> "${project_file}"

	sed -i -e '/ghcup/d' "${project_file}".freeze
done

