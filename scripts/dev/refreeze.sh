#!/bin/sh

set -xue

rm -f cabal.*.project
rm -f cabal.*.project.freeze

for ghc_ver in "$@" ; do
	# shellcheck disable=SC3060
	project_file=cabal.ghc${ghc_ver//./}.project

	case "$(uname -s)" in
        MSYS*|MINGW*)
	        # shellcheck disable=SC3060
	        project_file_os=cabal.ghc${ghc_ver//./}.Win32.project
	        cp cabal.project "${project_file_os}"
			cabal freeze --project-file="${project_file_os}" -w "ghc-${ghc_ver}" -ftui
			;;
		*)
	        # shellcheck disable=SC3060
	        project_file_os=cabal.ghc${ghc_ver//./}.Unix.project
	        cp cabal.project "${project_file_os}"
			cabal freeze --project-file="${project_file_os}" -w "ghc-${ghc_ver}" -ftui -finternal-downloader
			;;
	esac


	sed -i -e '/ghcup/d' "${project_file_os}".freeze

    cat <<EOF > "${project_file}" || die
if os(mingw32)
  import: cabal.ghc${ghc_ver//./}.Win32.project
  import: cabal.ghc${ghc_ver//./}.Win32.project.freeze
else
  import: cabal.ghc${ghc_ver//./}.Unix.project
  import: cabal.ghc${ghc_ver//./}.Unix.project.freeze

with-compiler: ghc-${ghc_ver}
EOF
done

