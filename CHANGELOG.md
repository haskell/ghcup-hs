# Revision history for ghcup

## 0.1.11 -- ????-??-??

* Allow to compile over existing version (`ghcup compile ghc -v 8.6.5 -b 8.6.5`) and replace it wrt #59
* When compiling GHC from source create a bindist first, store that bindist in `~/.ghcup/cache` and install it, wrt #51
* simplify installing from custom bindist wrt #60
  - `ghcup install ghc -u <url> <version>`
* fix bug when cabal isn't marked executable in bindist
* fix bug when `~/.ghcup` is a valid symlink wrt #49
* Add JSON roundtrip tests

## 0.1.10 -- 2020-08-14

* Show stray Cabals (useful for pre-releases or compiled ones)

## 0.1.9 -- 2020-08-14

* Fix bug when uninstalling all cabal versions
* Fix bug when setting a non-installed ghc version as current default
* Use yaml instead of generated json for download info for ease of adding new GHC versions #44
* Allow pre-release versions of GHC/cabal
* Add XDG dirs support (set `GHCUP_USE_XDG_DIRS`) wrt #39
* Allow to specify regex for tarball subdir (e.g. `ghc-.*`)
* Allow installing arbitrary bindists more seamlessly:
  - e.g. installing GHC HEAD: `ghcup -n install ghc -u '{"dlHash": "", "dlSubdir": { "RegexDir": "ghc-.*"}, "dlUri": "https://gitlab.haskell.org/api/v4/projects/1/jobs/artifacts/master/raw/ghc-x86_64-fedora27-linux.tar.xz?job=validate-x86_64-linux-fedora27" }' head`
* Avoid duplicate edits to .bashrc/.zshrc wrt #43

## 0.1.8 -- 2020-07-21

* Fix bug in logging thread dying on newlines
* Allow to install from arbitrary bindists: `ghcup -n install ghc -u '{"dlHash": "", "dlSubdir": "ghc-8.10.1", "dlUri": "https://github.com/commercialhaskell/ghc/releases/download/ghc-8.10.1-release/ghc-8.10.1-x86_64-deb9-linux.tar.xz"}' 8.10.1`

## 0.1.7 -- 2020-07-20

* Fix a bug in libarchive not unpacking some uncleanly packed bindists
* Improved fish support in bootstrap-haskell
* Only check for upgrades when not upgrading
* Fix platform detection for i386 docker images
* Improve alpine support
  - more/proper bindists
  - don't fall back to glibc based bindists
  - install bindists with `--disable-ld-override` to avoid ld.gold bugs

## 0.1.6 -- 2020-07-13

* Create a new curses (brick) based TUI, accessible via `ghcup tui` #24
* Support multiple installed versions of cabal #23
* Improvements to `ghcup list` (show unavailable bindists for platform)
* Fix redhat downloads #29
* Support for hadrian bindists (fixes alpine-8.10.1) #31
* Add FreeBSD bindists 8.6.5 and 8.8.3
* Fix memory leak during unpack

## 0.1.5 -- 2020-04-30

* Fix errors when PATH variable contains path components that are actually files
* Add `--version` and `--numeric-version` options
* Add `changelog` command
* Also check for available GHC and Cabal updates on start
* Add base versions as tags for every GHC version (these are "installable" tags and the latest GHC version matching the tag will be picked)
* Added `--format-raw` to list subcommand
* Allow to install X.Y versions (e.g.: ghcup install 8.8)
* Implement `--keep=<always|errors|never>` to control temporary build directories cleanup
* Add proper shell completions to the repo
* Fix building of documentation
* Allow to work in offline mode and use cached files if possible
* Allow to set the downloader via `--downloader=<curl|wget>`
* Support for compiling and installing a cross GHC (see README). This is experimental.

## 0.1.4 -- 2020-04-16

* build on all platforms with curl (as a binary), wrt https://gitlab.haskell.org/haskell/ghcup-hs/issues/6
* Fix unlinking of ghc symlinks after new installation, wrt https://gitlab.haskell.org/haskell/ghcup-hs/issues/7

## 0.1.3 -- 2020-04-15

* Fix lesser bug when skipping ghcup update

## 0.1.2 -- 2020-04-15

* Fix bug when removing the set GHC version
* Fix use of undocumented `GHCUP_INSTALL_BASE_PREFIX` variable
* skip upgrade if ghcup is already latest version

## 0.1.1 -- 2020-04-15

* fix awful fdopendir bug on mac bug by updating hpath-posix

## 0.1.0

* First version. Released on an unsuspecting world.
