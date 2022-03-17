# Revision history for ghcup

## 0.1.17.6 -- ????-??-??

* Vastly improve shell completions wrt [#242](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/242)
* Fix 'ghcup install cabal/hls/stack --set' wrt [#324](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/324)
* Fix bad error message wrt [#323](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/323)
* Use predictable /tmp names for `ghcup run`, fixes [#329](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/329)
* Fix bug with isolated installation of not previously installed versions
* Add `--no-set` to install commands, fixes [#330](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/330)
* Fix serious bug in `ghcup list --raw-format -t <tool> -c installed`
* Overhaul metadata merging and add `ghcup config add-release-channel URI` wrt [#328](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/328)

## 0.1.17.5 -- 2022-02-26

* Implement `ghcup run` subcommand wrt [#137](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/137)
* Support installation of dynamic HLS bindists wrt [HLS #2675](https://github.com/haskell/haskell-language-server/pull/2675) and [#237](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/237)
* Fix XDG support when `~/.local/bin` is a symlink wrt [#311](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/311)
* Add support for quilt-style patches wrt [#230](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/230), by James Hobson
* Fix redundant upgrade warnings in `ghcup upgrade`
* Fix `ghcup whereis ghc` for non-standard versions wrt [#289](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/289)
* Don't print logs to stdout, but stderr
* Allow unpacking legacy lzma archives wrt [#307](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/307)
* Allow to disable self-upgrade functionality wrt [#305](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/305)
* Fix `ghcup install ghc --set` when ghc is already installed wrt [#291](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/291)

## 0.1.17.4 -- 2021-11-13

* add `--metadata-caching` option, allowing to also disable yaml metadata caching wrt [#278](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/278)
* make upgrading ghcup in TUI more pleasant wrt [#276](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/276)
* fix parsing of atypical GHC versions (e.g. `8.10.5-patch1`)
* fix compiling HLS dynamically linked, also see [#245](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/245)
* redo (and break) some of the `ghcup compile <tool>` interface, improving patch options and setting custom cabal.project files
* avoid redundant update warnings wrt [#283](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/283)

## 0.1.17.3 -- 2021-10-27

* clean up during unpack failures as well
* migrate te aeson-2.0.1.0
* switch to yaml-streamly to fix performance regression wrt [#270](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/270)
* use [github.com/haskell/ghcup-metadata](https://github.com/haskell/ghcup-metadata) for metadata file download (better caching)

## 0.1.17.2 -- 2021-09-30

* Honour GHC bootstrap compiler during git clone stages wrt [#250](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/250)
* Speed up `unset` command
* Fix `--overwrite-version` for `ghcup compile ghc` wrt [#253](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/253)
* Apply patches before bootstrap

## 0.1.17.1 -- 2021-09-26

* Fix `NO_COLOR`
* Fix `ghcup list -t` for hls/stack, wrt [#244](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/244)
* Get rid of concurrent-output
* Improve cli interface with partial versions (e.g. `ghcup install ghc 8`)
* Fix HLS compilation builds
* Implement `ghcup gc` (garbage collection) command

## 0.1.17 -- 2021-09-20

* Add `--force` option to install/compile wrt [#210](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/210) by Arjun Kathuria
* Implement compiling HLS from source wrt [#201](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/201)
* Implement experimental GPG verification of the metadata file (see README) wrt [#263](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/236)
* Add `ghcup unset` command wrt [#145](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/145)
* Add `ghcup whereis bindir` etc wrt [#221](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/221)
* Greatly reduce dependency footprint wrt [#212](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/212)
* Add `ghcup --plan-json`
* Improve `--patchdir` option for GHC compilation wrt [#226](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/226)
* Try to improve logging and failure modes, especially during downloads
* Add descriptive warnings when HLS and GHC versions are incompatible
* Improve curl header parsing wrt [#213](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/213)

## 0.1.16.2 -- 2021-08-12

* Add isolated installations wrt [#141](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/141) by Arjun Kathuria
* Implement config cli MVP wrt [#134](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/134) by Oleksii Dorozhkin
* Fix `ghcup compile ghc --flavor`
* Fix minor installation bug causing increased disk space wrt [#139](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/139)
* Improved error handling wrt [#136](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/136)
* Various improvements to metadata download when using `file://` and `--offline` wrt [#137](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/137)

## 0.1.16.1 -- 2021-07-29

* Add 'nuke' subcommand wrt [#135](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/135), implemented by Arjun Kathuria
* Add uninstallation powershell script on windows wrt [#150](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/150)
* Improve logging
* Fix building GHC cross compiler wrt [#180](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/180)
* Allow to use hadrian as build system (for git based versions only) wrt [#35](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/35)
* Allow passing `--flavor` to `ghcup compile ghc`
* Support new GHC `bin/` directory format wrt [ghc/ghc#20074](https://gitlab.haskell.org/ghc/ghc/-/issues/20074#note_363720)
* Implement `whereis` subcommand wrt [#173](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/173)
* Add `--offline` switch and `prefetch` subcommand wrt [#186](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/186)
* Implement ETAGs hashing for metadata downloads to speed up `ghcup list` wrt [#193](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/193)
* Avoid unnecessary fetching of ghcup metadata in some commands
* Avoid unnecessary update checks for some commands
* Preserve mtimes on unpacked GHC tarballs on windows wrt [#187](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/187), fixing issues with `ghc-pkg`
* Fix lesser bug in `ghcup list` for stray stack versions wrt [#183](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/183)
* Major redo on how file removal on windows works, avoiding partial removals etc, wrt [#165](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/165)
* Improve ghcup tui for screen readers wrt [github/#4](https://github.com/haskell/ghcup-hs/pull/4), thanks to Mario Lang

## 0.1.15.2 -- 2021-06-13

* Remove legacy handling of cabal binary and be more graceful about binaries not installed by ghcup (e.g. stack)
* Fix GHC compilation from git
* Fix 'ghcup upgrade' on windows
* Allow to skip update checks via `GHCUP_SKIP_UPDATE_CHECK`
* Use libarchive on windows as well, fixing unpack errors wrt [#147](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/147)

## 0.1.15.1 -- 2021-06-11

* Add Apple Silicon support
* Add windows support wrt [#130](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/130)
* Add stack support
* Warn when /tmp doesn't have 5GB or more of disk space
* Allow to compile GHC from git repo wrt [#126](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/126)
* Allow to set custom ghc version when running 'ghcup compile ghc' wrt [#136](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/136)
* Add date to GHC bindist names created by ghcup

## 0.1.14.2 -- 2021-05-12

* Remove dead dependency on ascii-string

## 0.1.14.1 -- 2021-04-11

* Make internal symlink target parser more lax, fixes [#119](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/119)
* Prepare for hackage release

## 0.1.14 -- 2021-03-07

* Major bugfix: fix handling of stray versions wrt [#116](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/116)
* Fix error messages and overhaul pretty printing wrt [#115](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/115)

## 0.1.13 -- 2021-02-26

* Support ARMv7/AARCH64
* Add command line completions for installed and available versions wrt [MR #70](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/70)
* Allow to cycle through set tools wrt [#114](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/114)
* Fix item selection with unavailable versions wrt [#107](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/107)
* Allow for dynamic post-install, post-remove and pre-compile msgs wrt [MR #68](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/68)
* Alert user if upgraded ghcup is shadowed by old ghcup wrt [#111](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/111)
* Fix to `ghcup` directory creation and placement for the XDG install mode ([MR #49](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/49))
* Do 755 permissions on executables, wrt #97
* Add [NO_COLOR](https://no-color.org/) support wrt [MR #47](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/47)

## 0.1.12 -- 2020-11-21

* Fix disappearing HLS symlinks wrt #91
* improve TUI:
  - separators between tools sections
  - reverse list order so latest is on top
  - expand the blues selected bar
  - show new latest versions in bright white
* allow configuration file and setting TUI hotkeys wrt #41
  - see https://gitlab.haskell.org/haskell/ghcup-hs#configuration for a more in-depth explanation
* add a `--set` switch to `ghcup install ghc` to automatically set as default after install
* emit warnings when CC/LD is set wrt #82
* add support for version ranges in distro specifiers wrt #84
  - e.g. `"(>= 19 && <= 20) || ==0.2.2"` is a valid version key for distro

## 0.1.11 -- 2020-09-23

* Add support for installing haskell-language-server, wrt #65
* When compiling GHC from source create a bindist first, store that bindist in `~/.ghcup/cache` and install it, wrt #51
* Allow to compile over existing version (`ghcup compile ghc -v 8.6.5 -b 8.6.5`) and replace it wrt #59
* simplify installing from custom bindist wrt #60
  - `ghcup install ghc -u <url> <version>`
* fix bug when cabal isn't marked executable in bindist
* fix bug when `~/.ghcup` is a valid symlink wrt #49
* Drop support for compiling cabal from source (the old bootstrap script is discontinued)

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
