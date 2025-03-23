# Revision history for ghcup

## 0.1.50.0 -- 2025-03-23

* Fix logic when guessing incomplete PVP versions, fixes [#1243](https://github.com/haskell/ghcup-hs/pull/1243)
* Add `default` channel alias wrt [#1196](https://github.com/haskell/ghcup-hs/pull/1196)
* Print post rm messages in TUI wrt [#1202](https://github.com/haskell/ghcup-hs/pull/1202)
* Speed up metadata parsing significantly wrt [#1213](https://github.com/haskell/ghcup-hs/issues/1213)
* Implement "install targets", fixes [#1210](https://github.com/haskell/ghcup-hs/issues/1210)
* Fix handling of 'ghc' subdir in parser for obtaining set ghc version wrt [#1215](https://github.com/haskell/ghcup-hs/issues/1215)

## 0.1.40.0 -- 2025-01-01

### New features

* Introduction of "channel aliases" wrt [#1155](https://github.com/haskell/ghcup-hs/pull/1155)
  * as of now: `prereleases`, `cross` and `vanilla`
  * use via e.g. `ghcup config add-release-channel cross`
* Implement pager support wrt [#1118](https://github.com/haskell/ghcup-hs/pull/1118)
* Add explicit support for OpenSUSE wrt [#1124](https://github.com/haskell/ghcup-hs/pull/1124)
* Add explicit support for OpenBSD wrt [#1138](https://github.com/haskell/ghcup-hs/pull/1138) (still WIP)
* Support tui list item selection via mouse click [#1158](https://github.com/haskell/ghcup-hs/pull/1158)
* proper aarch64 Alpine support

### Improvements and bug fixes

* Context Menu improvements wrt [#1102](https://github.com/haskell/ghcup-hs/pull/1102)
  * GHC(s) can be selected from a list of installed GHCs in bootstrap-ghc, hadrian-ghc, and HLS target-ghcs
  * Build system can be selected from choices
  * All the text edit inputs require opening an edit box by pressing "Enter"
  * The help message an error message are also shown as the user types.
* Fix 'ghcup run' on windows, wrt [#1106](https://github.com/haskell/ghcup-hs/pull/1106)
* Print aeson decoding error when metadata decoding fails wrt [#1113]((https://github.com/haskell/ghcup-hs/pull/1113)
* laxer forward compatible metadata parsing (e.g. on unknown distros)


## 0.1.30.0 -- 2024-07-07

### New features

* TUI: new context menus for advanced installation and compilation by @lsmor [#949](https://github.com/haskell/ghcup-hs/pull/949)
* Add `def-ghc-conf-options` config option

### Improvements and bug fixes

* Fix TUI for screenreaders
* Fix windows process creation [#1036](https://github.com/haskell/ghcup-hs/issues/1036)
* Migrate from streamly to conduit to unbreak hackage releases, wrt [#1051](https://github.com/haskell/ghcup-hs/issues/1051)
* Improve parsing of --platform values, fixes [#1057](https://github.com/haskell/ghcup-hs/issues/1057)
* Avoid option clash on `-s`
* Don't allow to specify --hadrian and --config wrt [#1082](https://github.com/haskell/ghcup-hs/issues/1082)
* Point to correct store dir when XDG is used [#1089](https://github.com/haskell/ghcup-hs/issues/1089)
* Fix boot GHC for `ghcup compile ghc` wrt [#1045](https://github.com/haskell/ghcup-hs/issues/1045)
* Add hadrian-ghc flag wrt [#1044](https://github.com/haskell/ghcup-hs/issues/1044)

## 0.1.22.0 -- 2024-03-13

### New features

* A help screen/tutorial in the TUI by @lsmor (Luis Morillo)
* Beef up `--overwrite-version`, fixes [#998](https://github.com/haskell/ghcup-hs/issues/998)
  * e.g. `ghcup compile hls -g master --overwrite-version='%v-%h' --ghc 9.4.8` will produce a binary called `haskell-language-server-wrapper-<version-from-cabal-file>-<short-git-commit-hash>`... refer to `ghcup compile hls --help` for more information
* Allow to set ghcup msys2 environment wrt [#982](https://github.com/haskell/ghcup-hs/issues/982)
* Add mechanism to warn on new metadata versions, fixes [#860](https://github.com/haskell/ghcup-hs/issues/860)
* Add pre-install message support via ghcup metadata, wrt [#1016](https://github.com/haskell/ghcup-hs/issues/1016)
* Allow to remove all unset versions, fixes [#1019](https://github.com/haskell/ghcup-hs/issues/1019)
  * e.g.: `ghcup gc --unset`

### Improvements and bug fixes

* Fix potential [HSEC-2024-0002](https://haskell.github.io/security-advisories/advisory/HSEC-2024-0002.html)
* Fix TUI crash in windows terminal 1.19 [#1013](https://github.com/haskell/ghcup-hs/issues/1013)
* Clean up on git clone errors, fixes [#1004](https://github.com/haskell/ghcup-hs/issues/1004)
* Error out on empty UserSettings wrt [#922](https://github.com/haskell/ghcup-hs/issues/922)
* Fix failure mode when metadata is garbage, fixes [#921](https://github.com/haskell/ghcup-hs/issues/921)
* Be less confusing when user tries to 'set' ghcup in TUI, fixes [#923](https://github.com/haskell/ghcup-hs/issues/923)
* Fix prefetch for cross bindists
* Fix misinterpretation of '+' in URI paths, fixes [#408](https://github.com/haskell/ghcup-hs/issues/408)
* Stricter (and better) file uri handling
* Set LD=ld.bfd on Alpine linux during bindist configure
* Add rocky/void detection
* Logging improvements
* Remove the "show all tool" config in the TUI
* Fix opening changelog on windows
* Don't remove share dir link prematurely
* Require user to explicitly choose subcommand for 'ghcup config'
* Don't download twice when trying stack decoding

### Refactoring and maintenance

* Large TUI code cleanup by @lsmor (Luis Morillo)... more coming up soon
* Allow building with `tar` instead of `libarchive` (mainly to make contributions easier)

## 0.1.20.0 -- 2023-11-10

### New features

* support TUI on windows thanks to the work from vty and brick maintainers (Chris Hackett, Timofey Zakrevskiy, Jonathan Daugherty, ...), wrt [#912](https://github.com/haskell/ghcup-hs/pull/912)
* support JS and wasm cross compilers wrt [#838](https://github.com/haskell/ghcup-hs/issues/838), thanks to Sylvain Henry and IOG
* Support stacks installation strategy and metadata wrt [#892](https://github.com/haskell/ghcup-hs/issues/892)
  - you can now enable stacks installation method via `ghcup config set url-source '["GHCupURL", "StackSetupURL"]'`... for more information, check the [documentation](https://www.haskell.org/ghcup/guide/#using-stacks-setup-info-metadata-to-install-ghc)

### Improvements and bug fixes

* fix segfault in TUI when hitting enter early wrt [#887](https://github.com/haskell/ghcup-hs/issues/887)
* Improve key handling in TUI, fixes [#875](https://github.com/haskell/ghcup-hs/issues/875)
* add explicit support for Void Linux and Rocky Linux (this requires a metadata version bump to `ghcup-0.0.8.yaml`)
* optparse cli interface now has a test suite thanks to Lei Zhu, wrt [#862](https://github.com/haskell/ghcup-hs/pull/862)

## 0.1.19.4 -- 2023-7-02

* fix missing TUI for aarch64 linux binaries

## 0.1.19.3 -- 2023-6-29

* Implement support for nightlies, wrt [#824](https://github.com/haskell/ghcup-hs/issues/824)
* Fix GC with XDG dirs, fixes [#810](https://github.com/haskell/ghcup-hs/issues/810)

## 0.1.19.2 -- 2023-2-24

* Follow-up fix for JFS/ReiserFS and other filesystem that don't support `d_type`, fixes [#787](https://github.com/haskell/ghcup-hs/issues/787)
    - the previous release had a bug that invalidated that broke it
* Implement 'latest-prerelease' tag wrt [#788](https://github.com/haskell/ghcup-hs/issues/788)
* Fix 'Could not parse version of stray directory.DS_Store' warnings on macOs wrt [#797](https://github.com/haskell/ghcup-hs/issues/797)

## 0.1.19.1 -- 2023-2-19

* Fix GHCup on JFS/ReiserFS and other filesystem that don't support `d_type`, fixes [#766](https://github.com/haskell/ghcup-hs/issues/766)
* Don't fail on setModificationTime, fixes [#784](https://github.com/haskell/ghcup-hs/issues/784) and many GitHub actions issues
* Make armv7/aarch64 linux binaries more portable (built on Debian buster)
* Improve usability on 'ghcup config add-release-channel', fixes [#751](https://github.com/haskell/ghcup-hs/issues/751)
* Make version shortcuts work with 'ghcup set', fixes [#757](https://github.com/haskell/ghcup-hs/issues/757)
* Don't implicitly smuggle in config options in `ghcup config set` wrt [#775](https://github.com/haskell/ghcup-hs/issues/775)
* Fix build on unix with -ftui

## 0.1.19.0 -- 2023-1-13

* restore proper support for FreeBSD and Linux armv7
* integrate with [errors.haskell.org](https://errors.haskell.org/index.html), wrt [#434](https://github.com/haskell/ghcup-hs/issues/434)
* allow to overwrite distro detection via config wrt [#421](https://github.com/haskell/ghcup-hs/issues/421)
    - this is particularly useful for e.g. Ubuntu derivates, where ghcup doesn't pick the optimal bindist, also see the [GHCup documentation on overriding distro detection](https://www.haskell.org/ghcup/guide/#overriding-distro-detection)
* Add proper support for mirrors wrt [#357](https://github.com/haskell/ghcup-hs/issues/357)
* fix a (harmless) bug in `ghcup nuke` on windows
* improvements to `ghcup add-release-channel` wrt [#708](https://github.com/haskell/ghcup-hs/issues/708)
* fix building newer GHC from source wrt [#433](https://github.com/haskell/ghcup-hs/issues/433)
* Fix `ghcup install hls -u` on windows
* Fix failure with `--isolate=dir --force`
* Add `--metadata-fetching-mode` arg, fixes [#440](https://github.com/haskell/ghcup-hs/issues/440)
* Add content-length property to downloads
* [Fix a grave bug on armv7](https://github.com/haskell/ghcup-hs/commit/78ee956df2618862f421178a565c82548ff7e578) during installation wrt [#415](https://github.com/haskell/ghcup-hs/issues/415)
* improve many warning/error messages (contributions by @taylorfausak)
* some minor optimization in `ghcup whereis ghcup`
* improve `--keep=always` to not clean up directories in certain circumstances

## 0.1.18.1 -- 2022-08-06

* fix sdist and unbreak hackage, wrt [#399](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/399)

## 0.1.18.0 -- 2022-07-30

* Fix tui set wrt [#266](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/266) by Arjun Kathuria
    - Ask the user to install the tool via prompt when setting an non-installed version
* improvements to safe (un-)installations
    - bindists that don't support `make DESTDIR=/some/tmp/dir install` are now unsupported
    - installed GHC files are now recorded to avoid use of `removePathForcibly`
    - internally uses a newtype wrapper for user-input paths and restrict destructive operations to validated paths
* Add `--disable-ld-override` for darwin bindists wrt #391
* Allow passing bindist configure args wrt #377
* use of `TMPDIR` is dropped... now uses an internal tmp dir `~/.ghcup/tmp`
* improvements to error handling and warnings
* Require --isolate to have an absolute directory, fixes #367
* Fix mingw PATH handling wrt #371
* Add --mingw-path switch to `ghcup run`
* Fix `ghcup run` on windows, fixes #375
* Improve `ghcup compile <hls|ghc>`
    - short hashes now work
    - print the long hash in addition to the detected version
* Improve `ghcup compile hls`
    - add `--git-describe-version` switch as an alternative to `--overwrite-version`
    - Allow to build HLS from hackage (now is the default)
	- Allow to run 'cabal update' automatically before the HLS build
	- Fix parser and completer for 'ghcup compile hls --version'
* Improve `ghcup compile ghc`
    - Allow to build from arbitrary GHC source dists

## 0.1.17.10 -- 2022-05-12

* windows hotfix (hackage-only release)

## 0.1.17.9 -- 2022-05-12

* broken sdist (hackage-only release)

## 0.1.17.8 -- 2022-05-11

* Fix a serious (but hard to trigger) bug when combining `--isolate <DIR>` with `--force`, please make sure to upgrade or avoid `--force`
* Fix HLS build not cleaning up properly on failed installations, fixes [#361](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/361)
* Fix parsing of symlinks with multiple slashes, wrt [#353](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/353)
* Re-enable upgrade functionality for all configurations wrt [MR #250](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/250) and [VSCode haskell issue #601](https://github.com/haskell/vscode-haskell/issues/601)
* Fix `ghcup run --ghc 8.10` (for short versions) wrt [#360](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/360)
    - this also introduces a `--quick` switch for `ghcup run`

## 0.1.17.7 -- 2022-04-21

* Fix `ghcup run` on windows wrt [#345](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/345)

## 0.1.17.6 -- 2022-03-18

* Vastly improve shell completions wrt [#242](https://gitlab.haskell.org/haskell/ghcup-hs/-/merge_requests/242)
* Fix 'ghcup install cabal/hls/stack --set' wrt [#324](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/324)
* Fix bad error message wrt [#323](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/323)
* Use predictable /tmp names for `ghcup run`, fixes [#329](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/329)
* Fix bug with isolated installation of not previously installed versions
* Add `--no-set` to install commands, fixes [#330](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/330)
* Fix serious bug in `ghcup list --raw-format -t <tool> -c installed`
* Overhaul metadata merging and add `ghcup config add-release-channel URI` wrt [#328](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/328)
* Fix max path issues on windows with `ghcup run`

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
