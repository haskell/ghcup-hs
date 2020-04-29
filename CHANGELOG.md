# Revision history for ghcup

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
