# Revision history for ghcup

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
