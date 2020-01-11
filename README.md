# ghcup

A rewrite of ghcup in haskell.

## TODO

* create static ghcup binaries
    * adjust url in GHCupDownloads
* add print-system-reqs command

## Motivation

Maintenance problems:

* platform incompatibilities regularly causing breaking bugs:
	* [Mktemp not working properly on macOS](https://gitlab.haskell.org/haskell/ghcup/issues/130)
	* [ln: illegal option -- T on macOS Catalina](https://gitlab.haskell.org/haskell/ghcup/issues/123)
	* [Wrong tar flag on darwin](https://gitlab.haskell.org/haskell/ghcup/issues/119))
* refactoring being difficult due to POSIX sh

Benefits of a rewrite:

* Features such as installing [release candidates](https://gitlab.haskell.org/haskell/ghcup/issues/94) or [HEAD builds](https://gitlab.haskell.org/haskell/ghcup/issues/65) can be more conveniently implemented in a rewrite
* Refactoring will be easier
* Better tool support (such as linting the downloads file)
* saner downloads file format (such as JSON)

Downsides:

* building static binaries for all platforms (and possibly causing SSL/DNS problems)
* still bootstrapping those binaries via a POSIX sh script

## Goals

* Correct low-level code
* Good exception handling
* Cleaner user interface
