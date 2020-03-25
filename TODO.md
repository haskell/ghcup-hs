# TODOs and Remarks

## Now

* travis

* requirements
  * for ghcup (bootstrap script)
  * per tool
* mac build: xattr -cr .
* static binaries
  * upgrade plan from old ghcup

* bootstrap-haskell with new ghcup
  * add warning to ghcup script about new binary

* make sure smart-dl is not broken

* handle SIGINT better (remove dirs)

* review symlink handling (maybe fixed set of tools?)

## Maybe

* maybe: changelog          Show the changelog of a GHC release (online)
* sign the JSON? (Or check gpg keys?)
* testing (especially distro detection -> unit tests)

## Later

* add support for RC/alpha/HEAD versions

## Cleanups

* avoid alternative for IO
* use plucky or oops instead of Excepts

## Questions

* fully static musl builds for linux?
* mirror support
* interactive handling when distro doesn't exist and we know the tarball is incompatible?
* ghcup-with wrapper to execute a command with a given ghc in PATH?
