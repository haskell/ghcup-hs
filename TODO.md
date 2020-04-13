# TODOs and Remarks

## Now

* ghcup migration
* update static links

* releases, update download info and bootstrap-haskell

## Maybe

* maybe: changelog          Show the changelog of a GHC release (online)
* sign the JSON? (Or check gpg keys?)
* testing (especially distro detection -> unit tests)

## Later

* i386 support
* add support for RC/alpha/HEAD versions

## Cleanups

* too many decodeutf8
* avoid alternative for IO
* use plucky or oops instead of Excepts

## Questions

* mirror support
* interactive handling when distro doesn't exist and we know the tarball is incompatible?
* ghcup-with wrapper to execute a command with a given ghc in PATH?
