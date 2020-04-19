# TODOs and Remarks

## Now

* try to run exe before upgrade (backup to ~/.ghcup/bin/ghcup.old)
* allow to build 8.8
* curl DL does not cache json
* explain environment variables
* add --keep=<always|error> option

* allow to switch to curl/wget at runtime

* cross support
* installing multiple versions of the same

* proper test suite
* add more logging


## Maybe

* version ranges in json
* sign the JSON? (Or check gpg keys?)
* testing (especially distro detection -> unit tests)

## Later

* add support for RC/alpha/HEAD versions

## Cleanups

* avoid alternative for IO
* use plucky or oops instead of Excepts

## Questions

* move out GHCup.Version module, bc it's not library-ish?
* mirror support
* interactive handling when distro doesn't exist and we know the tarball is incompatible?
* ghcup-with wrapper to execute a command with a given ghc in PATH?
