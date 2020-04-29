# TODOs and Remarks

## Now

* ghcup init?
* merge two download files
* fetch/unpack functionality
* installing multiple versions of the same
* post-install

* proper test suite

* !! update of 0.1.5 must go in ghcup-0.0.1.json !!

* try to run exe before upgrade (backup to ~/.ghcup/bin/ghcup.old)
* stdout flushing?
* resume support (for make-install only)

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
