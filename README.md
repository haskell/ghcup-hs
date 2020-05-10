`ghcup` makes it easy to install specific versions of `ghc` on GNU/Linux,
macOS (aka Darwin) and FreeBSD and can also bootstrap a fresh Haskell developer environment from scratch.
It follows the unix UNIX philosophy of [do one thing and do it well](https://en.wikipedia.org/wiki/Unix_philosophy#Do_One_Thing_and_Do_It_Well).

Similar in scope to [rustup](https://github.com/rust-lang-nursery/rustup.rs), [pyenv](https://github.com/pyenv/pyenv) and [jenv](http://www.jenv.be).

*Ubuntu users may prefer [hvr's ppa](https://launchpad.net/~hvr/+archive/ubuntu/ghc).*

## Table of Contents

   * [Installation](#installation)
   * [Usage](#usage)
     * [Manpages](#manpages)
     * [Shell-completion](#shell-completion)
     * [Cross support](#cross-support)
   * [Design goals](#design-goals)
   * [How](#how)
   * [Known users](#known-users)
   * [Known problems](#known-problems)
   * [FAQ](#faq)

## Installation

### Simple bootstrap

Follow the instructions at [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/)

### Manual install

Download the binary for your platform at [https://downloads.haskell.org/~ghcup/](https://downloads.haskell.org/~ghcup/)
and place it into your `PATH` anywhere.

Then adjust your `PATH` in `~/.bashrc` (or similar, depending on your shell) like so:

```sh
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
```

## Usage

See `ghcup --help`.

Common use cases are:

```sh
# list available ghc/cabal versions
ghcup list

# install the recommended GHC version
ghcup install ghc

# install a specific GHC version
ghcup install ghc 8.2.2

# set the currently "active" GHC version
ghcup set ghc 8.4.4

# install cabal-install
ghcup install cabal

# update ghcup itself
ghcup upgrade
```

Generally this is meant to be used with [`cabal-install`](https://hackage.haskell.org/package/cabal-install), which
handles your haskell packages and can demand that [a specific version](https://cabal.readthedocs.io/en/latest/nix-local-build.html#cfg-flag---with-compiler)  of `ghc` is available, which `ghcup` can do.

### Manpages

For man pages to work you need [man-db](http://man-db.nongnu.org/) as your `man` provider, then issue `man ghc`. Manpages only work for the currently set ghc.
`MANPATH` may be required to be unset.

### Shell-completion

Shell completions are in `shell-completions`.

For bash: install `shell-completions/bash`
as e.g. `/etc/bash_completion.d/ghcup` (depending on distro)
and make sure your bashrc sources the startup script
(`/usr/share/bash-completion/bash_completion` on some distros).

### Cross support

ghcup can compile and install a cross GHC for any target. However, this
requires that the build host has a complete cross toolchain and various
libraries installed for the target platform.

Consult the GHC documentation on the [prerequisites](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling#tools-to-install).
For distributions with non-standard locations of cross toolchain and
libraries, this may need some tweaking of `build.mk` or configure args.
See `ghcup compile ghc --help` for further information.

## Design goals

1. simplicity
2. non-interactive
3. portable (eh)
4. do one thing and do it well (UNIX philosophy)

### Non-goals

1. invoking `sudo`, `apt-get` or *any* package manager
2. handling system packages
3. handling cabal projects
4. being a stack alternative

## How

Installs a specified GHC version into `~/.ghcup/ghc/<ver>`, and places `ghc-<ver>` symlinks in `~/.ghcup/bin/`.

Optionally, an unversioned `ghc` link can point to a default version of your choice.

This uses precompiled GHC binaries that have been compiled on fedora/debian by [upstream GHC](https://www.haskell.org/ghc/download_ghc_8_6_1.html#binaries).

Alternatively, you can also tell it to compile from source (note that this might fail due to missing requirements).

In addition this script can also install `cabal-install`.

## Known users

* [vabal](https://github.com/Franciman/vabal)

## Known problems

### Limited distributions supported

Currently only GNU/Linux distributions compatible with the [upstream GHC](https://www.haskell.org/ghc/download_ghc_8_6_1.html#binaries) binaries are supported.

### Precompiled binaries

Since this uses precompiled binaries you may run into
several problems.

#### Missing libtinfo (ncurses)

You may run into problems with *ncurses* and **missing libtinfo**, in case
your distribution doesn't use the legacy way of building
ncurses and has no compatibility symlinks in place.

Ask your distributor on how to solve this or
try to compile from source via `ghcup compile <version>`.

#### Libnuma required

This was a [bug](https://ghc.haskell.org/trac/ghc/ticket/15688) in the build system of some GHC versions that lead to
unconditionally enabled libnuma support. To mitigate this you might have to install the libnuma
package of your distribution. See [here](https://gitlab.haskell.org/haskell/ghcup/issues/58) for a discussion.

### Compilation

Although this script can compile GHC for you, it's just a very thin
wrapper around the build system. It makes no effort in trying
to figure out whether you have the correct toolchain and
the correct dependencies. Refer to [the official docs](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Linux)
on how to prepare your environment for building GHC.

## FAQ

1. Why reimplement stack?

ghcup is not a reimplementation of stack. The only common part is automatic installation of GHC, but even that differs in scope and design.

2. Why not support windows?

Consider using [Chocolatey](https://chocolatey.org/search?q=ghc) or [ghcups](https://github.com/kakkun61/ghcups).

3. Why the haskell reimplementation?

Why not?
