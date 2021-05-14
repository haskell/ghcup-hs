`ghcup` makes it easy to install specific versions of `ghc` on GNU/Linux,
macOS (aka Darwin) and FreeBSD and can also bootstrap a fresh Haskell developer environment from scratch.
It follows the unix UNIX philosophy of [do one thing and do it well](https://en.wikipedia.org/wiki/Unix_philosophy#Do_One_Thing_and_Do_It_Well).

Similar in scope to [rustup](https://github.com/rust-lang-nursery/rustup.rs), [pyenv](https://github.com/pyenv/pyenv) and [jenv](http://www.jenv.be).

## Table of Contents

   * [Installation](#installation)
     * [Simple bootstrap](#simple-bootstrap)
     * [Manual install](#manual-install)
     * [Vim integration](#vim-integration)
   * [Usage](#usage)
     * [Configuration](#configuration)
     * [Manpages](#manpages)
     * [Shell-completion](#shell-completion)
     * [Cross support](#cross-support)
     * [XDG support](#xdg-support)
     * [Env variables](#env-variables)
     * [Installing custom bindists](#installing-custom-bindists)
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

### Vim integration

See [ghcup.vim](https://github.com/hasufell/ghcup.vim).

## Usage

See `ghcup --help`.

For the simple interactive TUI, run:

```sh
ghcup tui
```

For the full functionality via cli:

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

GHCup works very well with [`cabal-install`](https://hackage.haskell.org/package/cabal-install), which
handles your haskell packages and can demand that [a specific version](https://cabal.readthedocs.io/en/latest/nix-local-build.html#cfg-flag---with-compiler)  of `ghc` is available, which `ghcup` can do.

### Configuration

A configuration file can be put in `~/.ghcup/config.yaml`. The default config file
explaining all possible configurations can be found in this repo: [config.yaml](./config.yaml).

Partial configuration is fine. Command line options always overwrite the config file settings.

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

### XDG support

To enable XDG style directories, set the environment variable `GHCUP_USE_XDG_DIRS` to anything.

Then you can control the locations via XDG environment variables as such:

* `XDG_DATA_HOME`: GHCs will be unpacked in `ghcup/ghc` subdir (default: `~/.local/share`)
* `XDG_CACHE_HOME`: logs and download files will be stored in `ghcup` subdir (default: `~/.cache`)
* `XDG_BIN_HOME`: binaries end up here (default: `~/.local/bin`)
* `XDG_CONFIG_HOME`: the config file is stored in `ghcup` subdir as `config.yaml` (default: `~/.config`)

### Env variables

This is the complete list of env variables that change GHCup behavior:

* `GHCUP_USE_XDG_DIRS`: see [XDG support](#xdg-support) above
* `TMPDIR`: where ghcup does the work (unpacking, building, ...)
* `GHCUP_INSTALL_BASE_PREFIX`: the base of ghcup (default: `$HOME`)
* `GHCUP_CURL_OPTS`: additional options that can be passed to curl
* `GHCUP_WGET_OPTS`: additional options that can be passed to wget
* `CC`/`LD` etc.: full environment is passed to the build system when compiling GHC via GHCup

### Installing custom bindists

There are a couple of good use cases to install custom bindists:

1. manually built bindists (e.g. with patches)
  - example: `ghcup install ghc -u 'file:///home/mearwald/tmp/ghc-eff-patches/ghc-8.10.2-x86_64-deb10-linux.tar.xz' 8.10.2-eff`
2. GHC head CI bindists
  - example: `ghcup install ghc -u 'https://gitlab.haskell.org/api/v4/projects/1/jobs/artifacts/master/raw/ghc-x86_64-fedora27-linux.tar.xz?job=validate-x86_64-linux-fedora27' head`
3. DWARF bindists
  - example: `ghcup install ghc -u 'https://downloads.haskell.org/~ghc/8.10.2/ghc-8.10.2-x86_64-deb10-linux-dwarf.tar.xz' 8.10.2-dwarf`

Since the version parser is pretty lax, `8.10.2-eff` and `head` are both valid versions
and produce the binaries `ghc-8.10.2-eff` and `ghc-head` respectively.
GHCup always needs to know which version the bindist corresponds to (this is not automatically
detected).

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

* Github action [haskell/actions/setup](https://github.com/haskell/actions/tree/main/setup)
* [vabal](https://github.com/Franciman/vabal)

## Known problems

### Custom ghc version names

When installing ghc bindists with custom version names as outlined in
[installing custom bindists](#installing-custom-bindists), then cabal might
be unable to find the correct `ghc-pkg` (also see [#73](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/73))
if you use `cabal build --with-compiler=ghc-foo`. Instead, point it to the full path, such as:
`cabal build --with-compiler=$HOME/.ghcup/ghc/<version-name>/bin/ghc` or set that GHC version
as the current one via: `ghcup set ghc <version-name>`.

This problem doesn't exist for regularly installed GHC versions.

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

We do.

3. Why the haskell reimplementation?

:-)
