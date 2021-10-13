# About

All you wanted to know about GHCup.

## Team

### Author and Maintainer

* [Julian Ospald](https://github.com/hasufell) (aka: maerwald, hasufell)

### Collaborators

* [Arjun Kathuria](https://github.com/arjunkathuria)
* [Ben Gamari](https://github.com/bgamari)
* [Javier Neira](https://github.com/jneira)

### Contributors

* amesgen
* Chris Smith
* Anton-Latukha
* Brian McKenna
* Huw campbell
* Tom Ellis
* Sigmund Vestergaard
* Ron Toland
* Paolo Martini
* Mario Lang
* Jan Hrček
* vglfr
* Fendor
* Enrico Maria De Angelis
* Emily Pillmore
* Colin Barrett
* Artur Gajowy

### Sponsors

* All [opencollective](https://opencollective.com/ghcup#category-CONTRIBUTE) contributors
* [haskell.org](https://www.haskell.org/haskell-org-committee/) via CI and infrastructure
* [Haskell Foundation](https://haskell.foundation/affiliates/) via affiliation

## How to help

* if you want to contribute code or documentation, check out the [issue tracker](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues) and the [Development guide](./dev.md)
* if you want to propose features or write user feedback, feel free to [open a ticket](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/new?issue)
* if you want to donate to the project, visit our [opencollective](https://opencollective.com/ghcup#category-CONTRIBUTE) page

## Design goals

1. simplicity
2. non-interactive CLI interface
3. portable
4. do one thing and do it well (UNIX philosophy)

## Non-goals

1. invoking `sudo`, `apt-get` or *any* package manager
2. handling system packages
3. handling cabal projects
4. being a stack alternative

## How

Installs a specified GHC version into `~/.ghcup/ghc/<ver>`, and places `ghc-<ver>` symlinks in `~/.ghcup/bin/`.

Optionally, an unversioned `ghc` link can point to a default version of your choice.

This uses precompiled GHC binaries that have been compiled on fedora/debian by [upstream GHC](https://www.haskell.org/ghc/download_ghc_8_6_1.html#binaries).

Alternatively, you can also tell it to compile from source (note that this might fail due to missing requirements).

cabal-install/HLS/stack are installed in `~/.ghcup/bin/<tool>-<ver>` and have unversioned symlinks to the latest version by default (`~/.ghcup/bin/<tool>-<ver>`).

## Known users

* Github actions:
	- [actions/virtual-environments](https://github.com/actions/virtual-environments)
	- [haskell/actions/setup](https://github.com/haskell/actions/tree/main/setup)
* mirrors:
	- [sjtug](https://mirror.sjtu.edu.cn/docs/ghcup)
* tools:
	- [vabal](https://github.com/Franciman/vabal)

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

### Stack support

There may be a number of bugs when trying to make ghcup installed GHC versions work with stack,
such as:

- https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/188

Further, stack's upgrade procedure may break/confuse ghcup. There are a number of integration
issues discussed here:

- https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/153

### Windows support

Windows support is in early stages. Since windows doesn't support symbolic links properly,
ghcup uses a [shimgen wrapper](https://github.com/71/scoop-better-shimexe). It seems to work
well, but there may be unknown issues with that approach.

Windows 7 and Powershell 2.0 aren't well supported at the moment, also see:

- https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/140
- https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/197

## FAQ

### Why reimplement stack?

GHCup is not a reimplementation of stack. The only common part is automatic installation of GHC,
but even that differs in scope and design.

### Why should I use ghcup over stack?

GHCup is not a replacement for stack. Instead, it supports installing and managing stack versions.
It does the same for cabal, GHC and HLS. As such, It doesn't make a workflow choice for you.

### Why should I let ghcup manage stack?

You don't need to. However, some users seem to prefer to have a central tool that manages cabal and stack
at the same time. Additionally, it can allow better sharing of GHC installation across these tools.
Also see:

* https://docs.haskellstack.org/en/stable/yaml_configuration/#system-ghc
* https://github.com/commercialhaskell/stack/pull/5585

### Why does ghcup not use stack code?

1. GHCup started as a shell script. At the time of rewriting it in Haskell, the authors didn't even know that stack exposes *some* of its [installation API](https://hackage.haskell.org/package/stack-2.5.1.1/docs/Stack-Setup.html)
2. it doesn't support cabal installation, which was the main motivation behind GHCup back then
3. depending on a codebase as big as stack for a central part of one's application without having a short contribution pipeline would likely have caused stagnation or resulted in simply copy-pasting the relevant code in order to adjust it
4. it's not clear how GHCup would have been implemented with the provided API. It seems the codebases are fairly different. GHCup does a lot of symlink handling to expose a central `bin/` directory that users can easily put in PATH, without having to worry about anything more. It also provides explicit removal functionality, GHC cross-compilation, a TUI, etc etc.

### Why not unify...

#### ...stack and Cabal and do away with standalone installers

GHCup is not involved in such decisions. cabal-install and stack might have a
sufficiently different user experience to warrant having a choice.

#### ...installer implementations and have a common library

This sounds like an interesting goal. However, GHC installation isn't a hard engineering problem
and the shared code wouldn't be too exciting. For such an effort to make sense, all involved
parties would need to collaborate and have a short pipeline to get patches in.

It's true this would solve the integration problem, but following unix principles, we can
do similar via **hooks**. Both cabal and stack can support installation hooks. These hooks
can then call into ghcup or anything else, also see:

* https://github.com/haskell/cabal/issues/7394
* https://github.com/commercialhaskell/stack/pull/5585

#### ...installers (like, all of it)

So far, there hasn't been an open discussion about this. Is this even a good idea?
Sometimes projects converge eventually if their overlap is big enough, sometimes they don't.

While unification sounds like a simplification of the ecosystem, it also takes away choice.
Take `curl` and `wget` as an example.

### Why not support windows?

Windows is supported since GHCup version 0.1.15.1.

### Why the haskell reimplementation?

GHCup started as a portable posix shell script of maybe 50 LOC. GHC installation itself can be carried out in
about ~3 lines of shell code (download, unpack , configure+make install). However, much convenient functionality
has been added since, as well as ensuring that all operations are safe and correct. The shell script ended up with
over 2k LOC, which was very hard to maintain.

The main concern when switching from a portable shell script to haskell was platform/architecture support.
However, ghcup now re-uses GHCs CI infrastructure and as such is perfectly in sync with all platforms that
GHC supports.
