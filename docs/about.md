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

## Project ownership and hierarchy

The project at the time of writing (2024-04-22) follows the model of
[benevolent dictator for life](https://en.wikipedia.org/wiki/Benevolent_dictator_for_life), which is
Julian Ospald at the moment.

Ideally, the ownership should be shared across a core team of collaborators sharing the same vision and
engagement in the future.

I do not believe in people making decisions over projects they are barely involved in. Instead I believe
in listening to end users needs very carefully and making decisions based on that. People who want a direct
influence on the decision process have to both demonstrate they share the vision of GHCup and do the actual
work.

"Work" here doesn't have to be "writing code". There are many ways to be engaged in a project.

### Transition plan in case of maintainer absence

In case the current maintainer of GHCup (Julian Ospald) is unreachable for a prolonged period of time (3 months),
the ownership of this project will be automatically transferred to the following individuals:

- [Moritz Angerman](https://github.com/angerman)
- [Andrew Lelechenko](https://github.com/Bodigrim)

They will be tasked with finding new maintainers in whatever way they see fit (be it appointing themselves or asking HF for help).

The appointed owners may choose to stay owners after the transition period (whether it's in a passive or active capacity) or
fully transfer ownership to someone else or an organization.

The community shall be informed about this process.

If not otherwise specified by the newly appointed owners, the following principles shall apply to the *transition period*:

#### During the transition period

During the transition period, no other individual or organization is allowed to drive changes to
[ghcup-hs](https://github.com/haskell/ghcup-hs) repository, unless they are explicitly allowed to do so by the appointed owners.

The following people (in addition to the owners) shall have full write access to the
[ghcup-metadata](https://github.com/haskell/ghcup-metadata) repository
(all files) for the length of the transition period, unless otherwise specified by the appointed owners:

- [Ben Gamari](https://github.com/bgamari)
- [Hécate Moonlight](https://github.com/Kleidukos)
- [Mike Pilgrem](https://github.com/mpilgrem)
- [Jens Petersen](https://github.com/juhp)

Contributions to the metadata are expected to follow a review process. If that turns out to be impractical due to lack of engagement, a wait
time of 2 days before merging shall be followed anyway, except for the `-vanilla` files, which may be merged at any time.

#### Access

The GHCup website, various scripts and unofficial bindists are hosted on haskell.org infrastructure. Contact the
[Haskell.org committee](https://www.haskell.org/haskell-org-committee/) for access.

The backup owners should already have admin rights on the GHCup repositories, which are hosted on the
[Github haskell namespace](https://github.com/haskell). In case of issues contact one of the
organization admins for access, e.g.:

- [Andrew Lelechenko](https://github.com/orgs/haskell/people/Bodigrim)
- [gbaz](https://github.com/orgs/haskell/people/gbaz)
- [Hécate Moonlight](https://github.com/Kleidukos)
- [davean](https://github.com/orgs/haskell/people/davean)
- [chessai](https://github.com/orgs/haskell/people/chessai)

The owners should already have access to the [hackage package](https://hackage.haskell.org/package/ghcup/maintainers/).
In case of issues contact the [hackage trustees](https://github.com/haskell-infra/hackage-trustees).

#### Private runners

Private runners maintained by Julian Ospald may cease to work. Moritz Angerman will have SSH access to the machines.
However, no one will have access to the Hetzner account and billing information. As such, those runners will simply
have to be replaced.

## How to help

* if you want to contribute code or documentation, check out the [issue tracker](https://github.com/haskell/ghcup-hs/issues) and the [Development guide](./dev.md)
* if you want to propose features or write user feedback, feel free to [open a ticket](https://github.com/haskell/ghcup-hs/issues/new)
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

## Distribution policies

Like most Linux distros and other distribution channels, GHCup also
follows certain policies. These are as follows:

1. The end-user experience is our primary concern
    - ghcup in CI systems as a use case is a first class citizen
2. We strive to collaborate with all maintainers of all the tools we support and maintain a good relationship
3. We may fix build system or other distribution bugs in upstream bindists
    - these are always communicated upstream
4. We may even patch source code of supported tools in very rare cases if that is required to ensure that the end-user experience does not break
    - we'll first try to upstream any such required patch and request a new release to avoid downstream patching
    - patches will be communicated to the maintainers either way and we'll strive to get their review
    - they will also be communicated to the end-user
    - they will be uploaded along with the bindist
    - we will avoid maintaining long-running downstream patches (currently zero)
5. We may add bindists for platforms that upstream does not support
    - this is currently the case for GHC for e.g. Alpine and possibly FreeBSD in the future
    - this is currently also the case for stack on darwin M1
    - we don't guarantee for unofficial bindists that the test suite passes at the moment (this may change in the future)
6. We GPG sign all the GHCup metadata as well as the unofficial bindists
    - any trust issues relating to missing checksums or GPG signatures is a bug and given high priority

## How

Installs a specified GHC version into `~/.ghcup/ghc/<ver>`, and places `ghc-<ver>` symlinks in `~/.ghcup/bin/`.

Optionally, an unversioned `ghc` link can point to a default version of your choice.

This uses precompiled GHC binaries that have been compiled on fedora/debian by [upstream GHC](https://www.haskell.org/ghc/download_ghc_8_6_1.html#binaries).

Alternatively, you can also tell it to compile from source (note that this might fail due to missing requirements).

cabal-install/HLS/stack are installed in `~/.ghcup/bin/<tool>-<ver>` and have unversioned symlinks to the latest version by default (`~/.ghcup/bin/<tool>-<ver>`).

## Known users

* CI:
    - [Github actions/virtual-environments](https://github.com/actions/virtual-environments)
    - [Github haskell/actions/setup](https://github.com/haskell/actions/tree/main/setup)
    - [haskell-ci](https://github.com/haskell-CI/haskell-ci)
* mirrors:
    - [sjtug](https://mirror.sjtu.edu.cn/docs/ghcup)
* tools:
    - [vscode-haskell](https://github.com/haskell/vscode-haskell)
    - [nvim-lsp-installer](https://github.com/williamboman/nvim-lsp-installer)
    - [vabal](https://github.com/Franciman/vabal)

## Known problems

### Custom ghc version names

When installing ghc bindists with custom version names as outlined in
[installing custom bindists](guide.md#installing-custom-bindists), then cabal might
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

### Is ghcup really the main installer?

This is based on the Haskell survey results from 2022, which show that more
than half of survey participants use GHCup: https://taylor.fausak.me/2022/11/18/haskell-survey-results/

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

* https://docs.haskellstack.org/en/stable/configure/yaml/non-project/#system-ghc
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
