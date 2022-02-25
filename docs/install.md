# Getting started

GHCup makes it easy to install specific versions of GHC on GNU/Linux,
macOS (aka Darwin), FreeBSD and Windows and can also bootstrap a fresh [Haskell developer environment](./install/#supported-tools) from scratch.
It follows the unix UNIX philosophy of [do one thing and do it well](https://en.wikipedia.org/wiki/Unix_philosophy#Do_One_Thing_and_Do_It_Well). Similar in scope to [rustup](https://github.com/rust-lang-nursery/rustup.rs), [pyenv](https://github.com/pyenv/pyenv) and [jenv](http://www.jenv.be).

## Installation

The following commands will download the `ghcup` binary into `~/.ghcup/bin` (or `C:\ghcup\bin` on windows) and then
run it to interactively install the [Haskell Toolchain](#supported-tools). These commands should be run as **non-root/non-admin
user**.

For Linux, macOS, FreeBSD or Windows Subsystem 2 for Linux, run this in a terminal:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

For Windows, run this in a PowerShell session:

```psh
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true
```

If you want to know what these scripts do, check out the [source code at the repository](https://gitlab.haskell.org/haskell/ghcup-hs/-/tree/master/scripts/bootstrap). Advanced users may want to perform a [manual installation](#manual-install) and GPG verify the binaries.

### Which versions get installed?

GHCup has two main channels for every tool: **recommended** and **latest**. By default, it installs *recommended*.

*latest* follows the latest release of every tool, while *recommended* is at the discretion of the GHCup maintainers and based on community adoption (hackage libraries, tools like HLS, stackage support, etc.) and known bugs.

Also see [tags and shortcuts](../guide/#tags-and-shortcuts) for more information.

## First steps

1. To get started with creating a Haskell project, follow the [Getting Started with Haskell and Cabal](https://cabal.readthedocs.io/en/stable/getting-started.html) guide
2. To learn Haskell, try any of those:
    - A beginner friendly [4-lectures course](https://github.com/haskell-beginners-2022/course-plan) with exercises (by [Kowainik](https://kowainik.github.io/))
    - An in-depth university [CIS 194 Haskell course](https://www.cis.upenn.edu/~cis194/spring13/) including exercises (by [Brent Yorgey](https://byorgey.wordpress.com/))
3. To learn more about Haskell Toolchain management, check out the [ghcup user guide](./guide.md)

## Uninstallation

On linux, just run `ghcup nuke`, then make sure any ghcup added lines in your `~/.bashrc` (or similar) are removed.

On windows, double-click on the `Uninstall Haskell.ps1` PowerShell script on your Desktop.

## Supported tools

GHCup supports the following tools, which are also known as the **Haskell Toolchain**:

1. [GHC](https://www.haskell.org/ghc/)
2. [cabal-install](https://cabal.readthedocs.io/en/stable/)
3. [haskell-language-server](https://haskell-language-server.readthedocs.io/en/stable/)
4. [stack](https://docs.haskellstack.org/en/stable/README/)

## Supported platforms

This list may not be exhaustive and specifies support for bindists only.

| Platform | Architecture | ghcup | GHC | cabal | HLS | stack | 
| ------ | ------ | ------ | ------ | ------ | ------ | ------ |
| Windows 7 | amd64 | ❔ | ✅ | ✅ | ✅ | ✅ |
| Windows 10 | amd64 | ✅ | ✅ | ✅ | ✅ | ✅ |
| Windows Server 2016 | amd64 | ✅ | ✅ | ✅ | ✅ | ✅ |
| Windows Server 2019 | amd64 | ✅ | ✅ | ✅ | ✅ | ✅ |
| Windows Server 2022 | amd64 | ✅ | ✅ | ✅ | ✅ | ✅ |
| Windows WSL1 | amd64 | ❌ | ❔ | ❔ | ❔ | ❔ |
| Windows WSL2 | amd64 | ✅ | ✅ | ✅ | ✅ | ✅ |
| MacOS >=10.13 | amd64 | ✅ | ✅ | ✅ | ✅ | ✅ |
| MacOS <10.13 | amd64 | ❌ | ❔ | ❔ | ❔ | ❔ |
| MacOS | aarch64 | ✅ | ✅ | ✅ | ⚠️ | ❌ |
| FreeBSD | amd64 | ✅ | ⚠️ | ✅ | ⚠️ | ❌ |
| Linux generic | x86 | ✅ | ✅ | ✅ | ✅ | ✅ |
| Linux generic | amd64 | ✅ | ✅ | ✅ | ✅ | ✅ |
| Linux generic | aarch64 | ✅ | ⚠️ | ✅ | ⚠️ | ❌ |
| Linux generic | armv7 | ✅ | ⚠️ | ✅ | ⚠️ | ❌ |

### Windows 7

May or may not work, several issues:

* [https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/140](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/140)
* [https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/197](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/197)

### WSL1

Unsupported. GHC may or may not work. Upgrade to WSL2.

### MacOS <10.13

Not supported. Would require separate binaries, since >=10.13 binaries are incompatible.
Please upgrade.

### MacOS aarch64

HLS bindists are still experimental. Stack has only unofficial binaries for this platform.
There are various issues with GHC itself.

### FreeBSD

Lacks some upstream bindists and may need compat libs, since most bindists are built on FreeBSD-12.
HLS bindists are experimental.

### Linux ARMv7/AARCH64

Lower availability of bindists. Stack and HLS binaries are experimental.

## Manual install

Download the binary for your platform at [https://downloads.haskell.org/~ghcup/](https://downloads.haskell.org/~ghcup/)
and place it into your `PATH` anywhere.

If you want to GPG verify the binaries, import the following key first: `7784930957807690A66EBDBE3786C5262ECB4A3F`.

Then adjust your `PATH` in `~/.bashrc` (or similar, depending on your shell) like so:

```sh
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
```

## Vim integration

See [ghcup.vim](https://github.com/hasufell/ghcup.vim).

## Get help

* [Libera IRC chat on #haskell-ghcup or #haskell](https://kiwiirc.com/nextclient/irc.libera.chat/?nick=Guest%7C?#haskell,#haskell-ghcup)
* [GHCup issue tracker](https://gitlab.haskell.org/haskell/ghcup-hs/issues)
* [Matrix](https://app.element.io/#/room/#haskell-tooling:matrix.org)
* [Discord](https://discord.gg/pKYf3zDQU7)

