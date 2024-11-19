# Installation

GHCup makes it easy to install specific versions of GHC on GNU/Linux,
macOS (aka Darwin), FreeBSD and Windows and can also bootstrap a fresh [Haskell developer environment](#supported-tools) from scratch.
It follows the UNIX philosophy of [do one thing and do it well](https://en.wikipedia.org/wiki/Unix_philosophy#Do_One_Thing_and_Do_It_Well). Similar in scope to [rustup](https://github.com/rust-lang-nursery/rustup.rs), [pyenv](https://github.com/pyenv/pyenv) and [jenv](http://www.jenv.be).

## How to install

The following commands will download the `ghcup` binary into `~/.ghcup/bin` (or `C:\ghcup\bin` on windows) and then
run it to interactively install the [Haskell Toolchain](#supported-tools). These commands should be run as **non-root/non-admin
user**.

For Linux, macOS, FreeBSD or Windows Subsystem 2 for Linux, run this in a terminal:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

For Windows, run this in a PowerShell session:

```psh
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

There's also a [youtube video](https://www.youtube.com/watch?v=bB4fmQiUYPw) explaining installation on windows.

If you want to know what these scripts do, check out the [source code at the repository](https://github.com/haskell/ghcup-hs/tree/master/scripts/bootstrap). Advanced users may want to perform a [manual installation](#manual-installation) and GPG verify the binaries.

### Which versions get installed?

GHCup has two main channels for every tool: **recommended** and **latest**. By default, it installs *recommended*.

*latest* follows the latest release of every tool, while *recommended* is at the discretion of the GHCup maintainers and based on community adoption (hackage libraries, tools like HLS, stackage support, etc.) and known bugs.

Also see [tags and shortcuts](guide.md#tags-and-shortcuts) for more information.

## System requirements

### Linux Debian

#### Generic

The following distro packages are required: `build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`

#### Version >= 11 && <= 12

The following distro packages are required: `build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`

#### Version >= 12

The following distro packages are required: `build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`

### Linux Ubuntu

#### Generic

The following distro packages are required: `build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`

#### Version >= 20.04 && < 20.10

The following distro packages are required: `build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`

#### Version >= 20.10 && < 23

The following distro packages are required: `build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`

#### Version >= 23

The following distro packages are required: `build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev`

### Linux Fedora

#### Generic

The following distro packages are required: `gcc gcc-c++ gmp gmp-devel make ncurses ncurses-compat-libs xz perl`


### Linux CentOS

#### Generic

The following distro packages are required: `gcc gcc-c++ gmp gmp-devel make ncurses ncurses-compat-libs xz perl`

#### Version >= 7 && < 8

The following distro packages are required: `gcc gcc-c++ gmp gmp-devel make ncurses xz perl`


### Linux Alpine

#### Generic

The following distro packages are required: `binutils-gold curl gcc g++ gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl tar xz`


### Linux (generic)

#### Generic

You need the following packages: curl g++ gcc gmp make ncurses realpath xz-utils. Consult your distro documentation on the exact names of those packages.

### Darwin

#### Generic

On OS X, in the course of running ghcup you will be given a dialog box to install the command line tools. Accept and the requirements will be installed for you. You will then need to run the command again.
On Darwin M1 you might also need a working llvm installed (e.g. via brew) and have the toolchain exposed in PATH.

### FreeBSD

#### Generic

The following distro packages are required: `curl gcc gmp gmake ncurses perl5 libffi libiconv`


### Windows

#### Generic

On Windows, msys2 should already have been set up during the installation, so most users should just proceed. If you are installing manually, make sure to have a working mingw64 toolchain and shell.

## Next steps

1. Follow the [First steps guide](steps.md) on how to build a "Hello world" program, use `ghc`, run an interactive REPL and create a Haskell project
2. To understand the difference and overlap of `stack` and `cabal`, read on [here](https://gist.github.com/merijn/8152d561fb8b011f9313c48d876ceb07)
3. To learn Haskell proper check out the links at [How to learn Haskell proper](steps.md#how-to-learn-haskell-proper)
4. To learn more about Haskell Toolchain management, check out the [ghcup user guide](./guide.md)

## Uninstallation

On linux, just run `ghcup nuke`, then make sure any ghcup added lines in your `~/.bashrc` (or similar) are removed.

On windows, right click on the `Uninstall Haskell.ps1` PowerShell script on your Desktop and select *Run with PowerShell*.

## Supported tools

GHCup supports the following tools, which are also known as the **Haskell Toolchain**:

<details> <summary>Show all supported <a href='https://www.haskell.org/ghc/'>GHC</a> versions</summary>
<table>
<thead><tr><th>GHC Version</th><th>Tags</th></tr></thead>
<tbody>
<tr><td>9.10.1</td><td><span style="color:blue">latest</span>, base-4.20.0.0</td></tr>
<tr><td>9.8.2</td><td>base-4.19.1.0</td></tr>
<tr><td>9.8.1</td><td>base-4.19.0.0</td></tr>
<tr><td>9.6.6</td><td>base-4.18.2.1</td></tr>
<tr><td>9.6.5</td><td>base-4.18.2.1</td></tr>
<tr><td>9.6.4</td><td>base-4.18.2.0</td></tr>
<tr><td>9.6.3</td><td>base-4.18.1.0</td></tr>
<tr><td>9.6.2</td><td>base-4.18.0.0</td></tr>
<tr><td>9.6.1</td><td>base-4.18.0.0</td></tr>
<tr><td>9.4.8</td><td><span style="color:green">recommended</span>, base-4.17.2.1</td></tr>
<tr><td>9.4.7</td><td>base-4.17.2.0</td></tr>
<tr><td>9.4.6</td><td>base-4.17.2.0</td></tr>
<tr><td>9.4.5</td><td>base-4.17.1.0</td></tr>
<tr><td>9.4.4</td><td>base-4.17.0.0</td></tr>
<tr><td>9.4.3</td><td>base-4.17.0.0</td></tr>
<tr><td>9.4.2</td><td>base-4.17.0.0</td></tr>
<tr><td>9.4.1</td><td>base-4.17.0.0</td></tr>
<tr><td>9.2.8</td><td>base-4.16.4.0</td></tr>
<tr><td>9.2.7</td><td>base-4.16.4.0</td></tr>
<tr><td>9.2.6</td><td>base-4.16.4.0</td></tr>
<tr><td>9.2.5</td><td>base-4.16.4.0</td></tr>
<tr><td>9.2.4</td><td>base-4.16.3.0</td></tr>
<tr><td>9.2.3</td><td>base-4.16.2.0</td></tr>
<tr><td>9.2.2</td><td>base-4.16.1.0</td></tr>
<tr><td>9.2.1</td><td>base-4.16.0.0</td></tr>
<tr><td>9.0.2</td><td>base-4.15.1.0</td></tr>
<tr><td>9.0.1</td><td>base-4.15.0.0</td></tr>
<tr><td>8.10.7</td><td>base-4.14.3.0</td></tr>
<tr><td>8.10.6</td><td>base-4.14.3.0</td></tr>
<tr><td>8.10.5</td><td>base-4.14.2.0</td></tr>
<tr><td>8.10.4</td><td>base-4.14.1.0</td></tr>
<tr><td>8.10.3</td><td>base-4.14.1.0</td></tr>
<tr><td>8.10.2</td><td>base-4.14.1.0</td></tr>
<tr><td>8.10.1</td><td>base-4.14.0.0</td></tr>
<tr><td>8.8.4</td><td>base-4.13.0.0</td></tr>
<tr><td>8.8.3</td><td>base-4.13.0.0</td></tr>
<tr><td>8.8.2</td><td>base-4.13.0.0</td></tr>
<tr><td>8.8.1</td><td>base-4.13.0.0</td></tr>
<tr><td>8.6.5</td><td>base-4.12.0.0</td></tr>
<tr><td>8.6.4</td><td>base-4.12.0.0</td></tr>
<tr><td>8.6.3</td><td>base-4.12.0.0</td></tr>
<tr><td>8.6.2</td><td>base-4.12.0.0</td></tr>
<tr><td>8.6.1</td><td>base-4.12.0.0</td></tr>
<tr><td>8.4.4</td><td>base-4.11.1.0</td></tr>
<tr><td>8.4.3</td><td>base-4.11.1.0</td></tr>
<tr><td>8.4.2</td><td>base-4.11.1.0</td></tr>
<tr><td>8.4.1</td><td>base-4.11.0.0</td></tr>
<tr><td>8.2.2</td><td>base-4.10.1.0</td></tr>
<tr><td>8.0.2</td><td>base-4.9.1.0</td></tr>
<tr><td>7.10.3</td><td>base-4.8.2.0</td></tr>
</tbody>
</table>
</details>

<details> <summary>Show all supported <a href='https://cabal.readthedocs.io/en/stable/'>cabal-install</a> versions</summary>
<table>
<thead><tr><th>Cabal Version</th><th>Tags</th></tr></thead>
<tbody>
<tr><td>3.12.1.0</td><td><span style="color:blue">latest</span></td></tr>
<tr><td>3.10.3.0</td><td><span style="color:green">recommended</span></td></tr>
<tr><td>3.10.2.1</td><td></td></tr>
<tr><td>3.10.2.0</td><td></td></tr>
<tr><td>3.10.1.0</td><td></td></tr>
<tr><td>3.8.1.0</td><td></td></tr>
<tr><td>3.6.2.0-p1</td><td></td></tr>
<tr><td>3.6.2.0</td><td></td></tr>
<tr><td>3.6.0.0</td><td></td></tr>
<tr><td>3.4.1.0</td><td></td></tr>
<tr><td>3.4.0.0</td><td></td></tr>
<tr><td>3.2.0.0</td><td></td></tr>
<tr><td>3.0.0.0</td><td></td></tr>
<tr><td>2.4.1.0</td><td></td></tr>
</tbody>
</table>
</details>

<details> <summary>Show all supported <a href='https://haskell-language-server.readthedocs.io/en/stable/'>HLS</a> versions</summary>
<table>
<thead><tr><th>HLS Version</th><th>Tags</th></tr></thead>
<tbody>
<tr><td>2.9.0.1</td><td><span style="color:blue">latest</span>, <span style="color:green">recommended</span></td></tr>
<tr><td>2.9.0.0</td><td></td></tr>
<tr><td>2.8.0.0</td><td></td></tr>
<tr><td>2.7.0.0</td><td></td></tr>
<tr><td>2.6.0.0</td><td></td></tr>
<tr><td>2.5.0.0</td><td></td></tr>
<tr><td>2.4.0.0</td><td></td></tr>
<tr><td>2.3.0.0</td><td></td></tr>
<tr><td>2.2.0.0</td><td></td></tr>
<tr><td>2.1.0.0</td><td></td></tr>
<tr><td>2.0.0.1</td><td></td></tr>
<tr><td>2.0.0.0</td><td></td></tr>
<tr><td>1.10.0.0</td><td></td></tr>
<tr><td>1.9.1.0</td><td></td></tr>
<tr><td>1.9.0.0</td><td></td></tr>
<tr><td>1.8.0.0</td><td></td></tr>
<tr><td>1.7.0.0</td><td></td></tr>
<tr><td>1.6.1.0</td><td></td></tr>
<tr><td>1.6.0.0</td><td></td></tr>
<tr><td>1.5.1</td><td></td></tr>
<tr><td>1.5.0</td><td></td></tr>
<tr><td>1.4.0</td><td></td></tr>
<tr><td>1.3.0</td><td></td></tr>
<tr><td>1.2.0</td><td></td></tr>
<tr><td>1.1.0</td><td></td></tr>
</tbody>
</table>
</details>

<details> <summary>Show all supported <a href='https://docs.haskellstack.org/en/stable/README/'>Stack</a> versions</summary>
<table>
<thead><tr><th>Stack Version</th><th>Tags</th></tr></thead>
<tbody>
<tr><td>3.1.1</td><td><span style="color:blue">latest</span>, <span style="color:green">recommended</span></td></tr>
<tr><td>2.15.7</td><td></td></tr>
<tr><td>2.15.5</td><td></td></tr>
<tr><td>2.15.3</td><td></td></tr>
<tr><td>2.15.1</td><td></td></tr>
<tr><td>2.13.1</td><td></td></tr>
<tr><td>2.11.1</td><td></td></tr>
<tr><td>2.9.3</td><td></td></tr>
<tr><td>2.9.1</td><td></td></tr>
<tr><td>2.7.5</td><td></td></tr>
<tr><td>2.7.3</td><td></td></tr>
<tr><td>2.7.1</td><td></td></tr>
<tr><td>2.5.1</td><td></td></tr>
</tbody>
</table>
</details>

## Supported platforms

This list may not be exhaustive and specifies support for bindists only.

| Platform | Architecture | ghcup | GHC | cabal | HLS | stack |
| ------ | ------ | ------ | ------ | ------ | ------ | ------ |
| Windows 8.1 | amd64 | ✅ | ✅ | ✅ | ✅ | ✅ |
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

### Windows <8.1

No longer supported for recent GHCs, according to manual testing of GHC 9.8.1 on Windows 7.
According to [msys2 documentation](https://www.msys2.org/docs/windows_support), the minimum Windows
version is now 8.1.

### WSL1

Unsupported. GHC may or may not work. Upgrade to WSL2.

### MacOS <10.13

Not supported. Would require separate binaries, since >=10.13 binaries are incompatible.
Please upgrade.

### MacOS aarch64

HLS bindists are still experimental. Stack has only unofficial binaries for this platform.
There are various issues with GHC itself.

### FreeBSD

Lacks some upstream bindists and may need compat libs (such as `misc/compat12x`).
HLS bindists are experimental.
Only latest FreeBSD is generally supported.

### Linux ARMv7/AARCH64

Lower availability of bindists. Stack and HLS binaries are experimental.

## Manual installation

### Unix

Download the binary for your platform at [https://downloads.haskell.org/~ghcup/](https://downloads.haskell.org/~ghcup/)
and place it into your `PATH` anywhere.

If you want to GPG verify the binaries, import the following keys first:

```sh
gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C
gpg --batch --keyserver keyserver.ubuntu.com --recv-keys FE5AB6C91FEA597C3B31180B73EDE9E8CFBAEF01
gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4
gpg --batch --keyserver keyserver.ubuntu.com --recv-keys EAF2A9A722C0C96F2B431CA511AAD8CEDEE0CAEF
```

Then adjust your `PATH` in `~/.bashrc` (or similar, depending on your shell) like so:

```sh
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
```

### Windows

1. Install ghcup binary
    - choose a base directory for installation, e.g. `C:\` that has sufficient space
    - then create the directory, e.g. `C:\ghcup\bin`
    - download the binary: https://downloads.haskell.org/~ghcup/x86_64-mingw64-ghcup.exe
    - place it as `ghcup.exe` into e.g. `C:\ghcup\bin`
2. Install MSYS2
    - download https://repo.msys2.org/distrib/msys2-x86_64-latest.exe and execute it
    - remember the installation destination you choose (default is `C:\msys64`)
    - finish the installation
* Add environment variables and update `Path`
    - open search bar and type in "Edit the system environment variables", then open it
    - click on "Environment Variables..." at the near bottom
    - in the upper half, select `Path` variable and double click on it
    - in the new window, click "New", type in `C:\ghcup\bin` (depending on step 1.) and press enter
    - click "OK" at the bottom
    - in the upper half, click on "New..."
    - enter `GHCUP_MSYS2` under "Variable name" and the installation destination from step 2. under "Variable value"
    - click "OK" at the bottom
    - in the upper half, click on "New..."
    - enter `GHCUP_INSTALL_BASE_PREFIX` under "Variable name" and based on the installation destination from step 1. enter the device directory (default `C:\`)
    - click "OK" at the bottom
    - in the upper half, click on "New..."
    - enter `CABAL_DIR` under "Variable name" and based on the installation destination from step 1. enter the device directory + `cabal` subdir (default `C:\cabal`)
    - click "OK" at the bottom
    - click "OK" at the bottom
    - click "OK" at the bottom
3. Install tools
    - open powershell
    - run `ghcup install ghc --set recommended`
    - run `ghcup install cabal latest`
    - run `ghcup install stack latest`
    - run `ghcup install hls latest`
    - run `cabal update`
4. Update msys2
    - run `ghcup run -m -- pacman --noconfirm -Syuu`
    - run `ghcup run -m -- pacman --noconfirm -Syuu`
    - run `ghcup run -m -- pacman --noconfirm -S --needed curl autoconf mingw-w64-x86_64-pkgconf`
    - run `ghcup run -m -- pacman --noconfirm -S ca-certificates`
5. Update cabal config
    - go to e.g. `C:\cabal` (based on device you picked in 1.)
    - open file `config`
    - uncomment `extra-include-dirs` (the `-- `) and add the value (depending on installation destination you chose in 2.), e.g. `C:\msys64\mingw64\include`... so the final line should be `extra-include-dirs: C:\msys64\mingw64\include`
    - uncomment `extra-lib-dirs` and do the same, adding `C:\msys64\mingw64\lib`
    - uncomment `extra-prog-path` and set it to `C:\ghcup\bin, C:\cabal\bin, C:\msys64\mingw64\bin, C:\msys64\usr\bin`, depending on your install destinations from 1. and 2.
6. Set up msys2 shell
    - run `ghcup run -m -- sed -i -e 's/db_home:.*$/db_home: windows/' /etc/nsswitch.conf` to make the HOME in your msys2 shell match the one from windows
    - make a desktop shortcut from `C:\msys64\msys2_shell.cmd`, which will allow you to start a proper msys2 shell
    - run `ghcup run -m -- sed -i -e 's/#MSYS2_PATH_TYPE=.*/MSYS2_PATH_TYPE=inherit/' /c/msys64/msys2.ini`
    - run `ghcup run -m -- sed -i -e 's/rem set MSYS2_PATH_TYPE=inherit/set MSYS2_PATH_TYPE=inherit/' /c/msys64/msys2_shell.cmd`

All set. You can run `cabal init` now in an empty directory to start a project.

## Esoteric distros

### Void Linux

Since void linux can be installed with glibc and musl, it's hard to support correctly with ghcup.
One way to make ghcup work on **Void Linux musl** is to follow the [Overriding distro detection](guide.md#overriding-distro-detection)
section and tell it to consider Alpine bindists only. E.g.:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_MINIMAL=1 sh
source ~/.ghcup/env
ghcup config set platform-override '{ "arch": "A_64", "platform": { "contents": "Alpine", "tag": "Linux" }, "version": "3.17" }'
ghcup install cabal --set latest
ghcup install ghc --set latest
ghcup install stack --set latest
ghcup install hls --set latest
```

## Vim integration

See [ghcup.vim](https://github.com/hasufell/ghcup.vim).

## VSCode integration
The developers of the Haskell Language Server offer an [extension](https://github.com/haskell/vscode-haskell) tightly integrated with the [Haskell Language Server](https://github.com/haskell/haskell-language-server). To get started:

1. Install GHCup. During installation, opt in to install the Haskell Language Server (HLS).
2. Install the extension (from VSCode: Ctrl + P and then `ext install haskell.haskell`).
3. Make sure your project uses the GHC version installed from GHCup (otherwise HLS is likely to fail on launch):
    - instructions for [stack](https://docs.haskellstack.org/en/stable/configure/yaml/non-project/#system-ghc)

On Linux, some users have reported an issue when VSCode is not launched from a terminal ("cannot find ghc version"). A solution is to [let HLS know about your GHCup on $PATH](https://github.com/haskell/vscode-haskell#stackcabalghc-can-not-be-found).

## Get help

* [Libera IRC chat on #haskell-ghcup or #haskell](https://kiwiirc.com/nextclient/irc.libera.chat/?nick=Guest%7C?#haskell,#haskell-ghcup)
* [GHCup issue tracker](https://github.com/haskell/ghcup-hs/issues/new)
* [Matrix](https://matrix.to/#/#ghcup:matrix.org)
* [Discord](https://discord.gg/WDqsWsnZfR)
