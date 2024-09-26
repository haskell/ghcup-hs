# User Guide

This is a more in-depth guide specific to GHCup. `ghcup --help` is your friend.

## Basic usage

For the simple, interactive, text-based user interface (TUI), run:

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

### Tags and shortcuts

GHCup has a number of tags and version shortcuts, that can be used as arguments to **install**/**set** etc.
All of the following are valid arguments to `ghcup install ghc`:

* `latest`, `recommended`
* `base-4.15.1.0`
* `9.0.2`, `9.0`, `9`

If the argument is omitted, the default is `recommended`.

Other tags include:

- `prerelease`: a prerelease version
- `latest-prerelease`: the latest prerelease version


## Manpages

For man pages to work you need [man-db](http://man-db.nongnu.org/) as your `man` provider, then issue `man ghc`. Manpages only work for the currently set ghc.
`MANPATH` may be required to be unset.

## Pager

You can have `ghcup list` use a pager, similar to git. E.g. run:

```sh
ghcup --paginate list
```

To set a specific pager you can use either `GHCUP_PAGER` or `PAGER` environment variable.

To make the changes permanent, you can add the following to your config:

```yaml
pager: most
```

Refer to the [config.yaml](https://github.com/haskell/ghcup-hs/blob/master/data/config.yaml) template for more fine-grained
control.

## Shell-completion

Shell completions are in [scripts/shell-completions](https://github.com/haskell/ghcup-hs/tree/master/scripts/shell-completions) directory of this repository.

For bash: install `shell-completions/bash`
as e.g. `/etc/bash_completion.d/ghcup` (depending on distro)
and make sure your bashrc sources the startup script
(`/usr/share/bash-completion/bash_completion` on some distros).

## Portability

`ghcup` is very portable. There are a few exceptions though:

1. legacy subcommands `ghcup install` (without a tool identifier) and `ghcup install-cabal` may be removed in the future

# Configuration

A configuration file can be put in `~/.ghcup/config.yaml`. The default config file
explaining all possible configurations can be found in this repo: [config.yaml](https://github.com/haskell/ghcup-hs/blob/master/data/config.yaml).

Partial configuration is fine. Command line options always override the config file settings.

## Overriding distro detection

If you're running e.g. an Ubuntu derivative based on 18.04 and ghcup is picking bindists that
don't work well, you could do this in `config.yaml`:

```yml
platform-override:
  arch: A_64
  platform:
    contents: Ubuntu
    tag: Linux
  version: '18.04'
```

## Env variables

This is the complete list of env variables that change GHCup behavior:

* `GHCUP_USE_XDG_DIRS`: see [XDG support](#xdg-support) below
* `GHCUP_INSTALL_BASE_PREFIX`: the base of ghcup (default: `$HOME`)
* `GHCUP_CURL_OPTS`: additional options that can be passed to curl
* `GHCUP_WGET_OPTS`: additional options that can be passed to wget
* `GHCUP_GPG_OPTS`: additional options that can be passed to gpg
* `GHCUP_SKIP_UPDATE_CHECK`: Skip the (possibly annoying) update check when you run a command
* `CC`/`LD` etc.: full environment is passed to the build system when compiling GHC via GHCup

On windows, there's additionally:

* `GHCUP_MSYS2`: Has to point to the root of an existing MSYS2 installation (when installed by GHCup, that's e.g. `C:\ghcup\msys64`). GHCup bootstrap takes care of this usually.
* `GHCUP_MSYS2_ENV`: The [MSYS2 environment](https://www.msys2.org/docs/environments/) to use when executing e.g. `ghcup run --mingw-path`. Possible values are `MSYS`, `UCRT64`, `CLANG64`, `CLANGARM64`, `CLANG32`, `MINGW64`, `MINGW32`. Defaults to `MINGW64`, `MINGW32` or `CLANGARM64`, depending on the architecture. `MSYS` is always added as the last component. If you change this value after running the bootstrap script, you may need to make sure that the cabal config reflects this change, more specifically `extra-prog-path`, `extra-include-dirs` and `extra-lib-dirs`. (**NOTE: specifying anything other than the default is considered experimental**)

### XDG support

To enable XDG style directories, set the environment variable `GHCUP_USE_XDG_DIRS` to anything.

Then you can control the locations via XDG environment variables as such:

* `XDG_DATA_HOME`: GHCs will be unpacked in `ghcup/ghc` subdir (default: `~/.local/share`)
* `XDG_CACHE_HOME`: logs and download files will be stored in `ghcup` subdir (default: `~/.cache`)
* `XDG_BIN_HOME`: binaries end up here (default: `~/.local/bin`)
* `XDG_CONFIG_HOME`: the config file is stored in `ghcup` subdir as `config.yaml` (default: `~/.config`)

**Note that `ghcup` makes some assumptions about structure of files in `XDG_BIN_HOME`. So if you have other tools
installing e.g. stack/cabal/ghc into it, this will likely clash. In that case consider disabling XDG support.**

## Caching

GHCup has a few caching mechanisms to avoid redownloads. All cached files end up in `~/.ghcup/cache` by default.

### Downloads cache

Downloaded tarballs (such as GHC, cabal, etc.) are not cached by default unless you pass `ghcup --cache` or set caching
in your [config](#configuration) via `ghcup config set cache true`.

### Metadata cache

The metadata files (also see [github.com/haskell/ghcup-metadata](https://github.com/haskell/ghcup-metadata))
have a 5 minutes cache per default depending on the last access time of the file. That means if you run
`ghcup list` 10 times in a row, only the first time will trigger a download attempt.

### Clearing the cache

If you experience problems, consider clearing the cache via `ghcup gc --cache`.

## Metadata

Metadata files are also called release or distribution channels. They describe tool versions, where to download them etc. and
can be viewed here: [https://github.com/haskell/ghcup-metadata](https://github.com/haskell/ghcup-metadata).

See the [description](https://github.com/haskell/ghcup-metadata#metadata-variants-distribution-channels)
of metadata files to understand their purpose. These can be combined.

For example, if you want access to both prerelease and cross bindists, you'd do:

```sh
ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-prereleases-0.0.8.yaml
ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-cross-0.0.8.yaml
```

This results in the following configuration in `~/.ghcup/config.yaml`:

```yaml
url-source:
# the base url that contains all the release bindists
- GHCupURL
# prereleases
- https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-prereleases-0.0.8.yaml
# cross bindists
- https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-cross-0.0.8.yaml
```

You can add as many channels as you like. They are combined under *Last*, so versions from the prerelease channel
here overwrite the default ones, if any.

To remove the channel, delete the entire `url-source` section or set it back to the default:

```yml
url-source:
  - GHCupURL
```

Also see [config.yaml](https://github.com/haskell/ghcup-hs/blob/master/data/config.yaml)
for more options.

You can also use an alternative metadata via the one-shot CLI option:

```sh
ghcup --url-source=https://some-url/ghcup-0.0.8.yaml tui
```

One main caveat of using URLs is that you might need to check whether there are new versions
of the file (e.g. `ghcup-0.0.7.yaml` vs `ghcup-0.0.8.yaml`). Although old metadata files
are supported for some time, they are not so indefinitely.

### Mirrors

Metadata files can also be used to operate 3rd party mirrors, in which case you want to use
a URL instead of the `GHCupURL` alias. E.g. in `~/.ghcup/config.yaml`, you'd do:

```yml
url-source:
  - https://mirror.sjtu.edu.cn/ghcup/yaml/ghcup/data/ghcup-0.0.6.yaml
```

Note that later versions of GHCup allow more sophisticated mirror support, see [here](#mirrors-proper).

#### Known mirrors

1. [https://mirror.sjtu.edu.cn/docs/ghcup](https://mirror.sjtu.edu.cn/docs/ghcup)
2. [https://mirrors.ustc.edu.cn/help/ghcup.html](https://mirrors.ustc.edu.cn/help/ghcup.html)

### Git-based metadata config

If you don't like the way ghcup updates its metadata with caching and fetching via curl, you can also do as follows:

Clone the metadata git repo:

```sh
mkdir -p /home/user/git/
cd /home/user/git/
git clone -b master https://github.com/haskell/ghcup-metadata.git
```

Then tell ghcup to use file locations in `~/.ghcup/config.yaml`, e.g.:

```yaml
url-source:
- file:///home/user/git/ghcup-metadata/ghcup-0.0.8.yaml
- file:///home/user/git/ghcup-metadata/ghcup-cross-0.0.8.yaml
- file:///home/user/git/ghcup-metadata/ghcup-prereleases-0.0.8.yaml
```

Now, if you invoke `ghcup tui`, it will open instantly without any download, since it just
reads the metadata from local disk.

You'll have to update the metadata manually though, like so:

```sh
cd /home/user/git/
git pull --ff-only origin master
```

## Stack integration

Stack manages GHC versions internally by default. In order to make it use ghcup installed
GHC versions there are two strategies.

### Strategy 1: Stack hooks (new, recommended)

Since stack 2.9.1 you can customize the installation logic of GHC completely, see
[https://docs.haskellstack.org/en/stable/configure/customisation_scripts/#ghc-installation-customisation](https://docs.haskellstack.org/en/stable/configure/customisation_scripts/#ghc-installation-customisation).

We can use this to simply invoke ghcup whenever stack is trying to install/discover a GHC versions. This
is done via placing a shell script at `~/.stack/hooks/ghc-install.sh` and making it executable.

The ghcup bootstrap script asks you during installation whether you want to install this shell script. You can also
install/update it manually like so:

```sh
mkdir -p ~/.stack/hooks/
curl https://raw.githubusercontent.com/haskell/ghcup-hs/master/scripts/hooks/stack/ghc-install.sh \
  > ~/.stack/hooks/ghc-install.sh
chmod +x ~/.stack/hooks/ghc-install.sh
# hooks are only run when 'system-ghc: false'
stack config set system-ghc false --global
```

By default, when the hook fails for whatever reason, stack will fall back to its own installation logic. To disable
this, run `stack config set install-ghc false --global`.

### Strategy 2: System GHC (works on all stack versions)

You can instruct stack to use "system" GHC versions (whatever is in PATH). To do so,
run the following commands:

```sh
stack config set install-ghc false --global
stack config set system-ghc  true  --global
```

### Using stack's setup-info metadata to install GHC

You can now use stack's [setup-info metadata](https://github.com/commercialhaskell/stackage-content/blob/master/stack/stack-setup-2.yaml)
to install GHC. For that, you can invoke ghcup like so as a shorthand:

```sh
# ghcup will only see GHC now
ghcup -s StackSetupURL install ghc 9.4.7
# this combines both ghcup and stack metadata
ghcup -s '["GHCupURL", "StackSetupURL"]' install ghc 9.4.7
```

To make this permanent and combine it with the GHCup metadata, you can add the following to your `~/.ghcup/config.yaml`:

```yaml
url-source:
  - GHCupURL
  # stack versions take precedence
  # you'll still have access to GHCup provided versions and tools in case they don't exist in stack metadata
  - StackSetupURL
```

You can customize or add sections to the setup-info similar to how the
[stack documentation](https://docs.haskellstack.org/en/stable/configure/yaml/non-project/#setup-info)
explains it. E.g. to change the 9.4.7 bindist, you might do:

```yaml
url-source:
  - GHCupURL
  - StackSetupURL
  - setup-info:
      ghc:
        linux64-tinfo6:
          9.4.7:
            url: "https://downloads.haskell.org/~ghc/9.4.7/ghc-9.4.7-x86_64-fedora27-linux.tar.xz"
            content-length: 179117892
            sha256: 216b76b7c6383e6ad9ba82533f323f8550e52893a8b9fa33c7b9dc4201ac766a
```

#### Caveats

The main caveat with using this method is that there's no guarantee that GHCup will pick a compatible HLS bindist
when you try to install HLS.

Another potential usability issue is that the `latest` and `recommended` shorthands won't work anymore, since
Stack metadata doesn't have a concept of those and we don't try to be smart when combining the metadatas.

### Windows

#### Using GHCup's MSYS2 installation

Stack usually maintains its own msys2 installation. However, you can instruct it to use GHCup's MSYS2 or any other. E.g. if you
had GHCup install msys2 into `C:\ghcup\msys64\`, then you would add the following config to stack's `config.yaml`
(you can find its location via `stack path --stack-root`):

```yaml
skip-msys: true
extra-lib-dirs:
- C:\ghcup\msys64\mingw64\lib
- C:\ghcup\msys64\mingw64\bin
extra-path:
- C:\ghcup\msys64\mingw64\bin
- C:\ghcup\msys64\usr\bin
- C:\ghcup\msys64\usr\local\bin
extra-include-dirs:
- C:\ghcup\msys64\mingw64\include
```

Also check out:
[https://docs.haskellstack.org/en/stable/configure/yaml/non-project](https://docs.haskellstack.org/en/stable/configure/yaml/non-project)

## Mirrors (proper)

Mirrors are now supported via configuration, instead of specifying alternative metadata files.

As an example, this would be a complete mirror configuration in `~/.ghcup/config.yaml`:

```yaml
mirrors:
  # yaml download location, would result in:
  #      https://raw.githubusercontent.com/haskell/ghcup-metadata/develop/ghcup-0.0.8.yaml
  #   -> https://mirror.sjtu.edu.cn/ghcup/yaml/haskell/ghcup-metadata/master/ghcup-0.0.8.yaml
  "raw.githubusercontent.com":
    authority:
      host: "mirror.sjtu.edu.cn"
    pathPrefix: "ghcup/yaml"
  # for stack and some older HLS versions, would result in e.g.
  #      https://github.com/haskell/haskell-language-server/releases/download/1.2.0/haskell-language-server-Windows-1.2.0.tar.gz
  #   -> https://mirror.sjtu.edu.cn/ghcup/github/haskell/haskell-language-server/releases/download/1.2.0/haskell-language-server-Windows-1.2.0.tar.gz
  "github.com":
    authority:
      host: "mirror.sjtu.edu.cn"
    pathPrefix: "ghcup/github"
  # for all haskell.org hosted bindists, would result in e.g.
  #      https://downloads.haskell.org/~ghc/9.8.1/ghc-9.8.1-x86_64-deb10-linux.tar.xz
  #   -> https://mirror.sjtu.edu.cn/ghcup/haskell-downloads/~ghc/9.8.1/ghc-9.8.1-x86_64-deb10-linux.tar.xz
  "downloads.haskell.org":
    authority:
      host: "mirror.sjtu.edu.cn"
    pathPrefix: "downloads.haskell.org"
```

The configuration depends on the host of the mirror and they have to provide the correct configuration.

## Linkers

The GHC bindist configure script by default doesn't honour the system `ld` that is set, but instead
probes for `ld.lld`, `ld.gold` and only then `ld` in order, see
[find_ld.m4](https://gitlab.haskell.org/ghc/ghc/-/blob/master/m4/find_ld.m4?ref_type=heads).

This is controlled by the configure switch `--enable-ld-override`/`--disable-ld-override`, which is enabled by default in GHC.
GHCup however [has decided](https://github.com/haskell/ghcup-hs/issues/1032) **to disable this switch by default**,
for reasons of stability and simplicity.

That means, when `--disable-ld-override` is passed, the linker is picked simply by:

* checking if `LD` env var is set, then use whatever is specified
* otherwise use `ld` binary in PATH (system/distro default)

You can restore the GHC vanilla default by adding this to your `~/.ghcup/config.yaml`:

```yaml
def-ghc-conf-options:
  - "--enable-ld-override"
```

# More on installation

## Customisation of the installation scripts

The scripts offered to install GHCup are available here:

* [bootstrap-haskell](https://github.com/haskell/ghcup-hs/blob/master/scripts/bootstrap/bootstrap-haskell#L7)
  for Unix-like operating systems
* [bootstrap-haskell.ps1](https://github.com/haskell/ghcup-hs/blob/master/scripts/bootstrap/bootstrap-haskell.ps1)
  for Windows (PowerShell). This will, in turn, run the final bootstrap script
  (by default, that for the Unix-like operating systems).

The effect of the scripts can be customised by setting one or more
`BOOTSTRAP_HASKELL_*` environment variables (as set out in the first script)
and, in the case of Windows, by specifying parameters (as set out in the
PowerShell script).

For example, you can toggle:

* non-interactive installation
* a more verbose installation
* whether to install only GHCup (and, on Windows, MSYS2)
* not to trigger the upgrade of GHCup
* whether to install the latest version of HLS
* whether to install the latest version of Stack
* whether to respect the XDG Base Directory Specification
* whether to adjust (prepend) the PATH in `bashrc`
* on Windows, whether to adjust MINGW paths in `cabal.config`

You can also specify:

* the GHC version to install
* the Cabal version to install
* which downloader to use (the default is `curl`)
* the base URL for the download of the GHCup binary distribution

On Windows, you can also use the parameters to:

* toggle whether to overwrite a previous installation
* specify the GHCup installation root directory
* specify the Cabal root directory
* specify the directory of an existing installation of MSYS2 (for example,
  the one supplied by Stack)
* specify the URL of the final bootstrap script
* toggle whether to run the final bootstrap script via `bash` (instead of in a
  new MSYS2 shell)

## Installing custom bindists

There are a couple of good use cases to install custom bindists:

1. manually built bindists (e.g. with patches)
    - example: `ghcup install ghc -u 'file:///home/mearwald/tmp/ghc-eff-patches/ghc-8.10.2-x86_64-deb10-linux.tar.xz' 8.10.2-eff`
2. GHC head CI bindists
    - example specifying a branch (`master`): `ghcup install ghc -u 'https://gitlab.haskell.org/ghc/ghc/-/jobs/artifacts/master/raw/ghc-x86_64-linux-fedora33-release.tar.xz?job=x86_64-linux-fedora33-release' head`
    - example specifying a job id (`1129565`): `ghcup install ghc -u ' https://gitlab.haskell.org/api/v4/projects/1/jobs/1129565/artifacts/ghc-x86_64-linux-alpine3_12-validate+fully_static.tar.xz' mr7847`
3. DWARF bindists
    - example: `ghcup install ghc -u 'https://downloads.haskell.org/~ghc/8.10.2/ghc-8.10.2-x86_64-deb10-linux-dwarf.tar.xz' 8.10.2-dwarf`

Since the version parser is pretty lax, `8.10.2-eff` and `head` are both valid versions
and produce the binaries `ghc-8.10.2-eff` and `ghc-head` respectively.
GHCup always needs to know which version the bindist corresponds to (this is not automatically
detected).

## Compiling from source

### GHC

Compiling from source is supported for both source tarballs and arbitrary git refs. See `ghcup compile ghc --help`
for a list of all available options.

If you need to overwrite the existing `build.mk`, check the default files
in [data/build_mk](https://github.com/haskell/ghcup-hs/tree/master/data/build_mk), copy them somewhere, adjust them and
pass `--config path/to/build.mk` to `ghcup compile ghc`.
Common `build.mk` options are explained [here](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/using#build-configuration).

Make sure your system meets all the [prerequisites](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/preparation).

### HLS

There are 3 main ways to compile HLS from source.

1. from hackage (should have up to date version bounds)
    - `ghcup compile hls --version 1.7.0.0 --ghc 9.2.3`
2. from git (allows to build latest sources and PRs)
    - `ghcup compile hls --git-ref master --ghc 9.2.3`
    - `ghcup compile hls --git-ref a32db0b --ghc 9.2.3`
    - `ghcup compile hls --git-ref 1.7.0.0 --ghc 9.2.3`
3. from source distribution that's packaged during release from the corresponding git sources
    - `ghcup compile hls --source-dist 1.7.0.0 --ghc 9.2.3`

All these use `cabal v2-install` under the hood, so all build components are cached.
You can pass arbitrary arguments to cabal, e.g. set the index state like so:

```sh
ghcup compile hls --git-ref master --ghc 9.2.3 -- --index-state=2022-06-12T00:00:00Z --allow-newer
```

You can pass `--ghc <ver>` multiple times to install for many GHCs at once.

When building from git sources, ghcup will auto-detect the HLS version that the git commit corresponds to
from the `haskell-language-server.cabal` file. This version might not have been updated since the last release.
If you want to avoid overwriting the existing installed HLS version, you can instruct ghcup to use `git describe`
to set the HLS version instead:

```sh
ghcup compile hls --git-ref master --ghc 9.2.3 --git-describe-version
```

You can also set the version explicitly:

```sh
ghcup compile hls --git-ref master --ghc 9.2.3 --overwrite-version 1.7.0.0-p1
```

To instruct cabal to run `cabal update` before building, run `ghcup compile hls --version 1.7.0.0 --ghc 9.2.3 --cabal-update`

As always, check `ghcup compile hls --help`.

#### Updating HLS for a new GHC version

First try to build from hackage with some tricks:

```sh
ghcup compile hls --version 1.7.0.0 --ghc 9.2.4 --cabal-update -- --allow-newer --index-state=2022-06-12T00:00:00Z
```

This augments the currently installed 1.7.0.0 official bindists in ghcup with new GHC versions support.

If that fails (since `--allow-newer` is quite brutal), you can install from HLS master branch (which may contain new fixes) like so:
```
ghcup compile hls --git-ref master --git-describe-version --ghc 8.10.7 --ghc 9.2.4 --cabal-update
```

This however will create a new HLS version in ghcup, e.g. `1.7.0.0-105-gdc682ba1`, for both 8.10.7 and 9.2.4. If you want to switch back to the official bindists, run `ghcup set hls 1.7.0.0`.

## Cross support

ghcup can compile a cross GHC for any target. However, this
requires that the build host has a complete cross toolchain and various
libraries installed for the target platform.

Consult the GHC documentation on the [prerequisites](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling#tools-to-install).
For distributions with non-standard locations of cross toolchain and
libraries, this may need some tweaking of `build.mk` or configure args.
See `ghcup compile ghc --help` for further information.

Since ghcup version 0.1.20.0, we provide cross bindists for GHC JS and WASM. These can be installed conveniently.
However, these are intended as a developer preview only. By using these GHC variants, you are implicitly signing up to participate in GHC development!
If you run into bugs or missing behavior, join the dev chat at https://matrix.to/#/#GHC:matrix.org.

First, add the cross release channel:

```sh
ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/develop/ghcup-cross-0.0.8.yaml
```

The next sections explain how to install each cross bindist.

### GHC JS cross bindists (experimental)

You need the required emscripten JS toolchain. GHC JS cross bindists might require you to install a specific
version of emscripten. If that is the case, then ghcup will display the required emscripten version in the
pre install message. You can use the following commands to install the emscripten toolchain on your system,
substituting the required version for the bindist that you want to install.
(Cf. [GHC-MR 10918](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10918))

```sh
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
./emsdk install VERSION
./emsdk activate VERSION
source ./emsdk_env.sh
```

Instructions are also here: [Download and install — Emscripten documentation](https://emscripten.org/docs/getting_started/downloads.html).

To install you can either use the tui interface by invoking `emconfigure ghcup tui` or
you can install directly like so:

```sh
emconfigure ghcup install ghc --set javascript-unknown-ghcjs-9.6.2
```

You'll now have the compiler `javascript-unknown-ghcjs-ghc`. To build a hello world, do e.g.:

```sh
echo 'main = putStrLn "hello world"' > hello.hs
javascript-unknown-ghcjs-ghc -fforce-recomp hello.hs
./hello
```

You can follow the instructions [here](https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend/building#compiling-hello-world).

### GHC WASM cross bindists (experimental)

You need the required wasm toolchain:

```sh
git clone https://gitlab.haskell.org/ghc/ghc-wasm-meta.git
cd ghc-wasm-meta/
export SKIP_GHC=yes
./setup.sh
source ~/.ghc-wasm/env
```

**Note that some wasm bindists don't work with the master branch of ghc-wasm-meta. GHCup will warn you about such cases prior to installation and point you to the right commit.**

To install, we need to invoke ghcup like so also passing the `--host=<host>` flag (adjust as needed):

```sh
ghcup install ghc --set wasm32-wasi-9.6.3.20230927 -- --host=x86_64-linux --with-intree-gmp --with-system-libffi
```

Also check the documentation here: [Glasgow Haskell Compiler / ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta).

You'll now have the compiler `wasm32-wasi-ghc`. To build a hello world, do e.g.:

```sh
echo 'main = putStrLn "hello world"' > hello.hs
wasm32-wasi-ghc hello.hs -o hello.wasm
wasmtime ./hello.wasm
```

## Isolated installs

**Before using isolated installs, make sure to have at least GHCup version 0.1.17.8!**

Ghcup also enables you to install a tool (GHC, Cabal, HLS, Stack) at an isolated location of your choosing.
These installs, as the name suggests, are separate from your main installs and DO NOT conflict with them.


- No symlinks are made to these isolated installed tools, you'd have to manually point to them wherever you intend to use them.

- These installs, can also NOT be deleted from ghcup, you'd have to go and manually delete these.

You need to use the `--isolate` or `-i` flag followed by the directory path.

Examples:

1. install an isolated GHC version at location /home/user/isolated_dir/ghc/
    - `ghcup install ghc 8.10.5 --isolate /home/user/isolated_dir/ghc`

2. isolated install Cabal at a location you desire
    - `ghcup install cabal --isolate /home/username/my_isolated_dir/`

3. do an isolated install with a custom bindist
    - `ghcup install ghc --isolate /home/username/my_isolated_dir/ -u 'https://gitlab.haskell.org/ghc/ghc/-/jobs/artifacts/master/raw/ghc-x86_64-linux-fedora33-release.tar.xz?job=x86_64-linux-fedora33-release' head`

4. isolated install HLS
    - `ghcup install hls --isolate /home/username/dir/hls/`

5. you can even compile ghc to an isolated location.
    - `ghcup compile ghc -j 4 -v 9.0.1 -b 8.10.5 -i /home/username/my/dir/ghc`

## Continuous integration

On Windows, GHCup can be installed automatically on a CI runner
non-interactively, as below. The parameters to the PowerShell script are
specified positionally, after `-ArgumentList`:

```ps
$ErrorActionPreference = 'Stop';Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Minimal -InBash -InstallDir "C:\" } catch { Write-Error $_ }
```

`$ErrorActionPreference = 'Stop'` here acts like `set -e` and stops execution if ghcup installation fails.

On linux/darwin/freebsd, run the following on your runner:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
```

This will just install `ghcup` and on Windows additionally MSYS2.

See the installation scripts referred to above for the full list of environment
variables and, in the case of Windows, parameters to tweak the script behavior.

### github workflows

On github workflows GHCup itself is pre-installed on all platforms, but may use non-standard install locations.
Here's an example workflow with a GHC matrix:

```yaml
jobs:
  build:
    runs-on: ${{ matrix.os }}
    strategy:
      fail-fast: true
      matrix:
        os: [ubuntu-22.04, macOS-latest]
        ghc: ['9.6', '9.4', '9.2', '9.0', '8.10', '8.8', '8.6']
    steps:
    - uses: actions/checkout@v3
    - name: Setup toolchain
      run: |
        ghcup install cabal --set recommended
        ghcup install ghc --set ${{ matrix.ghc }}
    - name: Build
      run: |
        cabal update
        cabal test all --test-show-details=direct

  i386:
    runs-on: ubuntu-latest
    container:
      image: i386/ubuntu:bionic
    steps:
    - name: Install GHCup in container
      run: |
        apt-get update -y
        apt-get install -y autoconf build-essential zlib1g-dev libgmp-dev curl
        # we just go with recommended versions of cabal and GHC
        curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 sh
    - uses: actions/checkout@v1
    - name: Test
      run: |
        # in containers we need to fix PATH
        source ~/.ghcup/env
        cabal update
        cabal test all --test-show-details=direct
```

## GPG verification

GHCup supports verifying the GPG signature of the metadata file. The metadata file then contains SHA256 hashes of all downloads, so
this is cryptographically secure.

First, obtain the gpg keys:

```sh
gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C
gpg --batch --keyserver keyserver.ubuntu.com --recv-keys FE5AB6C91FEA597C3B31180B73EDE9E8CFBAEF01
gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4
gpg --batch --keyserver keyserver.ubuntu.com --recv-keys EAF2A9A722C0C96F2B431CA511AAD8CEDEE0CAEF
```

Then verify the gpg key in one of these ways:

1. find out where I live and visit me to do offline key signing
2. figure out my mobile phone number and call me to verify the fingerprint
3. more boring: contact me on Libera IRC (`maerwald`) and verify the fingerprint

Once you've verified the key, you have to figure out if you trust me.

If you trust me, then you can configure gpg in `~/.ghcup/config.yaml`:

```yml
gpg-setting: GPGLax # GPGStrict | GPGLax | GPGNone
```

In `GPGStrict` mode, ghcup will fail if verification fails. In `GPGLax` mode it will just print a warning.
You can also pass the mode via `ghcup --gpg <strict|lax|none>`.

# Tips and tricks

## ghcup run

If you don't want to explicitly switch the active GHC all the time and are using
tools that rely on the plain `ghc` binary, GHCup provides an easy way to execute
commands with a certain toolchain prepended to PATH, e.g.:

```sh
ghcup run --ghc 8.10.7 --cabal latest --hls latest --stack latest --install -- code Setup.hs
```

This will execute vscode with GHC set to 8.10.7 and all other tools to their latest version.

# Troubleshooting

## Script immediately exits on windows

There are two possible reasons:

1. your company blocks the script (some have a whitelist)... ask your administrator
2. your Antivirus or Windows Defender interfere with the installation. Disable them temporarily.

## C compiler cannot create executables

### Darwin

You need to update your XCode command line tools, e.g. [like this](https://stackoverflow.com/questions/34617452/how-to-update-xcode-from-command-line).

## Certificate authority errors (curl or wget)

GHCup uses the tools `curl` or `wget` to download files. If certificate
authority (CA) certificates are outdated or improperly configured, then these
tools may be unable to download and errors will be reported.

### Unix-like operating systems

There are two known workarounds to being unable to download with `curl`:

1.  Tell `curl` to ignore CA certificate errors (dangerous):
    ~~~sh
    `curl -k https://raw.githubusercontent.com/haskell/ghcup-hs/master/scripts/bootstrap/bootstrap-haskell | GHCUP_CURL_OPTS="-k" sh`
    ~~~

2.  Try to use `wget` instead:
    ~~~sh
    `wget -O /dev/stdout https://raw.githubusercontent.com/haskell/ghcup-hs/master/scripts/bootstrap/bootstrap-haskell | BOOTSTRAP_HASKELL_DOWNLOADER=wget sh`
    ~~~

### Windows

On Windows, GHCup uses the MSYS2-supplied `curl` or `wget` in the MSYS2
environment that has specified for use with GHCup. In some circumstances (see
further below), the MSYS2 environment may need certain CA certificates to be
manually added and trusted.

On Windows, you can disable the use of `curl` like so:
~~~pwsh
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
~~~

In some circumstances, an organisation may install their own CA certificates
onto a Windows system. However, OpenSSL in a MSYS2 environment does not
integrate with the Windows system's store of certificates. In those
circumstances, the certificates need to be obtained and then the MSYS2
environment configured to use them before `curl` or `wget` will work. These
circumstances may give rise to errors that include messages such as:
~~~text
curl: (60) SSL certificate problem: unable to get local issuer certificate
More details here: https://curl.se/docs/sslcerts.html
~~~

or
~~~text
curl: (35) schannel: next InitializeSecurityContext failed: CRYPT_E_NO_REVOCATION_CHECK (0x80092012) - The revocation function was unable to check revocation for the certificate.
~~~

or
~~~text
ERROR: The certificate of ‘downloads.haskell.org’ is not trusted.
ERROR: The certificate of ‘downloads.haskell.org’ doesn't have a known issuer.
~~~

The relevant CA certificates can be identified and obtained by opening
`https://downloads.haskell.org` in a browser and getting information about the
heirarchy of certificates. In the case of Microsoft Edge, that information is
provided via (in order) the 'padlock' icon, 'Connection is secure', the
'certificate' icon, and the 'Details' tab of the 'Certificate Viewer' dialog.
Each certificate in the heirarchy above the domain needs to be chosen and
exported as a file in the relevant format (`*.pem`, `*.crt` or `*.cer` files).

All those certificate files are then copied to the directory
`/etc/pki/ca-trust/source/anchors` in the MSYS2 environment.

In a MSYS2 shell provided by that environment, the command `update-ca-trust`
will (silently) add the CA certificates in that location to those that are
trusted. This command is available as part of the basic MSYS2 installation.

See also the related MSYS2
[FAQ](https://www.msys2.org/docs/faq/#how-can-i-make-msys2pacman-trust-my-companys-custom-tls-ca-certificate).
