## documentation

- configure script must support `--prefix`
- make must support `DESTDIR`
- write about the rewrite in the dev section
- first tool in exe list matters
- invariants (`make` must not compile the binaries that are to be installed)
- limitations (packaging `cabal-cache`)

## breaking changes

- `cabal install <ghc-ver>` is gone, use `cabal install ghc <ghc-ver>`
- `cabal install-cabal <cabal-ver>` is gone, use `cabal install cabal <cabal-ver>`
- `ghcup compile hls --isolate=/tmp/foo` installs binaries into `/tmp/foo/bin` instead of `/tmp/foo`

## refactoring and some day

- get rid of Utils/Common... and improve module structure
- rename GHCTargetVersion
- better logging

## investigate

- fix cleanup on tmpdir

## metadata TODO

* cross GHC JS has less binaries
* description of tools

## code TODO

- emit dhall types
- fetch from InstallMetadata first in list view


## ideas

- revisions

- masking/unmasking of overlay packages

- extract part of the metadata to use as an override in `config.yaml`
- maybe per-tool-per-version overrides?






```yaml

  hlint:
    3.10:
      viTags:
        - Latest
        - Recommended
      viArch:
        A_64:
          Linux_UnknownLinux:
            unknown_versioning:
              dlUri: https://github.com/ndmitchell/hlint/releases/download/v3.10/hlint-3.10-x86_64-linux.tar.gz
              dlHash: ccabc8802a58154699a3583b8dddc5ea2e6d65753a62c45c0e80088ebb16b42b
              dlSubdir: hlint-3.10
              dlInstallInfo: &hlint-install-info1
                bindistFiles:
                  exeRules:
                    - installSource: "hlint"
                      installDest: "bin/hlint"
                  # exeSymLinked:
                  #  - ["bin/hlint", "hlint-${PKGVER}"]
                  dataRules:
                    - installPattern: ["data/**"]
                  preserveMtimes: false
          Windows:
            unknown_versioning:
              dlUri: https://github.com/ndmitchell/hlint/releases/download/v3.10/hlint-3.10-x86_64-windows.zip
              dlHash: 9dc50b66771920464910472c5ae2b8eb83d33d4eadebe6d99bf512452ca22a20
              dlSubdir: hlint-3.10
              dlInstallInfo: *hlint-install-info1
          Darwin:
            unknown_versioning:
              dlUri: https://github.com/ndmitchell/hlint/releases/download/v3.10/hlint-3.10-x86_64-osx.tar.gz
              dlHash: 7a3d208fb117999336b35a32a68c476f95e22e232357fea5862250f14493cae5
              dlSubdir: hlint-3.10
              dlInstallInfo: *hlint-install-info1
```
