## breaking changes

- `cabal install <ghc-ver>` is gone, use `cabal install ghc <ghc-ver>`
- `cabal install-cabal <cabal-ver>` is gone, use `cabal install cabal <cabal-ver>`
- `ghcup compile hls --isolate=/tmp/foo` installs binaries into `/tmp/foo/bin` instead of `/tmp/foo`

## behavioral changes

- the order of tools in `ghcup list` is different (alphabetical), but the order of versions remains the same
- we don't clean up the destination anymore if merging the files fails
- improved update warnings
- explain revisions to end users
- `ghcup check`

## refactoring and some day

- better logging

## investigate

- fix cleanup on tmpdir

## metadata TODO

* cross GHC JS has less binaries

## code TODO

- test: make sure we can generate dhall and convert it back to yaml
- import qualified Dhall.Schema as Schema
- show details of tool version

- bug in set

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
