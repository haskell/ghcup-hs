# GHCup metadata

## For end users

### Metadata variants (distribution channels)

* `ghcup-A.B.C.yaml`: this is the main metadata and what ghcup uses by default
* `ghcup-vanilla-A.B.C.yaml`: this is similar to `ghcup-A.B.C.yaml`, but only uses upstream bindists (no patches/fixes are applied, no missing platforms added)
* `ghcup-prereleases-A.B.C.yaml`: this contains pre-releases of all tools
* `ghcup-cross-A.B.C.yaml`: this contains experimental cross compilers. See https://www.haskell.org/ghcup/guide/#cross-support for details.

### Using the metadata

If you want access to both pre-releases and cross compilers, run:

```
ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-prereleases-0.0.7.yaml
ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-cross-0.0.8.yaml
```

## For contributors

### Adding a new GHC version

1. open the latest `ghcup-<yaml-ver>.yaml`
2. find the latest ghc version (in yaml tree e.g. `ghcupDownloads -> GHC -> 8.10.7`)
3. copy-paste it
4. adjust the version, tags, changelog, source url
5. adjust the various bindist urls (make sure to also change the yaml anchors)
6. run `cabal run ghcup-gen -- check             -f ghcup-<yaml-ver>.yaml`
7. run `cabal run ghcup-gen -- check-tarballs    -f ghcup-<yaml-ver>.yaml -u 'ghc-8\.10\.8'`
8. run `cabal run ghcup-gen -- generate-hls-ghcs -f ghcup-<yaml-ver>.yaml --format json -o hls-metadata-0.0.1.json`
9. run `cabal run ghcup-gen -- generate-table    -f ghcup-<yaml-ver>.yaml --stdout` and adjust [docs/install](https://gitlab.haskell.org/haskell/ghcup-hs/-/blob/master/docs/install.md) tables

### During a pull request

* make sure to always add new versions to both `ghcup-A.B.C.yaml` and `ghcup-vanilla-A.B.C.yaml`
* make sure to run the bindist action to check tool installation on all platforms: https://github.com/haskell/ghcup-metadata/actions/workflows/bindists.yaml
  - this is a manual pipeline
  - set the appropriate parameters
* make sure to sign the yaml files you edited, e.g.: `gpg --detach-sign -u <your-email> ghcup-0.0.7.yaml` or ask a GHCup developer to sign
  - PGP pubkeys need to be cross-signed by the GHCup team
  - they need to be added to the CI: https://github.com/haskell/ghcup-metadata/blob/develop/.github/workflows/sigs
  - and need to be documented on the homepage
    * https://github.com/haskell/ghcup-hs/blob/master/docs/guide.md#gpg-verification
	* https://github.com/haskell/ghcup-hs/blob/master/docs/install.md#unix

### Understanding tags

Tags are documented [here](https://github.com/haskell/ghcup-hs/blob/master/lib/GHCup/Types.hs). Search for `data Tag`.
Some tags are unique. Uniqueness is checked by `cabal run ghcup-gen -- check -f ghcup-<yaml-ver>.yaml`.

If you want to check prereleases, do: `cabal run ghcup-gen -- check -f ghcup-prereleases-<yaml-ver>.yaml --channel=prerelease`

