# GHCup metadata

## Adding a new GHC version

1. open the latest `ghcup-<yaml-ver>.yaml`
2. find the latest ghc version (in yaml tree e.g. `ghcupDownloads -> GHC -> 8.10.7`)
3. copy-paste it
4. adjust the version, tags, changelog, source url
5. adjust the various bindist urls (make sure to also change the yaml anchors)
6. run `cabal run ghcup-gen -- check          -f ghcup-<yaml-ver>.yaml`
7. run `cabal run ghcup-gen -- check-tarballs -f ghcup-<yaml-ver>.yaml -u 'ghc-8\.10\.8'`

