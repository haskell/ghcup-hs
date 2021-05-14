# HACKING

## Design decisions

### Using [Excepts](https://hackage.haskell.org/package/haskus-utils-variant-3.0/docs/Haskus-Utils-Variant-Excepts.html) as a beefed up ExceptT

This is an open variant, similar to [plucky](https://hackage.haskell.org/package/plucky) or [oops](https://github.com/i-am-tom/oops) and allows us to combine different error types. Maybe it is too much and it's a little bit [unergonomic](https://github.com/haskus/packages/issues/32) at times. If it really hurts maintenance, it will be removed. It was more of an experiment.

### No use of haskell-TLS

I consider haskell-TLS an interesting experiment, but not a battle-tested and peer-reviewed crypto implementation. There is little to no research about what the intricacies of using haskell for low-level crypto are and how vulnerable such binaries are. Instead, we use either curl the binary (for FreeBSD and mac) or http-io-streams, which works with OpenSSL bindings.

### Optics instead of lens

They're a little safer (less Monoid weirdness with view) and have better error messages. Consider the following wit lens

```
> view (_Just . to (++ "abc")) Nothing
""
```

vs optics

```
> view (_Just % to (++ "abc")) Nothing

<interactive>:2:1: error:
    • An_AffineFold cannot be used as A_Getter
    • In the expression: view (_Just % to (++ "abc")) Nothing
      In an equation for ‘it’: it = view (_Just % to (++ "abc")) Nothing
```

### Strict and StrictData on by default

Kazu Yamamoto [explained it in his PR](https://github.com/yesodweb/wai/pull/752#issuecomment-501531386) very well. I like to agree with him. The instances where we need non-strict behavior, we annotate it.

## Code style and formatting

1. Brittany
2. mtl-style preferred
3. no overly pointfree style

## Code structure

Main functionality is in `GHCup` module. Utility functions are
organised tree-ish in `GHCup.Utils` and `GHCup.Utils.*`.

Anything dealing with ghcup specific directories is in
`GHCup.Utils.Dirs`.

Download information on where to fetch bindists from is in the appropriate
yaml files: `ghcup-<yaml-ver>.yaml`.

## Common Tasks

### Adding a new GHC version

1. open the latest `ghcup-<yaml-ver>.yaml`
2. find the latest ghc version (in yaml tree e.g. `ghcupDownloads -> GHC -> 8.10.3`)
3. copy-paste it
4. adjust the version, tags, changelog, source url
5. adjust the various bindist urls (make sure to also change the yaml anchors)
6. run `cabal run exe:ghcup-gen -- check-tarballs -f ghcup-<yaml-ver>.yaml -u 'ghc-8\.10\.4'`

## Major refactors

1. First major refactor included adding cross support. This added
   `GHCTargetVersion`, which includes the target in addition to the version.
   Most of the `Version` parameters to functions had to be replaced with
   that and ensured the logic is consistent for cross and non-cross
   installs.
