# Development

All you wanted to know about GHCup development.

## Module graph

[![Module graph](./modules_small.svg){: .center style="width:900px"}](./modules_wide.svg)

Main functionality is in `GHCup` module. Utility functions are
organised tree-ish in `GHCup.Utils` and `GHCup.Utils.*`.

Anything dealing with ghcup specific directories is in
`GHCup.Utils.Dirs`.

Download information on where to fetch bindists from is in the appropriate
yaml files: `data/metadata/ghcup-<yaml-ver>.yaml`.

## Design decisions

### Using [Excepts](https://hackage.haskell.org/package/haskus-utils-variant-3.0/docs/Haskus-Utils-Variant-Excepts.html) as a beefed up ExceptT

This is an open variant, similar to [plucky](https://hackage.haskell.org/package/plucky) or [oops](https://github.com/i-am-tom/oops) and allows us to combine different error types. Maybe it is too much and it's a little bit [unergonomic](https://github.com/haskus/packages/issues/32) at times. If it really hurts maintenance, it will be removed. It was more of an experiment.

### No use of haskell-TLS

I consider haskell-TLS an interesting experiment, but not a battle-tested and peer-reviewed crypto implementation. There is little to no research about what the intricacies of using haskell for low-level crypto are and how vulnerable such binaries are. Instead, we use either curl the binary or wget. There's also an implementation based on OpenSSL bindings, but it isn't enabled by default, since it would complicate shipping static binaries.

### Optics instead of lens

They're a little safer (less Monoid weirdness with view) and have better error messages. Consider the following with [lens](https://hackage.haskell.org/package/lens):

```
> view (_Just . to (++ "abc")) Nothing
""
```

vs [optics](https://hackage.haskell.org/package/optics):

```
> view (_Just % to (++ "abc")) Nothing

<interactive>:2:1: error:
    • An_AffineFold cannot be used as A_Getter
    • In the expression: view (_Just % to (++ "abc")) Nothing
      In an equation for ‘it’: it = view (_Just % to (++ "abc")) Nothing
```

### StrictData on by default

Kazu Yamamoto [explained it in his PR](https://github.com/yesodweb/wai/pull/752#issuecomment-501531386) very well. I like to agree with him. The instances where we need non-strict behavior, we annotate it.

`Strict` is a little more odd as a default, since it depends on how you define your functions as well.

## Code style and formatting

Unfortunately, code formatters are semi-broken on this codebase, due to TH and CPP.

Some light suggestions:

1. mtl-style preferred
2. no overly pointfree style
3. use `where` a lot, so the main function body reads like prose
4. documentation is part of the code

## Common Tasks

### Adding a new GHC version

1. open the latest `data/metadata/ghcup-<yaml-ver>.yaml`
2. find the latest ghc version (in yaml tree e.g. `ghcupDownloads -> GHC -> 8.10.7`)
3. copy-paste it
4. adjust the version, tags, changelog, source url
5. adjust the various bindist urls (make sure to also change the yaml anchors)
6. run `cabal run exe:ghcup-gen -- check          -f data/metadata/ghcup-<yaml-ver>.yaml`
7. run `cabal run exe:ghcup-gen -- check-tarballs -f data/metadata/ghcup-<yaml-ver>.yaml -u 'ghc-8\.10\.8'`

### Adding a new CLI command

An example illustration on how to deal with [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) can be seen here: [https://gitlab.haskell.org/haskell/ghcup-hs/-/commit/c19dd5ee8b2edbaf0336af143f1c75b6f4843e26](https://gitlab.haskell.org/haskell/ghcup-hs/-/commit/c19dd5ee8b2edbaf0336af143f1c75b6f4843e26)

## Major refactors

1. First major refactor included adding cross support. This added
   `GHCTargetVersion`, which includes the target in addition to the version.
   Most of the `Version` parameters to functions had to be replaced with
   that and ensured the logic is consistent for cross and non-cross
   installs.
2. This refactor added windows support wrt [#130](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/130).
   The major changes here were switching `hpath` library out for `filepath`/`directory` (sadly) and
   introducing a non-unix way of handling processes via the `process` library. It also introduced considerable
   amounts of CPP wrt file handling, installation etc.

# Releasing

1. Update version in `ghcup.cabal` and `boostrap-haskell` (`ghver` variable at the top of the script)

2. Update `GHCup.Version` module. `ghcupURL` must only be updated if we change the `GHCupInfo` type or the YAML representation of it. The version of the YAML represents the change increments. `ghcUpVer` is the current application version, read from `ghcup.cabal`.

3. Add ChangeLog entry

4. Add/fix downloads in `ghcup-<ver>.yaml` (under `data/metadata`), then verify with `ghcup-gen check -f data/metadata/ghcup-<ver>.yaml` and possibly (example only) `ghcup-gen check-tarballs -f data/metadata/ghcup-<ver>.yaml -u 'ghc-8.10.7'`. Generally, new GHC/cabal/stack/hls versions are only added to the latest yaml file. New GHCup versions are added to all (great care must be taken here to not break the parser... e.g. ARM platforms don't parse in all older formats).

5. Commit and git push with tag. Wait for tests to succeed and release artifacts to build.

6. Download release artifacts and upload them `downloads.haskell.org/~ghcup` along with checksum files (`sha256sum --tag * > SHA256SUMS && gpg --detach-sign -u <your-email> SHA256SUMS`)

7. Add ghcup release artifacts to ALL yaml files (see point 4.)

8. Upload the final `data/metadata/ghcup-<ver>.yaml` (and a detached GPG sig of it) to `webhost.haskell.org/ghcup/data/`.

9. Update `bootstrap-haskell` and `bootstrap-haskell.ps1` to `webhost.haskell.org/ghcup/sh/`

10. Update the top-level ghcup symlinks at `downloads.haskell.org/~ghcup`

11. Post on reddit/discourse/etc. and collect rewards

# Documentation

This documentation page is built via [mkdocs](https://www.mkdocs.org/), see `mkdocs.yml` and `docs/` subfolder.
The module graph needs [graphmod](https://github.com/yav/graphmod) and is generated via `scripts/dev/modgraph.sh`.
