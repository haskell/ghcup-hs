# HACKING

## Design decisions

### Using [Excepts](https://hackage.haskell.org/package/haskus-utils-variant-3.0/docs/Haskus-Utils-Variant-Excepts.html) as a beefed up ExceptT

This is an open variant, similar to [plucky](https://hackage.haskell.org/package/plucky) or [oops](https://github.com/i-am-tom/oops) and allows us to combine different error types. Maybe it is too much and it's a little bit [unergonomic](https://github.com/haskus/packages/issues/32) at times. If it really hurts maintenance, it will be removed. It was more of an experiment.

### No use of filepath or directory

Filepath and directory have two fundamental problems: 1. they use String as filepath (see [AFPP](https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path) as to why this is wrong) and 2. they try very hard to be cross-platform at the expense of low-level correctness. Instead, we use the [hpath](https://github.com/hasufell/hpath) libraries for file and filepath related stuff, which also gives us stronger filepath types.

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

Download information on where to fetch bindists from is in
`GHCup.Data.GHCupDownloads`.

