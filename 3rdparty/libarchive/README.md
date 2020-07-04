# libarchive

[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/libarchive/badge)](https://matrix.hackage.haskell.org/package/libarchive)
[![Hackage](https://img.shields.io/hackage/v/libarchive.svg)](http://hackage.haskell.org/package/libarchive)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/libarchive.svg)](https://hackage.haskell.org/package/libarchive)

This contains Haskell bindings to
[libarchive](http://libarchive.org/). It was created as an alternative to
[tar](http://hackage.haskell.org/package/tar) and
[tar-conduit](http://hackage.haskell.org/package/tar-conduit), but it supports
more archive formats.

It has a high-level Haskell API for creating and unpacking archives in addition
to the C API. Like the `tar` package, it can stream from lazy `ByteString`s.

## Hacking

To run the test suite, first run

```
make
```

so that you have appropriate test data downloaded.

## Performance

`libarchive` is faster than `tar` or `tar-conduit` when unpacking archives.
