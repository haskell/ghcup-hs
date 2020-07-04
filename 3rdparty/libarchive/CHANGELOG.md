# libarchive

## 2.2.5.0

  * Speed improvements in places
  * Add `throwArchiveM` convenience function

## 2.2.4.0

  * Add convenience functions for `.xar` archives

## 2.2.3.0

  * Add convenience functions for working with `.cpio` archives

## 2.2.2.0

  * Add `Ord` instance to `Entry`, `Symlink`, `EntryContent`, `Ownership`
  * Make `content` field of `Entry` lazy
  * Add `Eq` instance to `ArchiveEncryption`

## 2.2.1.0

  * Add `Exception` instance for `ArchiveResult`

## 2.2.0.2

  * Use `bracket` where it doesn't crash GHC

## 2.2.0.1

  * Use `bracket` in a few places where it doesn't crash GHC

## 2.2.0.0

  * Haskell `Entry` type now includes `Symlink` field

## 2.1.3.2

  * Fix segfault in strict function

## 2.1.3.0

  * `archiveVersionString` &c. are now pure

## 2.1.2.1

  * Fixed bug that would cause segfaults on lazy bytestrings with large chunks

## 2.1.2.0

  * More complete API

## 2.1.1.0

  * Remove weeds, export things that were missing from past releases

## 2.1.0.1

  * `packEntries` and friends now detect hardlinks
  * Add `Cabal` to `custom-setup` depends to ensure builds work with stack

## 2.1.0.0

  * Remove `archiveEntryAclNext` since it doesn't exist in the static linked
    library. This means `libarchive` can be used in profiling builds.

## 2.0.0.2

  * Export `FilePtr` type constructor
  * Clean up spurious code

## 2.0.0.1

  * Polish documentation

## 2.0.0.0

  * Fix typo in documentation
  * Improve docs
  * `archiveReadOpenMemory` now accepts an argument of type `Ptr a` rather
    than `Ptr CChar`
  * `unpackToDirLazy`, `unpackArchive`, and `archiveUnpackToDir` now occur in the `ArchiveM` monad
  * `readArchiveBSL` and `readArchiveBS` now return `Either ArchiveResult [Entry]` rather than
    failing silently
  * `readArchiveFile` now returns an `ArchiveM [Entry]` rather than returning an
    `IO [Entry]`
  * `enriesToFile`, `entriesToFile7Zip`, and `entriesToFileZip` now occur in the
    `ArchiveM` monad
  * Make various parts of an `Entry` optional
  * Add `packToFile` functions and `packFiles` functions
  * Remove `ArchiveError` newtype, replace it with `ArchiveResult`
  * Fix bug in `archiveEntryMTimeIsSet`
  * Add `archiveEntryACLEntryInherited`, `archiveEntryACLStyleSolaris`,
    `archiveEntryACLStyleSeparatorComma`, `archiveEntryACLStyleCompact`
  * Add `archiveReadDiskNoAcl`, `archiveReadDiskNoFFlags`
  * Depend on `libarchive` >= 3.4.0
  * Remove `Raw` modules, use c2hs throughout.
  * Fix potential bug with lazy bytestrings of nonstandard size

## 1.0.5.1

  * Add `cross` flag

## 1.0.5.0

  * Add facilities for lazy packing, e.g. `entriesToBSL`
  * Minor documentation fixes

## 1.0.4.0

  * Add `noOpenCallback`
  * Add various facilities for lazy/streaming archives, viz. `unpackToDirLazy`,
    `bslToArchive`, and `readArchiveBSL`.
  * Remove `unsafe` stuff everywhere

## 1.0.3.0

  * Fix types for `archive_set_read_callback` and
    `archive_read_set_seek_callback`

## 1.0.2.0

  * Add `Eq` instance for `ArchiveFormat`

## 1.0.1.0

  * Remove functions from libarchive 3.3.3

## 1.0.0.0

  * Get rid of `cbits`
  * Add low-level FFI bindings
  * Add high-level functions for unpacking archives

## 0.2.1.2

  * Stream from a file when using `unpackArchive`

## 0.2.1.1

  * Preserve modification times by default

## 0.2.1.0

  * Enable autodetection of archive format/compression
  * Slightly improved docs
  * Rename `unpackTarball` to `unpackArchive`

## 0.2.0.0

  * Fix bug in paths
