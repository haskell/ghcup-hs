# RELEASING

1. update `GHCup.Version` module. `ghcupURL` must only be updated if we change the `_toolRequirements` type or the JSON representation of it. The version of the json represents the change increments. `ghcUpVer` is the current application version.

2. Add/fix downloads to `GHCupDownloads` module, then run `ghcup-gen gen` to generate the new json and validate it via `ghcup-gen check`.

3. Commit and git push with tag. Wait for tests to succeed.

4. Upload the new `ghcup-<ver>.json` to `webhost.haskell.org/ghcup/data/`.

5. Build ghcup releases for Linux (fully static), mac (with `-fcurl`) and FreeBSD (with `-fcurl`). Upload to `webhost.haskell.org/ghcup/bin/` and update symlinks.
