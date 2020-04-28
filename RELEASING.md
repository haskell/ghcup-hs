# RELEASING

1. update `GHCup.Version` module. `ghcupURL` must only be updated if we change the `_toolRequirements` type or the JSON representation of it. The version of the json represents the change increments. `ghcUpVer` is the current application version.

2. Add/fix downloads to `GHCupDownloads` module, then run `ghcup-gen gen` to generate the new json and validate it via `ghcup-gen check`.

3. Commit and git push with tag. Wait for tests to succeed and release artifacts to build.

4. Download release artifacts and upload them `downloads.haskell.org/ghcup`

5. Add release artifacts to GHCupDownloads (see point 2.)

6. Upload the final `ghcup-<ver>.json` to `webhost.haskell.org/ghcup/data/`.

