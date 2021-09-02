# RELEASING

1. Update version in `ghcup.cabal` and `boostrap-haskell` (`ghver` variable at the top of the script)

2. Update `GHCup.Version` module. `ghcupURL` must only be updated if we change the `GHCupInfo` type or the YAML representation of it. The version of the YAML represents the change increments. `ghcUpVer` is the current application version, read from `ghcup.cabal`.

3. Add ChangeLog entry

4. Add/fix downloads in `ghcup-<ver>.yaml`, then verify with `ghcup-gen check -f ghcup-<ver>.yaml` and possibly (example only) `ghcup-gen check-tarballs -f ghcup-<ver>.yaml -u 'ghc-8.10.7'`. Generally, new GHC/cabal/stack/hls versions are only added to the latest yaml file. New GHCup versions are added to all (great care must be taken here to not break the parser... e.g. ARM platforms don't parse in all older formats).

5. Commit and git push with tag. Wait for tests to succeed and release artifacts to build.

6. Download release artifacts and upload them `downloads.haskell.org/ghcup` along with checksum files (`sha256sum --tag * > SHA256SUMS && gpg --detach-sign -u <your-email> SHA256SUMS`)

7. Add ghcup release artifacts to ALL yaml files (see point 4.)

8. Upload the final `ghcup-<ver>.yaml` to `webhost.haskell.org/ghcup/data/`.

9. Update `bootstrap-haskell` and `bootstrap-haskell.ps1` to `webhost.haskell.org/ghcup/sh/`

10. Update the ghcup symlinks at `downloads.haskell.org/ghcup`
