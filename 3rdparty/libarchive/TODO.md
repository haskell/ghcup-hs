# Code Maintenance
- [ ] Test suite
  - [ ] Figure out how to test for memory leaks
  - [ ] Space leak via Neil Mitchell
  - [ ] Valgrind?
  - [x] Test edge cases (non-standard chunk sizes)
  - [ ] Test multithreaded (start unpacking in one thread)
- [ ] CI
  - [x] Nix integration (`cabal2nix`)
    - [x] Dependent on upstream
# Performance
- [ ] Fix actual laziness (?) -> appears downstream in sak in particular
  conditions
# Bugs
- [ ] segfault (libarchive?) when I mess with .tar files
- [ ] librachive haskell broken? my .7z and .xar outputs aren't great
  - [ ] Fix space leak in converting stuff (bad; also from verify-archive)
# Features
- [ ] xar? 7zip?
# Documentation
- [ ] Add example in haddocks
- [ ] Document ability to use via `archive-sig`/`archive-libarchive`.
# Upstream
- [ ] PVP for signatures?
- [ ] sig: specify compatible stuff? versions not clear...
- [ ] GHC: `bracket` branch? What is going on??
