#/bin/sh

set -ex

## install ghc via old ghcup

mkdir -p ~/.ghcup/bin
curl https://gitlab.haskell.org/haskell/ghcup/raw/master/ghcup > ~/.ghcup/bin/ghcup
chmod +x ~/.ghcup/bin/ghcup

export PATH="$HOME/.ghcup/bin:$PATH"

ghcup install 8.8.3
ghcup install-cabal 3.2.0.0
ghcup set 8.8.3


## install ghcup

cabal update
cabal build -fcurl
cp "$(cabal new-exec --verbose=0 --offline sh -- -c 'command -v ghcup')" "./${ARTIFACT}"
