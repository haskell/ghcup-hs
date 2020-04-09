{-# LANGUAGE QuasiQuotes       #-}


module GHCup.Version where

import           GHCup.Utils.Version.QQ

import           Data.Versions
import           URI.ByteString
import           URI.ByteString.QQ

ghcupURL :: URI
ghcupURL = [uri|https://www.haskell.org/ghcup/data/ghcup-0.0.1.json|]

ghcUpVer :: PVP
ghcUpVer = [pver|0.0.1|]
