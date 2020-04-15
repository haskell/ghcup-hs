{-# LANGUAGE QuasiQuotes       #-}


module GHCup.Version where

import           GHCup.Utils.Version.QQ

import           Data.Versions
import           URI.ByteString
import           URI.ByteString.QQ

-- | This reflects the API version of the JSON.
ghcupURL :: URI
ghcupURL = [uri|https://www.haskell.org/ghcup/data/ghcup-0.0.1.json|]

ghcUpVer :: PVP
ghcUpVer = [pver|0.1.3|]
