{-# LANGUAGE QuasiQuotes       #-}


{-|
Module      : GHCup.Version
Description : Static version information
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX
-}
module GHCup.Version where

import           GHCup.Utils.Version.QQ

import           Data.Versions
import           URI.ByteString
import           URI.ByteString.QQ

import qualified Data.Text                     as T

-- | This reflects the API version of the YAML.
ghcupURL :: URI
ghcupURL = [uri|https://www.haskell.org/ghcup/data/ghcup-0.0.3.yaml|]

-- | The current ghcup version.
ghcUpVer :: PVP
ghcUpVer = [pver|0.1.11|]

-- | ghcup version as numeric string.
numericVer :: String
numericVer = T.unpack . prettyPVP $ ghcUpVer
