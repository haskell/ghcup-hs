{-# LANGUAGE QuasiQuotes       #-}


{-|
Module      : GHCup.Version
Description : Version information and version handling.
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Version where

import           GHCup.Types
import           Paths_ghcup (version)

import           Data.Version (Version(versionBranch))
import           Data.Versions hiding (version)
import           URI.ByteString
import           URI.ByteString.QQ

import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T

-- | This reflects the API version of the YAML.
ghcupURL :: URI
ghcupURL = [uri|https://www.haskell.org/ghcup/data/ghcup-0.0.4.yaml|]

-- | The current ghcup version.
ghcUpVer :: PVP
ghcUpVer = PVP . NE.fromList . fmap fromIntegral $ versionBranch version

-- | ghcup version as numeric string.
numericVer :: String
numericVer = T.unpack . prettyPVP $ ghcUpVer

versionCmp :: Versioning -> VersionCmp -> Bool
versionCmp ver1 (VR_gt ver2)   = ver1 > ver2
versionCmp ver1 (VR_gteq ver2) = ver1 >= ver2
versionCmp ver1 (VR_lt ver2)   = ver1 < ver2
versionCmp ver1 (VR_lteq ver2) = ver1 <= ver2
versionCmp ver1 (VR_eq ver2)   = ver1 == ver2

versionRange :: Versioning -> VersionRange -> Bool
versionRange ver' (SimpleRange cmps) = and $ fmap (versionCmp ver') cmps
versionRange ver' (OrRange cmps range) = 
  versionRange ver' (SimpleRange cmps) || versionRange ver' range

