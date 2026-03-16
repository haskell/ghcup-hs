{-|
Module      : GHCup.Hardcoded.Version
Description : Version information
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Hardcoded.Version where

import GHCup.Prelude.Version
import GHCup.Types
import Paths_ghcup           ( version )

import Data.Maybe        ( fromJust )
import Data.Version      ( Version (versionBranch) )

import qualified Data.List.NonEmpty     as NE
import qualified Data.Text              as T
import qualified Data.Versions          as V


-- | The current ghcup version.
ghcUpVer :: V.PVP
ghcUpVer = V.PVP . NE.fromList . fmap fromIntegral $ versionBranch version

ghcUpVer' :: GHCTargetVersion
ghcUpVer' = mkTVer $ fromJust $ pvpToVersion ghcUpVer mempty

-- | ghcup version as numeric string.
numericVer :: String
numericVer = T.unpack . V.prettyPVP $ ghcUpVer

