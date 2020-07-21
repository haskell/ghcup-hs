{-|
Module      : GHCup.Data.GHCupInfo
Description :
Copyright   : (c) Julian Ospald, 2020
License     : GPL-3
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX
-}
module GHCup.Data.GHCupInfo where

import           GHCup.Data.GHCupDownloads
import           GHCup.Data.ToolRequirements
import           GHCup.Types


ghcupInfo :: GHCupInfo
ghcupInfo = GHCupInfo { _toolRequirements = toolRequirements
                      , _ghcupDownloads   = ghcupDownloads
                      }
