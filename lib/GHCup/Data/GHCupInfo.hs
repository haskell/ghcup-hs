module GHCup.Data.GHCupInfo where

import           GHCup.Data.GHCupDownloads
import           GHCup.Data.ToolRequirements
import           GHCup.Types


ghcupInfo :: GHCupInfo
ghcupInfo = GHCupInfo { _toolRequirements = toolRequirements
                      , _ghcupDownloads   = ghcupDownloads
                      }
