module GHCupInfo where

import           GHCupDownloads
import           ToolRequirements
import           GHCup.Types


ghcupInfo :: GHCupInfo
ghcupInfo = GHCupInfo { _toolRequirements = toolRequirements
                      , _ghcupDownloads   = ghcupDownloads
                      }
