module GHCup.Utils.Dirs
 ( GHCupPath
 , appendGHCupPath
 , fromGHCupPath
 , createTempGHCupDirectory
 , removeDirectory
 , removeDirectoryRecursive
 , removePathForcibly
 )
 where

import Control.DeepSeq (NFData)


-- | A 'GHCupPath' is a safe sub-path that can be recursively deleted.
newtype GHCupPath = GHCupPath FilePath

instance Show GHCupPath where

instance Eq GHCupPath where

instance Ord GHCupPath where

instance NFData GHCupPath where

appendGHCupPath :: GHCupPath -> FilePath -> GHCupPath

fromGHCupPath :: GHCupPath -> FilePath

createTempGHCupDirectory :: GHCupPath -> FilePath -> IO GHCupPath

removeDirectory :: GHCupPath -> IO ()

removeDirectoryRecursive :: GHCupPath -> IO ()

removePathForcibly :: GHCupPath -> IO ()

