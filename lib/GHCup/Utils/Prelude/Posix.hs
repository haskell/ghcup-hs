module GHCup.Utils.Prelude.Posix where

import           System.Directory hiding ( removeDirectory
                                         , removeDirectoryRecursive
                                         , removePathForcibly
                                         , findFiles
                                         )
import System.Posix.Files


isWindows, isNotWindows :: Bool
isWindows = False
isNotWindows = not isWindows


moveFile :: FilePath -> FilePath -> IO ()
moveFile = rename


moveFilePortable :: FilePath -> FilePath -> IO ()
moveFilePortable from to = do
  copyFile from to
  removeFile from

