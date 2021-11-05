module GHCup.Prelude.Posix where

import System.Directory
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
  
