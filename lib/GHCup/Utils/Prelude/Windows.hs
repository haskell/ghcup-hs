module GHCup.Utils.Prelude.Windows where

import qualified System.Win32.File             as Win32


isWindows, isNotWindows :: Bool
isWindows = True
isNotWindows = not isWindows


moveFile :: FilePath -> FilePath -> IO ()
moveFile from to = Win32.moveFileEx from (Just to) 0


moveFilePortable :: FilePath -> FilePath -> IO ()
moveFilePortable = Win32.moveFile

