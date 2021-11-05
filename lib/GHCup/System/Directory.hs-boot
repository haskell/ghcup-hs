module GHCup.System.Directory where

import           Text.Regex.Posix

findFiles :: FilePath -> Regex -> IO [FilePath]
