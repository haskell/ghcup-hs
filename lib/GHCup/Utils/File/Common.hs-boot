module GHCup.Utils.File.Common where

import           Text.Regex.Posix

findFiles :: FilePath -> Regex -> IO [FilePath]
