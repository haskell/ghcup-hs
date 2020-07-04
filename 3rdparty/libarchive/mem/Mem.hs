module Main ( main ) where

import           Codec.Archive
import           Control.Exception (throw)

forceList :: [a] -> IO ()
forceList = (`seq` mempty) . last

main :: IO ()
main = readArc

readArc :: IO ()
readArc = forceList =<< throwArchiveM
    (readArchiveFile "test/data/llvm-9.0.0.src.tar")
