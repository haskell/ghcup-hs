module Main where

import System.Environment

import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.Zlib as Zlib

main = do args <- getArgs
          case args of
           [] -> B.interact Zlib.compress
           ["-d"] -> B.interact Zlib.decompress
           _ -> do name <- getProgName
                   error $ "usage: " ++ name ++ " [-d] < source > dest"
