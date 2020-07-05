{-# LANGUAGE CPP #-}

module Golden.Real where

import System.OsRelease

import System.FilePath
import Text.Pretty.Simple
import Test.Tasty
import Test.Tasty.Golden

import qualified Data.ByteString.Lazy          as B
import qualified Data.Text.Lazy                as L
import qualified Data.Text.Encoding            as E


goldenTests :: IO TestTree
goldenTests = do
  files <- findByExtension [".in"] (takeDirectory (__FILE__) </> "data")
  return $ testGroup
    "Parse os-release into OsRelease"
    (flip fmap files $ \file ->
      let out = replaceExtension file ".golden"
      in  goldenVsString (takeBaseName file) out (parse file)
    )
 where
  parse f = do
    c <- readFile f
    pure
      . B.fromStrict
      . E.encodeUtf8
      . L.toStrict
      . pShowNoColor
      . parseOsRelease'
      $ c
