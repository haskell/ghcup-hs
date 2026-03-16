{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module RmTest where

import Test.Tasty
import GHCup.OptParse
import Utils
import GHCup.Types
import Data.Versions


rmTests :: TestTree
rmTests =
  testGroup "rm"
    $ map (buildTestTree rmParseWith)
        [ ("ghc", rmGhcCheckList)
        , ("cabal", rmCabalCheckList)
        , ("hls", rmHlsCheckList)
        , ("stack", rmStackCheckList)
        ]

rmGhcCheckList :: [(String, RmCommand)]
rmGhcCheckList = mapSecond (RmGHC . RmOptions)
  [ -- failed with ("rm ghc", xxx)
    ("rm ghc 9.2.8", mkTVer $(versionQ "9.2.8"))
  ]

rmCabalCheckList :: [(String, RmCommand)]
rmCabalCheckList = mapSecond RmCabal
  [ -- failed with ("rm cabal", xxx)
    ("rm cabal 3.10", $(versionQ "3.10"))
  ]

rmHlsCheckList :: [(String, RmCommand)]
rmHlsCheckList = mapSecond RmHLS
  [ -- failed with ("rm hls", xxx)
    ("rm hls 2.0", $(versionQ "2.0"))
  ]

rmStackCheckList :: [(String, RmCommand)]
rmStackCheckList = mapSecond RmStack
  [ -- failed with ("rm stack", xxx)
    ("rm stack 2.9.1", $(versionQ "2.9.1"))
  ]

rmParseWith :: [String] -> IO RmCommand
rmParseWith args = do
  Rm a <- parseWith args
  pure a
