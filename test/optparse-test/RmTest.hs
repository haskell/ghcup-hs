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
        [ ("old-style", oldStyleCheckList)
        , ("ghc", rmGhcCheckList)
        , ("cabal", rmCabalCheckList)
        , ("hls", rmHlsCheckList)
        , ("stack", rmStackCheckList)
        ]

oldStyleCheckList :: [(String, Either RmCommand RmOptions)]
oldStyleCheckList = mapSecond (Right . RmOptions)
  [ -- failed with ("rm", xxx)
    ("rm 9.2.8", mkTVer $(versionQ "9.2.8"))
  , ("rm ghc-9.2.8",  GHCTargetVersion (Just "ghc") $(versionQ "9.2.8"))
  ]

rmGhcCheckList :: [(String, Either RmCommand RmOptions)]
rmGhcCheckList = mapSecond (Left . RmGHC . RmOptions)
  [ -- failed with ("rm ghc", xxx)
    ("rm ghc 9.2.8", mkTVer $(versionQ "9.2.8"))
  , ("rm ghc ghc-9.2.8",  GHCTargetVersion (Just "ghc") $(versionQ "9.2.8"))
  ]

rmCabalCheckList :: [(String, Either RmCommand RmOptions)]
rmCabalCheckList = mapSecond (Left . RmCabal)
  [ -- failed with ("rm cabal", xxx)
    ("rm cabal 3.10", $(versionQ "3.10"))
  , ("rm cabal cabal-3.10", $(versionQ "cabal-3.10"))
  ]

rmHlsCheckList :: [(String, Either RmCommand RmOptions)]
rmHlsCheckList = mapSecond (Left . RmHLS)
  [ -- failed with ("rm hls", xxx)
    ("rm hls 2.0", $(versionQ "2.0"))
  , ("rm hls hls-2.0", $(versionQ "hls-2.0"))
  ]

rmStackCheckList :: [(String, Either RmCommand RmOptions)]
rmStackCheckList = mapSecond (Left . RmStack)
  [ -- failed with ("rm stack", xxx)
    ("rm stack 2.9.1", $(versionQ "2.9.1"))
  , ("rm stack stack-2.9.1", $(versionQ "stack-2.9.1"))
  ]

rmParseWith :: [String] -> IO (Either RmCommand RmOptions)
rmParseWith args = do
  Rm a <- parseWith args
  pure a
