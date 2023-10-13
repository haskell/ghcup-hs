{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell   #-}

module RunTest where

import Test.Tasty
import GHCup.OptParse
import Utils
import GHCup.Types
import Data.Versions (versionQ)


runTests :: TestTree
runTests = buildTestTree runParseWith ("run", runCheckList)

defaultOptions :: RunOptions
defaultOptions =
  RunOptions
    False
    False
    False
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    False
    []

runCheckList :: [(String, RunOptions)]
runCheckList =
  [ ("run", defaultOptions)
  , ("run -a", defaultOptions{runAppendPATH = True})
  , ("run --append", defaultOptions{runAppendPATH = True})
  , ("run -i", defaultOptions{runInstTool' = True})
  , ("run --install", defaultOptions{runInstTool' = True})
  , ("run -m", defaultOptions{runMinGWPath = True})
  , ("run --mingw-path", defaultOptions{runMinGWPath = True})
  , ("run --ghc 9.2.8", defaultOptions{runGHCVer = Just $ GHCVersion $ mkTVer $(versionQ "9.2.8")})
  , ("run --ghc latest", defaultOptions{runGHCVer = Just $ ToolTag Latest})
  , ("run --cabal 3.10", defaultOptions{runCabalVer = Just $ ToolVersion $(versionQ "3.10")})
  , ("run --hls 2.0", defaultOptions{runHLSVer = Just $ ToolVersion $(versionQ "2.0")})
  , ("run --stack 2.9", defaultOptions{runStackVer = Just $ ToolVersion $(versionQ "2.9") })
#ifdef IS_WINDOWS
  , ("run -b C:\\\\tmp\\dir", defaultOptions{runBinDir = Just "C:\\\\tmp\\dir"})
  , ("run --bindir C:\\\\tmp\\dir", defaultOptions{runBinDir = Just "C:\\\\tmp\\dir"})
#else
  , ("run -b /tmp/dir", defaultOptions{runBinDir = Just "/tmp/dir"})
  , ("run --bindir /tmp/dir", defaultOptions{runBinDir = Just "/tmp/dir"})
#endif
  , ("run -q", defaultOptions{runQuick = True})
  , ("run --quick", defaultOptions{runQuick = True})
  , ("run --ghc latest --cabal 3.10 --stack 2.9 --hls 2.0 --install",
        defaultOptions
          { runGHCVer = Just $ ToolTag Latest
          , runCabalVer = Just $ ToolVersion $(versionQ "3.10")
          , runHLSVer = Just $ ToolVersion $(versionQ "2.0")
          , runStackVer = Just $ ToolVersion $(versionQ "2.9")
          , runInstTool' = True
          }
    )
  ]

runParseWith :: [String] -> IO RunOptions
runParseWith args = do
  Run a <- parseWith args
  pure a
