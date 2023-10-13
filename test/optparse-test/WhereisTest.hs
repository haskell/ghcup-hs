{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WhereisTest where

import Test.Tasty
import GHCup.OptParse
import Utils
import GHCup.Types
import Data.Versions (versionQ)

whereisTests :: TestTree
whereisTests = buildTestTree whereisParseWith ("whereis", whereisCheckList)

whereisCheckList :: [(String, (WhereisOptions, WhereisCommand))]
whereisCheckList = concatMap mk
  [ ("whereis ghc", WhereisTool GHC Nothing)
  , ("whereis ghc 9.2.8", WhereisTool GHC (Just $ GHCVersion $ mkTVer $(versionQ "9.2.8")))
  , ("whereis ghc ghc-9.2.8", WhereisTool GHC (Just $ GHCVersion $ GHCTargetVersion (Just "ghc") $(versionQ "9.2.8")))
  , ("whereis ghc latest", WhereisTool GHC (Just $ ToolTag Latest))
  , ("whereis cabal", WhereisTool Cabal Nothing)
  , ("whereis hls", WhereisTool HLS Nothing)
  , ("whereis stack", WhereisTool Stack Nothing)
  , ("whereis ghcup", WhereisTool GHCup Nothing)
  , ("whereis basedir", WhereisBaseDir)
  , ("whereis bindir", WhereisBinDir)
  , ("whereis cachedir", WhereisCacheDir)
  , ("whereis logsdir", WhereisLogsDir)
  , ("whereis confdir", WhereisConfDir)
  ]
  where
    mk :: (String, WhereisCommand) -> [(String, (WhereisOptions, WhereisCommand))]
    mk (cmd, res) =
      [ (cmd, (WhereisOptions False, res))
      , (cmd <> " -d", (WhereisOptions True, res))
      , (cmd <> " --directory", (WhereisOptions True, res))
      ]

whereisParseWith :: [String] -> IO (WhereisOptions, WhereisCommand)
whereisParseWith args = do
  Whereis a b <- parseWith args
  pure (a, b)
