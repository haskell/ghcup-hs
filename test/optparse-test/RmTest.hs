{-# LANGUAGE OverloadedStrings #-}

module RmTest where

import Test.Tasty
import GHCup.OptParse
import Utils
import GHCup.Types
import Data.List.NonEmpty (NonEmpty ((:|)))
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
    ("rm 9.2.8", mkTVer (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| [], Digits 8 :| []]))
  , ("rm ghc-9.2.8",  GHCTargetVersion (Just "ghc") (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| [], Digits 8 :| []]))
  ]

rmGhcCheckList :: [(String, Either RmCommand RmOptions)]
rmGhcCheckList = mapSecond (Left . RmGHC . RmOptions)
  [ -- failed with ("rm ghc", xxx)
    ("rm ghc 9.2.8", mkTVer (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| [], Digits 8 :| []]))
  , ("rm ghc ghc-9.2.8",  GHCTargetVersion (Just "ghc") (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| [], Digits 8 :| []]))
  ]

rmCabalCheckList :: [(String, Either RmCommand RmOptions)]
rmCabalCheckList = mapSecond (Left . RmCabal)
  [ -- failed with ("rm cabal", xxx)
    ("rm cabal 3.10", mkVersion $ (Digits 3 :| []) :| [Digits 10 :| []])
  , ("rm cabal cabal-3.10", Version
        { _vEpoch = Nothing
        , _vChunks = (Str "cabal" :| []) :| []
        , _vRel = [Digits 3 :| [], Digits 10 :| []]
        , _vMeta = Nothing
        }
    )
  ]

rmHlsCheckList :: [(String, Either RmCommand RmOptions)]
rmHlsCheckList = mapSecond (Left . RmHLS)
  [ -- failed with ("rm hls", xxx)
    ("rm hls 2.0", mkVersion $ (Digits 2 :| []) :| [Digits 0 :| []])
  , ("rm hls hls-2.0", Version
        { _vEpoch = Nothing
        , _vChunks = (Str "hls" :| []) :| []
        , _vRel = [Digits 2 :| [], Digits 0 :| []]
        , _vMeta = Nothing
        }
    )
  ]

rmStackCheckList :: [(String, Either RmCommand RmOptions)]
rmStackCheckList = mapSecond (Left . RmStack)
  [ -- failed with ("rm stack", xxx)
    ("rm stack 2.9.1", mkVersion $ (Digits 2 :| []) :| [Digits 9 :| [], Digits 1 :| []])
  , ("rm stack stack-2.9.1", Version
        { _vEpoch = Nothing
        , _vChunks = (Str "stack" :| []) :| []
        , _vRel = [Digits 2 :| [], Digits 9 :| [], Digits 1 :| []]
        , _vMeta = Nothing
        }
    )
  ]

rmParseWith :: [String] -> IO (Either RmCommand RmOptions)
rmParseWith args = do
  Rm a <- parseWith args
  pure a
