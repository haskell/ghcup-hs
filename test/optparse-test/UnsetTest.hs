{-# LANGUAGE OverloadedStrings #-}

module UnsetTest where

import Test.Tasty
import GHCup.OptParse
import Utils


unsetTests :: TestTree
unsetTests =
  testGroup "unset"
    $ map (buildTestTree unsetParseWith)
        [ ("ghc", unsetGhcCheckList)
        , ("cabal", unsetCabalCheckList)
        , ("hls", unsetHlsCheckList)
        , ("stack", unsetStackCheckList)
        ]

unsetGhcCheckList :: [(String, UnsetCommand)]
unsetGhcCheckList = mapSecond (UnsetGHC . UnsetOptions)
  [ ("unset ghc", Nothing)
  , ("unset ghc armv7-unknown-linux-gnueabihf", Just "armv7-unknown-linux-gnueabihf")
  ]

unsetCabalCheckList :: [(String, UnsetCommand)]
unsetCabalCheckList = mapSecond (UnsetCabal . UnsetOptions)
  [ ("unset cabal", Nothing)
    -- This never used
  , ("unset cabal armv7-unknown-linux-gnueabihf", Just "armv7-unknown-linux-gnueabihf")
  ]

unsetHlsCheckList :: [(String, UnsetCommand)]
unsetHlsCheckList = mapSecond (UnsetHLS . UnsetOptions)
  [ ("unset hls", Nothing)
    -- This never used
  , ("unset hls armv7-unknown-linux-gnueabihf", Just "armv7-unknown-linux-gnueabihf")
  ]

unsetStackCheckList :: [(String, UnsetCommand)]
unsetStackCheckList = mapSecond (UnsetStack . UnsetOptions)
  [ ("unset stack", Nothing)
    -- This never used
  , ("unset stack armv7-unknown-linux-gnueabihf", Just "armv7-unknown-linux-gnueabihf")
  ]

unsetParseWith :: [String] -> IO UnsetCommand
unsetParseWith args = do
  UnSet a <- parseWith args
  pure a
