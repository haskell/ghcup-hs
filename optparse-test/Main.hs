{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHCup.OptParse as GHCup
import Test.Tasty
import Test.Tasty.HUnit
import Options.Applicative
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHCup.Types
import Data.Versions
import Data.List.NonEmpty (NonEmpty ((:|)))


main :: IO ()
main = defaultMain setTests

setTests :: TestTree
setTests =
  testGroup "set"
    [ check "set" (Right $ SetOptions SetRecommended)
    , check "set ghc" (Left $ SetGHC $ SetOptions SetRecommended)
    , check "set ghc-9.2" (Right $ SetOptions
        $ SetGHCVersion
          (GHCTargetVersion (Just "ghc")
          (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| []])))
    , check "set next" (Right $ SetOptions SetNext)
    , check "set latest" (Right $ SetOptions $ SetToolTag Latest)
    , check "set ghc-javascript-unknown-ghcjs-9.6"
        (Right $ SetOptions
          $ SetGHCVersion
            (GHCTargetVersion
              (Just "ghc-javascript-unknown-ghcjs")
              (mkVersion $ (Digits 9 :| []) :| [Digits 6 :| []])
            )
        )
    , check "set next" (Right $ SetOptions SetNext)
    , check "set nightly" (Right $ SetOptions
        $ SetGHCVersion
          (GHCTargetVersion
            Nothing
            (mkVersion $ (Str "nightly" :| []) :| [])
          )
        )
    , check "set cabal-3.10"
        (Right $ SetOptions
          $ SetGHCVersion
            (GHCTargetVersion
              (Just "cabal")
              (mkVersion $ (Digits 3 :| []) :| [Digits 10 :| []])
            )
        )
    , check "set latest" (Right $ SetOptions $ SetToolTag Latest)
    ]
  where
    check :: String -> Either SetCommand SetOptions -> TestTree
    check args expected = testCase args $ do
      res <- setParseWith (words args)
      liftIO $ res @?= expected

    mkVersion :: NonEmpty VChunk -> Version
    mkVersion chunks = Version Nothing chunks [] Nothing

setParseWith :: [String] -> IO (Either SetCommand SetOptions)
setParseWith args = do
  Set a <- parseWith args
  pure a

parseWith :: [String] -> IO Command
parseWith args =
  optCommand <$> handleParseResult
    (execParserPure defaultPrefs (info GHCup.opts fullDesc) args)
