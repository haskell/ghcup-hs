module ChangeLogTest where

import Test.Tasty
import GHCup.OptParse
import Utils
import Test.Tasty.HUnit
import Control.Monad.IO.Class
import GHCup.Types
import Data.Versions
import Data.List.NonEmpty (NonEmpty ((:|)))

changeLogTests :: TestTree
changeLogTests = testGroup "changelog" $ map (uncurry check) checkList
  where
    check :: String -> ChangeLogOptions -> TestTree
    check args expected = testCase args $ do
      res <- changeLogParseWith (words args)
      liftIO $ res @?= expected

checkList :: [(String, ChangeLogOptions)]
checkList =
  [ ("changelog", ChangeLogOptions False Nothing Nothing)
  , ("changelog -o", ChangeLogOptions True Nothing Nothing)
  , ("changelog -t ghc", ChangeLogOptions False (Just GHC) Nothing)
  , ("changelog -t cabal", ChangeLogOptions False (Just Cabal) Nothing)
  , ("changelog -t hls", ChangeLogOptions False (Just HLS) Nothing)
  , ("changelog -t stack", ChangeLogOptions False (Just Stack) Nothing)
  , ("changelog -t ghcup", ChangeLogOptions False (Just GHCup) Nothing)
  , ("changelog 9.2", ChangeLogOptions False Nothing
      (Just $ GHCVersion
        $ GHCTargetVersion
          Nothing
          (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| []]))
    )
  , ("changelog recommended", ChangeLogOptions False Nothing (Just $ ToolTag Recommended))
  , ("changelog -t cabal recommended", ChangeLogOptions False (Just Cabal) (Just $ ToolTag Recommended))
  , ("changelog -t cabal 3.10.1.0", ChangeLogOptions False (Just Cabal)
      (Just $ GHCVersion
        $ GHCTargetVersion
          Nothing
          (mkVersion $ (Digits 3 :| []) :| [Digits 10 :| [],Digits 1 :| [],Digits 0 :| []]))
    )
  , ("changelog 2023-07-22", ChangeLogOptions False Nothing (Just (ToolDay (read "2023-07-22"))))
  ]

changeLogParseWith :: [String] -> IO ChangeLogOptions
changeLogParseWith args = do
  ChangeLog a <- parseWith args
  pure a
