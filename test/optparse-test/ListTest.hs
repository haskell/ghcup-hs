module ListTest where

import Test.Tasty
import GHCup.OptParse
import Utils
import GHCup.List
import GHCup.Types


listTests :: TestTree
listTests = buildTestTree listParseWith ("list", listCheckList)

defaultOptions :: ListOptions
defaultOptions = ListOptions Nothing Nothing Nothing Nothing False False False

listCheckList :: [(String, ListOptions)]
listCheckList =
  [ ("list", defaultOptions)
  , ("list -t ghc", defaultOptions{loTool = Just GHC})
  , ("list -t cabal", defaultOptions{loTool = Just Cabal})
  , ("list -t hls", defaultOptions{loTool = Just HLS})
  , ("list -t stack", defaultOptions{loTool = Just Stack})
  , ("list -c installed", defaultOptions{lCriteria = Just $ ListInstalled True})
  , ("list -c +installed", defaultOptions{lCriteria = Just $ ListInstalled True})
  , ("list -c -installed", defaultOptions{lCriteria = Just $ ListInstalled False})
  , ("list -c set", defaultOptions{lCriteria = Just $ ListSet True})
  , ("list -c +set", defaultOptions{lCriteria = Just $ ListSet True})
  , ("list -c -set", defaultOptions{lCriteria = Just $ ListSet False})
  , ("list -c available", defaultOptions{lCriteria = Just $ ListAvailable True})
  , ("list -c +available", defaultOptions{lCriteria = Just $ ListAvailable True})
  , ("list -c -available", defaultOptions{lCriteria = Just $ ListAvailable False})
  , ("list -s 2023-07-22", defaultOptions{lFrom = Just $ read "2023-07-22"})
  , ("list -u 2023-07-22", defaultOptions{lTo = Just $ read "2023-07-22"})
  , ("list --since 2023-07-22 --until 2023-07-22", defaultOptions{lFrom = Just $ read "2023-07-22", lTo = Just $ read "2023-07-22"})
  , ("list -o", defaultOptions{lHideOld = True})
  , ("list --hide-old", defaultOptions{lHideOld = True})
  , ("list -n", defaultOptions{lShowNightly = True})
  , ("list --show-nightly", defaultOptions{lShowNightly = True})
  , ("list -r", defaultOptions{lRawFormat = True})
  , ("list --raw-format", defaultOptions{lRawFormat = True})
  ]

listParseWith :: [String] -> IO ListOptions
listParseWith args = do
  List a <- parseWith args
  pure a
