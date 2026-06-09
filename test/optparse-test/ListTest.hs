module ListTest where

import Test.Tasty
import GHCup.OptParse
import Utils
import GHCup.Command.List
import GHCup.Types


listTests :: TestTree
listTests = buildTestTree listParseWith ("list", listCheckList)

defaultOptions :: ListOptions
defaultOptions = ListOptions Nothing [] Nothing Nothing False NShowLatest False ShowUpdates

listCheckList :: [(String, ListOptions)]
listCheckList =
  [ ("list",                defaultOptions)
  , ("list -t ghc",         defaultOptions{loTool = Just [ghc]})
  , ("list -t cabal",       defaultOptions{loTool = Just [cabal]})
  , ("list -t hls",         defaultOptions{loTool = Just [hls]})
  , ("list -t stack",       defaultOptions{loTool = Just [stack]})
  , ("list -c installed",   defaultOptions{lCriteria = [ListInstalled True]})
  , ("list -c +installed",  defaultOptions{lCriteria = [ListInstalled True]})
  , ("list -c -installed",  defaultOptions{lCriteria = [ListInstalled False]})
  , ("list -c set",         defaultOptions{lCriteria = [ListSet True]})
  , ("list -c +set",        defaultOptions{lCriteria = [ListSet True]})
  , ("list -c -set",        defaultOptions{lCriteria = [ListSet False]})
  , ("list -c available",   defaultOptions{lCriteria = [ListAvailable True]})
  , ("list -c +available",  defaultOptions{lCriteria = [ListAvailable True]})
  , ("list -c -available",  defaultOptions{lCriteria = [ListAvailable False]})
  , ("list -s 2023-07-22",  defaultOptions{lFrom = Just $ read "2023-07-22"})
  , ("list -u 2023-07-22",  defaultOptions{lTo = Just $ read "2023-07-22"})
  , ("list --since 2023-07-22 --until 2023-07-22",
                            defaultOptions{lFrom = Just $ read "2023-07-22", lTo = Just $ read "2023-07-22"})
  , ("list -o",             defaultOptions{lHideOld = True})
  , ("list --hide-old",     defaultOptions{lHideOld = True})
  , ("list -n",             defaultOptions{lShowNightly = NShowAll})
  , ("list --show-nightly", defaultOptions{lShowNightly = NShowAll})
  , ("list -r",             defaultOptions{lRawFormat = True})
  , ("list --raw-format",   defaultOptions{lRawFormat = True})
  ]

listParseWith :: [String] -> IO ListOptions
listParseWith args = do
  List a <- parseWith args
  pure a
