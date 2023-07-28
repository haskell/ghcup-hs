module RunTest where

import Test.Tasty
import GHCup.OptParse
import Utils


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
runCheckList = []

runParseWith :: [String] -> IO RunOptions
runParseWith args = do
  Run a <- parseWith args
  pure a
