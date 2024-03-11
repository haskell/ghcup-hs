module GCTest where

import Test.Tasty
import GHCup.OptParse
import Utils


gcTests :: TestTree
gcTests = buildTestTree gcParseWith ("gc", gcCheckList)

defaultOptions :: GCOptions
defaultOptions =
  GCOptions
    False
    False
    False
    False
    False
    False
    False

gcCheckList :: [(String, GCOptions)]
gcCheckList =
  [ ("gc", defaultOptions)
  , ("gc -o", defaultOptions{gcOldGHC = True})
  , ("gc --ghc-old", defaultOptions{gcOldGHC = True})
  , ("gc -p", defaultOptions{gcProfilingLibs = True})
  , ("gc --profiling-libs", defaultOptions{gcProfilingLibs = True})
  , ("gc -s", defaultOptions{gcShareDir = True})
  , ("gc --share-dir", defaultOptions{gcShareDir = True})
  , ("gc -h", defaultOptions{gcHLSNoGHC = True})
  , ("gc --hls-no-ghc", defaultOptions{gcHLSNoGHC = True})
  , ("gc -c", defaultOptions{gcCache = True})
  , ("gc --cache", defaultOptions{gcCache = True})
  , ("gc -t", defaultOptions{gcTmp = True})
  , ("gc --tmpdirs", defaultOptions{gcTmp = True})
  , ("gc -u", defaultOptions{gcUnset = True})
  , ("gc --unset", defaultOptions{gcUnset = True})
  , ("gc -o -p -s -h -c -t -u", GCOptions True True True True True True True)
  ]

gcParseWith :: [String] -> IO GCOptions
gcParseWith args = do
  GC a <- parseWith args
  pure a
