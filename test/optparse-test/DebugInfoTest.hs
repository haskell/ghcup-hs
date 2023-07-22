module DebugInfoTest where

import Test.Tasty
import Test.Tasty.HUnit
import GHCup.OptParse
import Utils
import Control.Monad.IO.Class

debugInfoTests :: TestTree
debugInfoTests =
  testGroup "debug-info" $ pure
    $ testCase "1. debug-info" $ do
        res <- parseWith ["debug-info"]
        liftIO $ assertBool "debug-info parse failed" (isDInfo res)
  where
    isDInfo :: Command -> Bool
    isDInfo DInfo = True
    isDInfo _     = False
