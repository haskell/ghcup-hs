module OtherCommandTest where

import Test.Tasty
import Test.Tasty.HUnit
import GHCup.OptParse
import Utils
import Control.Monad.IO.Class

otherCommandTests :: TestTree
otherCommandTests = testGroup "other command"
  [ testCase "debug-info" $ do
      res <- parseWith ["debug-info"]
      liftIO $ assertBool "debug-info parse failed" (isDInfo res)
  , testCase "tool-requirements" $ do
      ToolRequirements opt <- parseWith ["tool-requirements"]
      liftIO $ tlrRaw opt @?= False
  , testCase "tool-requirements -r" $ do
      ToolRequirements opt <- parseWith ["tool-requirements", "--raw-format"]
      liftIO $ tlrRaw opt @?= True
  ]

isDInfo :: Command -> Bool
isDInfo DInfo = True
isDInfo _     = False
