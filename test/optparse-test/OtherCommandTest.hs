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
  , testCase "nuke" $ do
      res <- parseWith ["nuke"]
      liftIO $ assertBool "nuke parse failed" (isNuke res)
  , testCase "test ghc" $ do
      res <- parseWith ["test", "ghc"]
      liftIO $ assertBool "test parse failed" (isTest res)
  ]

isDInfo :: Command -> Bool
isDInfo DInfo = True
isDInfo _     = False

isNuke :: Command -> Bool
isNuke Nuke = True
isNuke _    = False

isTest :: Command -> Bool
isTest (Test _) = True
isTest _        = False
