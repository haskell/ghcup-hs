module Main where
import Test.Tasty
import qualified SetTest
import qualified DebugInfoTest

main :: IO ()
main = defaultMain $ testGroup "ghcup"
  [ SetTest.setTests
  , DebugInfoTest.debugInfoTests
  ]