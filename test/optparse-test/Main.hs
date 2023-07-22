module Main where
import Test.Tasty
import qualified SetTest
import qualified OtherCommandTest
import qualified ChangeLogTest

main :: IO ()
main = defaultMain $ testGroup "ghcup"
  [ SetTest.setTests
  , OtherCommandTest.otherCommandTests
  , ChangeLogTest.changeLogTests
  ]
