module Main where

import Test.Tasty
import qualified SetTest
import qualified OtherCommandTest
import qualified ChangeLogTest
import qualified ConfigTest
import qualified InstallTest
import qualified UnsetTest

main :: IO ()
main = defaultMain $ testGroup "ghcup"
  [ SetTest.setTests
  , OtherCommandTest.otherCommandTests
  , ChangeLogTest.changeLogTests
  , ConfigTest.configTests
  , InstallTest.installTests
  , UnsetTest.unsetTests
  ]
