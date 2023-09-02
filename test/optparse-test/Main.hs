module Main where

import Test.Tasty
import qualified SetTest
import qualified OtherCommandTest
import qualified ChangeLogTest
import qualified ConfigTest
import qualified InstallTest
import qualified UnsetTest
import qualified RmTest
import qualified ListTest
import qualified UpgradeTest
import qualified CompileTest
import qualified WhereisTest
import qualified GCTest
import qualified RunTest

main :: IO ()
main = defaultMain $ testGroup "ghcup"
  [ SetTest.setTests
  , OtherCommandTest.otherCommandTests
  , ChangeLogTest.changeLogTests
  , ConfigTest.configTests
  , InstallTest.installTests
  , UnsetTest.unsetTests
  , RmTest.rmTests
  , ListTest.listTests
  , UpgradeTest.upgradeTests
  , CompileTest.compileTests
  , WhereisTest.whereisTests
  , GCTest.gcTests
  , RunTest.runTests
  ]
