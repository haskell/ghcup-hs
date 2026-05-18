module Main (main) where

import           Test.Tasty.Bench
import qualified BenchList
import qualified BenchParse


main :: IO ()
main = do
  defaultMain [ BenchParse.benchMark
              , BenchList.benchMark
              ]

