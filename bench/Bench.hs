module Main (main) where

import           Test.Tasty.Bench
import qualified BenchList


main :: IO ()
main = do
  defaultMain [ BenchList.benchMark
              ]

