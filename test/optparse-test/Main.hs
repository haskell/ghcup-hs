module Main where
import Test.Tasty
import qualified SetTest

main :: IO ()
main = defaultMain SetTest.setTests