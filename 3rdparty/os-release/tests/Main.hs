import Golden.Real
import Specs.Megaparsec

import Test.Tasty
import Test.Tasty.Hspec


main :: IO ()
main = do
  ms <- testSpec "megaparsec spec" megaparsecSpec
  gs <- goldenTests
  defaultMain (tests [ms, gs])

tests :: [TestTree] -> TestTree
tests ts = testGroup "Tests" (ts++[])

