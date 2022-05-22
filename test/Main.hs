import Test.Hspec.Runner
import qualified Spec


main :: IO ()
main =
  hspecWith
    defaultConfig
    Spec.spec
