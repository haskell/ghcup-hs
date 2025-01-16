{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}

module GHCup.ParserSpec where

import           GHCup.Types
import           GHCup.Types.JSON
import           GHCup.Prelude.Version.QQ

import           Data.List.NonEmpty             ( NonEmpty (..) )
import qualified Data.Set as Set
import qualified Text.Megaparsec as MP
import           Text.Megaparsec

import           Test.Hspec

spec :: Spec
spec = do
  describe "GHCup Parsers" $ do
    it "versionRangeP" $ do
      MP.parse versionRangeP "" ">= 8" `shouldBe` Right (SimpleRange (VR_gteq [vers|8|]:| []))
      MP.parse versionRangeP "" "< 9" `shouldBe` Right (SimpleRange (VR_lt [vers|9|]:| []))
      MP.parse versionRangeP "" "<= 10" `shouldBe` Right (SimpleRange (VR_lteq [vers|10|]:| []))
      MP.parse versionRangeP "" "=< 100" `shouldBe` Left (ParseErrorBundle {bundleErrors = FancyError 6 (Set.fromList [ErrorFail "unexpected comparator: =<"]) :| [], bundlePosState = PosState {pstateInput = "=< 100", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}, pstateTabWidth = mkPos 8, pstateLinePrefix = ""}})
      MP.parse versionRangeP "" "> 11" `shouldBe` Right (SimpleRange (VR_gt [vers|11|]:| []))
      MP.parse versionRangeP "" "12" `shouldBe` Right (SimpleRange (VR_eq [vers|12|]:| []))
      MP.parse versionRangeP "" "( >= 8 && < 9 )" `shouldBe` Right (SimpleRange (VR_gteq [vers|8|]:| [VR_lt [vers|9|]]))
      MP.parse versionRangeP "" ">= 3 || < 1" `shouldBe` Right (OrRange (VR_gteq [vers|3|]:| []) (SimpleRange (VR_lt [vers|1|]:|[])))
