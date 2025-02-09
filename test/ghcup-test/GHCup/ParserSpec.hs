{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}

module GHCup.ParserSpec where

import           GHCup.Types
import           GHCup.Types.JSON
import           GHCup.Prelude.Version.QQ
import           GHCup.Prelude.MegaParsec

import           Data.List.NonEmpty             ( NonEmpty (..) )
import qualified Data.Set as Set
import           Data.Versions
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

    it "ghcVersionFromPath" $ do
      MP.parse ghcVersionFromPath "" "../ghc/8.10.7/bin/ghc" `shouldBe` Right ghc8107
      MP.parse ghcVersionFromPath "" "../ghc/8.10.7/bin/ghc-8.10.7" `shouldBe` Right ghc8107
      MP.parse ghcVersionFromPath "" "c:/ghcup/ghc/8.10.7/bin/ghc" `shouldBe` Right ghc8107
      MP.parse ghcVersionFromPath "" "c:/ghcup/ghc/8.10.7/bin/ghc-8.10.7" `shouldBe` Right ghc8107
      MP.parse ghcVersionFromPath "" "c:/ghc/ghcup/ghc/8.10.7/bin/ghc" `shouldBe` Right ghc8107
      MP.parse ghcVersionFromPath "" "c:/ghc/ghcup/ghc/8.10.7/bin/ghc-8.10.7" `shouldBe` Right ghc8107

      -- a user specified version
      MP.parse ghcVersionFromPath "" "../ghc/9.4.8-rc2/bin/ghc-9.4.8" `shouldBe` Right ghc948rc2
      MP.parse ghcVersionFromPath "" "c:/ghcup/ghc/9.4.8-rc2/bin/ghc" `shouldBe` Right ghc948rc2
      MP.parse ghcVersionFromPath "" "c:/ghcup/ghc/9.4.8-rc2/bin/ghc-9.4.8" `shouldBe` Right ghc948rc2
      MP.parse ghcVersionFromPath "" "c:/ghc/ghcup/ghc/9.4.8-rc2/bin/ghc" `shouldBe` Right ghc948rc2
      MP.parse ghcVersionFromPath "" "c:/ghc/ghcup/ghc/9.4.8-rc2/bin/ghc-9.4.8" `shouldBe` Right ghc948rc2

      -- a user specified alphanum
      MP.parse ghcVersionFromPath "" "../ghc/mytag9.4.8/bin/ghc-9.4.8" `shouldBe` Right ghcMytag
      MP.parse ghcVersionFromPath "" "c:/ghcup/ghc/mytag9.4.8/bin/ghc" `shouldBe` Right ghcMytag
      MP.parse ghcVersionFromPath "" "c:/ghcup/ghc/mytag9.4.8/bin/ghc-9.4.8" `shouldBe` Right ghcMytag

  where
    ghc8107 = GHCTargetVersion {_tvTarget = Nothing, _tvVersion = Version {_vEpoch = Nothing, _vChunks = Chunks (Numeric 8 :| [Numeric 10,Numeric 7]), _vRel = Nothing, _vMeta = Nothing}}
    ghc948rc2 = GHCTargetVersion {_tvTarget = Nothing, _tvVersion = Version {_vEpoch = Nothing, _vChunks = Chunks (Numeric 9 :| [Numeric 4,Numeric 8]), _vRel = Just (Release (Alphanum "rc2" :| [])), _vMeta = Nothing}}
    ghcMytag = GHCTargetVersion {_tvTarget = Nothing, _tvVersion = Version {_vEpoch = Nothing, _vChunks = Chunks (Alphanum "mytag9" :| [Numeric 4,Numeric 8]), _vRel = Nothing, _vMeta = Nothing}}
