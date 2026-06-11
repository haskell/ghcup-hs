{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

module GHCup.ParserSpec where

import GHCup.Prelude.MegaParsec
import GHCup.Prelude.Version.QQ
import GHCup.Types
import GHCup.Types.JSON
    ()
import GHCup.Input.Parsers.Domain

import           Data.Either        ( isLeft )
import           Data.List.NonEmpty ( NonEmpty (..) )
import           Data.Versions
import           Text.Megaparsec

import qualified Data.Map.Strict as M
import qualified Data.Set           as Set
import qualified Text.Megaparsec    as MP

import Test.Hspec

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

      -- With Target
      MP.parse ghcVersionFromPath "" "../ghc/javascript-unknown-ghcjs-9.6.2/bin/javascript-unknown-ghcjs-ghc-9.6.2" `shouldBe` Right ghcjs962
      MP.parse ghcVersionFromPath "" "c:/ghc/bin/ghcup/ghc/javascript-unknown-ghcjs-9.6.2/bin/javascript-unknown-ghcjs-ghc-9.6.2" `shouldBe` Right ghcjs962

#if defined(IS_WINDOWS)
      -- windows
      MP.parse ghcVersionFromPath "" "c:\\ghcup\\ghc\\mytag9.4.8\\bin\\ghc-9.4.8" `shouldBe` Right ghcMytag
      MP.parse ghcVersionFromPath "" "..\\ghc\\9.4.8-rc2\\bin\\ghc-9.4.8" `shouldBe` Right ghc948rc2
#endif

      -- errors
      isLeft (MP.parse ghcVersionFromPath "" "") `shouldBe` True
      isLeft (MP.parse ghcVersionFromPath "" "k") `shouldBe` True
      isLeft (MP.parse ghcVersionFromPath "" "/ghc/") `shouldBe` True

    it "toolVersionFromPath" $ do
      -- other tools
      MP.parse (toolVersionFromPath hls) "" "../hls/2.13.0.0/bin/haskell-language-server-wrapper" `shouldBe` hls2_13_0_0
      MP.parse (toolVersionFromPath cabal) "" "../cabal/3.14.1.0/cabal" `shouldBe` cabal_3_14_1_0

    it "verRevP" $ do
      -- other tools
      MP.parse verRevP "" "1.2.3-roo-rar-r100" `shouldBe` Right VersionReq {
                   _vqVersion = Version {
                     _vEpoch = Nothing,
                     _vChunks = Chunks (Numeric 1 :| [Numeric 2, Numeric 3]),
                     _vRel = Just (Release (Alphanum "roo-rar" :| [])),
                     _vMeta = Nothing
                   },
                   _vqRev = Just 100
                 }
      MP.parse verRevP "" "1.2.3-roo-rar" `shouldBe` Right VersionReq {
                   _vqVersion = Version {
                     _vEpoch = Nothing,
                     _vChunks = Chunks (Numeric 1 :| [Numeric 2, Numeric 3]),
                     _vRel = Just (Release (Alphanum "roo-rar" :| [])),
                     _vMeta = Nothing
                   },
                   _vqRev = Nothing
                 }

    it "ghcLinkVersion" $ do
      MP.parse ghcLinkVersion "" "ghc-8.10.7" `shouldBe` Right ghc8107

    it "domainParserP" $ do
      MP.parse domainParserP "" "${TARGETFN}"               `shouldBe` Right (TARGETFN, Nothing)
      MP.parse domainParserP "" "${TARGETFN//./}"           `shouldBe` Right (TARGETFN, Just $ ReplaceAll "." "")
      MP.parse domainParserP "" "${TARGETFN/foo/bar}"       `shouldBe` Right (TARGETFN, Just $ Replace "foo" "bar")
      MP.parse domainParserP "" "${TARGETFN/\\/foo/\\/bar}" `shouldBe` Right (TARGETFN, Just $ Replace "/foo" "/bar")
      MP.parse domainParserP "" "${PKGVER#foo}"             `shouldBe` Right (PKGVER,   Just $ StripPrefix "foo")
      MP.parse domainParserP "" "${PKGVER%bar}"             `shouldBe` Right (PKGVER,   Just $ StripSuffix "bar")
      MP.parse domainParserP "" "${PKGVER%\\/bar}"          `shouldBe` Right (PKGVER,   Just $ StripSuffix "/bar")

    it "domainParser" $ do
      MP.parse (domainParser $ ml (PKGVER, "1.2.3")) "" "haskell-language-server.exe"
        `shouldBe` Right "haskell-language-server.exe"
      MP.parse (domainParser $ ml (PKGVER, "1.2.3")) "" "haskell-language-server-${PKGVER//./}.exe"
        `shouldBe` Right "haskell-language-server-123.exe"
      MP.parse (domainParser $ ml' [(TARGETFN, "haskell-language-server-9.6.7.exe")
                                   ,(PKGVER, "2.14.0.0")
                                   ]
               ) "" "${TARGETFN%.exe}~${PKGVER}.exe"
        `shouldBe` Right "haskell-language-server-9.6.7~2.14.0.0.exe"
      MP.parse (domainParser $ ml' [(TARGETFN, "haskell-language-server-9.6.7.exe")
                                   ,(PKGVER, "2.14.0.0")
                                   ]
               ) "" "${TARGETFN%.*}~${PKGVER}.exe"
        `shouldBe` Right "haskell-language-server-9~2.14.0.0.exe"

    it "globToRegex" $ do
      globToRegex "foo/bar.ext" `shouldBe` "foo/bar\\.ext"
      globToRegex "[][!]" `shouldBe` "[\\]\\[!]"
      globToRegex "[!][!]" `shouldBe` "\\[!]\\[!]"
      globToRegex "[abk*?]" `shouldBe` "[abk\\*?]"
      globToRegex "foo/bar.ext" `shouldBe` "foo/bar\\.ext"
      globToRegex "f?o/ba*.ext" `shouldBe` "f.o/ba.*\\.ext"

    it "evalDomainVal" $ do
      evalDomainVal (ml (PKGVER, "1.2.3")) (PKGVER, Nothing)                   `shouldBe` Just "1.2.3"
      evalDomainVal (ml (PKGVER, "1.2.3")) (PKGVER, Just $ Replace "." "")     `shouldBe` Just "12.3"
      evalDomainVal (ml (PKGVER, "1.2.3")) (PKGVER, Just $ ReplaceAll "." "")  `shouldBe` Just "123"
      evalDomainVal (ml (PKGVER, "1.2.3")) (PKGVER, Just $ Replace "." ",")    `shouldBe` Just "1,2.3"
      evalDomainVal (ml (PKGVER, "1.2.3")) (PKGVER, Just $ ReplaceAll "." ",") `shouldBe` Just "1,2,3"
      evalDomainVal (ml (PKGVER, "1.2.3")) (PKGVER, Just $ Replace "l" "k")    `shouldBe` Just "1.2.3"
      evalDomainVal (ml (PKGVER, "1.2.3")) (PKGVER, Just $ ReplaceAll "l" "k") `shouldBe` Just "1.2.3"

      evalDomainVal (ml (TARGETFN, "1.2.3")) (PKGVER, Nothing)                   `shouldBe` Nothing
      evalDomainVal (ml (TARGETFN, "1.2.3")) (PKGVER, Just $ Replace "." "")     `shouldBe` Nothing
      evalDomainVal (ml (TARGETFN, "1.2.3")) (PKGVER, Just $ ReplaceAll "." "")  `shouldBe` Nothing
      evalDomainVal (ml (TARGETFN, "1.2.3")) (PKGVER, Just $ Replace "." ",")    `shouldBe` Nothing
      evalDomainVal (ml (TARGETFN, "1.2.3")) (PKGVER, Just $ ReplaceAll "." ",") `shouldBe` Nothing

      evalDomainVal (ml (TARGETFN, "foo/bar.ext")) (TARGETFN, Nothing)                     `shouldBe` Just "foo/bar.ext"
      evalDomainVal (ml (TARGETFN, "foo/bar.ext")) (TARGETFN, Just $ Replace ".ext" "")    `shouldBe` Just "foo/bar"
      evalDomainVal (ml (TARGETFN, "foo/bar.ext")) (TARGETFN, Just $ ReplaceAll ".ext" "") `shouldBe` Just "foo/bar"
      evalDomainVal (ml (TARGETFN, "foo/bar.ext")) (TARGETFN, Just $ StripSuffix ".ext")   `shouldBe` Just "foo/bar"
      evalDomainVal (ml (TARGETFN, "foo/bar.ext")) (TARGETFN, Just $ StripPrefix "foo/")   `shouldBe` Just "bar.ext"
      evalDomainVal (ml (TARGETFN, "foo/bar.ext*")) (TARGETFN, Just $ Replace ".ext[abk*?]" "") `shouldBe` Just "foo/bar"
      evalDomainVal (ml (TARGETFN, "foo/BaR.ext*")) (TARGETFN, Just $ Replace "[!.][B-D][a-b][A-R][.*?]" "bar") `shouldBe` Just "foobarext*"
      evalDomainVal (ml (TARGETFN, "foo*bar.ext")) (TARGETFN, Just $ StripPrefix "foo\\*")   `shouldBe` Just "bar.ext"

  where
    ml (a, b) = M.singleton a b
    ml' = M.fromList
    hls2_13_0_0 = Right TargetVersion {
                   _tvTarget = Nothing,
                   _tvVersion = Version {
                     _vEpoch = Nothing,
                     _vChunks = Chunks (Numeric 2 :| [Numeric 13, Numeric 0, Numeric 0]),
                     _vRel = Nothing,
                     _vMeta = Nothing
                   }
                 }
    cabal_3_14_1_0 = Right TargetVersion {
                   _tvTarget = Nothing,
                   _tvVersion = Version {
                     _vEpoch = Nothing,
                     _vChunks = Chunks (Numeric 3 :| [Numeric 14, Numeric 1, Numeric 0]),
                     _vRel = Nothing,
                     _vMeta = Nothing
                   }
                 }
    ghc8107 = TargetVersion {_tvTarget = Nothing, _tvVersion = Version {_vEpoch = Nothing, _vChunks = Chunks (Numeric 8 :| [Numeric 10,Numeric 7]), _vRel = Nothing, _vMeta = Nothing}}
    ghc948rc2 = TargetVersion {_tvTarget = Nothing, _tvVersion = Version {_vEpoch = Nothing, _vChunks = Chunks (Numeric 9 :| [Numeric 4,Numeric 8]), _vRel = Just (Release (Alphanum "rc2" :| [])), _vMeta = Nothing}}
    ghcMytag = TargetVersion {_tvTarget = Nothing, _tvVersion = Version {_vEpoch = Nothing, _vChunks = Chunks (Alphanum "mytag9" :| [Numeric 4,Numeric 8]), _vRel = Nothing, _vMeta = Nothing}}
    ghcjs962 = TargetVersion {_tvTarget = Just "javascript-unknown-ghcjs", _tvVersion = Version { _vEpoch = Nothing, _vChunks = Chunks (Numeric 9 :| [Numeric 6,Numeric 2]), _vRel = Nothing, _vMeta = Nothing}}
