{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module GHCup.Types.JSONSpec where

import GHCup.ArbitraryTypes ()
import GHCup.Prelude
import GHCup.Types hiding ( defaultSettings )
import GHCup.Types.Dhall ()
import GHCup.Types.JSON ()

import Control.Monad (when, forM_)
import Test.Aeson.GenericSpecs
import Test.Hspec.Golden
import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Yaml as YAML

#if defined(DHALL)
import Dhall.Marshal.Encode ()
import qualified Dhall.Core as Dhall
import qualified Dhall.Binary as Dhall
import qualified Dhall hiding (Text)
import qualified Data.Void as V
import Dhall.Yaml (dhallToYaml, defaultOptions)
import Dhall.JSON (convertToHomogeneousMaps, Conversion(..))
import System.FilePath
#endif

spec :: Spec
spec = do
  roundtripSpecs (Proxy @LinuxDistro)
  roundtripSpecs (Proxy @EnvSpec)
  roundtripSpecs (Proxy @ConfigSpec)
  roundtripSpecs (Proxy @MakeSpec)
  roundtripSpecs (Proxy @SymlinkInputSpec)
  roundtripSpecs (Proxy @InstallationSpecInput)
  roundtripSpecs (Proxy @Tool)
  roundtripSpecs (Proxy @TargetVersion)
  roundtripSpecs (Proxy @VersionInfo)
  roundtripSpecs (Proxy @Architecture)
  roundtripSpecs (Proxy @Platform)
  roundtripSpecs (Proxy @VersionRange)
  roundtripSpecs (Proxy @URLSource)
  roundtripSpecs (Proxy @UserSettings)
  roundtripSpecs (Proxy @GHCupDownloads)
  roundtripSpecs (Proxy @ToolInfo)
  roundtripSpecs (Proxy @ToolDescription)
  roundtripSpecs (Proxy @ToolVersionSpec)
  roundtripSpecs (Proxy @VersionMetadata)
  roundtripSpecs (Proxy @RevisionSpec)
  roundtripSpecs (Proxy @VersionInfo)
  roundtripSpecs (Proxy @ArchitectureSpec)
  roundtripSpecs (Proxy @PlatformSpec)
  roundtripSpecs (Proxy @PlatformVersionSpec)
  roundtripSpecs (Proxy @DownloadInfo)
  roundtripSpecs (Proxy @GHCupInfo)
  when (not isWindows) $
    roundtripAndGoldenSpecsWithSettings (defaultSettings { goldenDirectoryOption = CustomDirectoryName "test/ghcup-test/golden/unix", sampleSize = 2 }) (Proxy @GHCupInfo)

  describe "Parse old metadata" $ do
    forM_ yamls $ \v ->
      it v $ do
        (Right info) <- YAML.decodeFileEither @GHCupInfo $ "data/test/ghcup-" <> v
        pure $ Golden {
             output = decUTF8Safe' $ Aeson.encodePretty info
           , encodePretty = T.unpack
           , writeToFile = T.writeFile
           , readFromFile = T.readFile
           , goldenFile = "test/ghcup-test/golden/old_metadata/ghcup-" <> dropExtension v
           , actualFile = Nothing
           , failFirstTime = False
           }

#if defined(DHALL)
  describe "Dhall roundtrip" $ do
    beforeAll (do
        info <- YAML.decodeFileEither @GHCupInfo "data/test/ghcup-0.1.0.yaml" >>= either (fail . show) pure
        let expr = Dhall.denote $ Dhall.embed @GHCupInfo Dhall.inject info
        pure (info, expr)
      ) $ do
          it "Normal dhall" $ \(info, expr) -> do
            nInfo <- roundTripNormal expr
            nInfo `shouldBe` info
          it "Binary dhall" $ \(info, expr) -> do
            bInfo <- roundTripBinary expr
            bInfo `shouldBe` info
          it "dhall-to-yaml" $ \(info, expr) -> do
            yInfo <- roundTripYaml expr
            yInfo `shouldBe` info
#endif
 where
  yamls = [ "0.0.1.json"
          , "0.0.2.json"
          , "0.0.3.yaml"
          , "0.0.4.yaml"
          , "0.0.5.yaml"
          , "0.0.6.yaml"
          , "0.0.7.yaml"
          , "0.0.8.yaml"
          , "0.0.9.yaml"
          , "0.1.0.yaml"
          ]
#if defined(DHALL)
  roundTripNormal :: Dhall.Expr V.Void V.Void -> IO GHCupInfo
  roundTripNormal expr = do
    let dhallCode = Dhall.pretty expr
    Dhall.input @GHCupInfo Dhall.auto dhallCode

  roundTripBinary :: Dhall.Expr V.Void V.Void -> IO GHCupInfo
  roundTripBinary expr = do
    let dhallCode = Dhall.encodeExpression expr
    expr' <- either (fail . show) pure $ Dhall.decodeExpression @V.Void @V.Void dhallCode
    Dhall.rawInput Dhall.auto expr'

  -- using dhall-to-yaml essentially
  roundTripYaml :: Dhall.Expr V.Void V.Void -> IO GHCupInfo
  roundTripYaml (convertToHomogeneousMaps NoConversion -> expr) = do
    let dhallCode = Dhall.pretty expr
    yaml <- dhallToYaml defaultOptions Nothing dhallCode
    either (fail . show) pure $ YAML.decodeEither' yaml
#endif
