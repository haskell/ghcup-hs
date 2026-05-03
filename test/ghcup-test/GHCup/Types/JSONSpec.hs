{-# LANGUAGE TypeApplications #-}

module GHCup.Types.JSONSpec where

import           GHCup.ArbitraryTypes ()
import           GHCup.Types hiding ( defaultSettings )
import           GHCup.Types.JSON ()
import           GHCup.Prelude

import           Control.Monad (when)
import           Test.Aeson.GenericSpecs
import           Test.Hspec



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
  roundtripSpecs (Proxy @UserSettings)
  roundtripSpecs (Proxy @GHCupInfo)
  when (not isWindows) $
    roundtripAndGoldenSpecsWithSettings (defaultSettings { goldenDirectoryOption = CustomDirectoryName "test/ghcup-test/golden/unix", sampleSize = 2 }) (Proxy @GHCupInfo)

