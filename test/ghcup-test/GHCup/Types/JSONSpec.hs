{-# LANGUAGE TypeApplications #-}

module GHCup.Types.JSONSpec where

import           GHCup.ArbitraryTypes ()
import           GHCup.Types hiding ( defaultSettings )
import           GHCup.Types.JSON ()
import           GHCup.Prelude

import           Test.Aeson.GenericSpecs
import           Test.Hspec



spec :: Spec
spec = do
  roundtripSpecs (Proxy @LinuxDistro)
  roundtripAndGoldenSpecsWithSettings (defaultSettings { goldenDirectoryOption = CustomDirectoryName goldenDir, sampleSize = 2 }) (Proxy @GHCupInfo)
 where
  goldenDir
    | isWindows = "test/ghcup-test/golden/windows"
    | otherwise = "test/ghcup-test/golden/unix"

