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
  when (not isWindows) $
    roundtripAndGoldenSpecsWithSettings (defaultSettings { goldenDirectoryOption = CustomDirectoryName "test/ghcup-test/golden/unix", sampleSize = 2 }) (Proxy @GHCupInfo)

