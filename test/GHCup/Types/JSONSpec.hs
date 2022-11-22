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
  roundtripAndGoldenSpecsWithSettings (defaultSettings { goldenDirectoryOption = CustomDirectoryName goldenDir }) (Proxy @GHCupInfo)
 where
  goldenDir
    | isWindows = "test/golden/windows"
    | otherwise = "test/golden/unix"

