{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module GHCup.Types.JSONSpec where

import           GHCup.ArbitraryTypes ()
import           GHCup.Types
import           GHCup.Types.JSON ()

import           Test.Aeson.GenericSpecs
import           Test.Hspec



spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @GHCupInfo)
