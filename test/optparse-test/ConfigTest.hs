{-# LANGUAGE QuasiQuotes #-}

module ConfigTest where

import Test.Tasty
import Test.Tasty.HUnit
import GHCup.OptParse
import GHCup.Types (NewURLSource(..))
import Utils
import Control.Monad.IO.Class
import URI.ByteString.QQ

configTests :: TestTree
configTests = testGroup "config" $ map (uncurry check) checkList
  where
    check :: String -> ConfigCommand -> TestTree
    check args expected = testCase args $ do
      res <- configParseWith (words args)
      liftIO $ res @?= expected

checkList :: [(String, ConfigCommand)]
checkList =
  [ ("config", ShowConfig)
  , ("config init", InitConfig)
  , ("config show", ShowConfig)
  , ("config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-prereleases-0.0.7.yaml"
    , AddReleaseChannel False (NewURI [uri|https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-prereleases-0.0.7.yaml|])
    )
  , ("config add-release-channel GHCupURL"
    , AddReleaseChannel False NewGHCupURL
    )
  , ("config add-release-channel StackSetupURL"
    , AddReleaseChannel False NewStackSetupURL
    )
  , ("config set cache true", SetConfig "cache" (Just "true"))
  ]

configParseWith :: [String] -> IO ConfigCommand
configParseWith args = do
  Config a <- parseWith args
  pure a
