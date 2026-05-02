{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : GHCup.Hardcoded.URLs
Description : URLs used by GHCup at runtime
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Hardcoded.URLs where

import GHCup.Types

import URI.ByteString
import URI.ByteString.QQ

import qualified Data.Text              as T

-- | This reflects the API version of the YAML.
--
-- Note that when updating this, CI requires that the file exists AND the same file exists at
-- 'https://www.haskell.org/ghcup/exp/ghcup-<ver>.yaml' with some newlines added.
-- TODO: revert to master
ghcupURL :: URI
ghcupURL = [uri|https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-0.1.0.yaml|]

stackSetupURL :: URI
stackSetupURL = [uri|https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/stack-setup-2.yaml|]

shimGenURL :: URI
shimGenURL = [uri|https://downloads.haskell.org/~ghcup/shimgen/shim-2.exe|]

shimGenSHA :: T.Text
shimGenSHA = T.pack "7c55e201f71860c5babea886007c8fa44b861abf50d1c07e5677eb0bda387a70"

channelURL :: ChannelAlias -> URI
channelURL = \case
  DefaultChannel -> ghcupURL
  StackChannel -> stackSetupURL
  CrossChannel -> [uri|https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-cross-0.1.0.yaml|]
  PrereleasesChannel -> [uri|https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-prereleases-0.1.0.yaml|]
  VanillaChannel -> [uri|https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-vanilla-0.1.0.yaml|]
  ThirdPartyChannel -> [uri|https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-3rdparty-0.1.0.yaml|]

