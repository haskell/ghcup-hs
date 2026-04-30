{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module GHCup.Query.DB.HLS where

import GHCup.Legacy.Utils
import GHCup.Prelude
import GHCup.Query.DB
import GHCup.Types
import GHCup.Types.Optics

import Control.Monad.Reader ( MonadReader )
import Data.List            ( stripPrefix )
import Data.Maybe           ( catMaybes )
import Data.Variant.Excepts ( pattern VLeft, pattern VRight, runE )
import Data.Versions        ( Version, version )
import Safe                 ( headMay )

import qualified Data.Text as T


getHLSGHCs ::
  ( MonadReader env m
  , HasDirs env
  , HasPlatformReq env
  , HasLog env
  , MonadIOish m
  )
  => Version
  -> m [Version]
getHLSGHCs hlsVer = do
  vspec <- runE $ getSymlinkSpec hls (mkTVer hlsVer)
  case vspec of
    VRight (fmap (\SymlinkSpec{..} -> _slLinkName) -> bins) -> do
      let extractGHCVerFromBinary bin = do
            prefix <- headMay $ splitOn "~" bin
            s <- stripPrefix "haskell-language-server-" prefix
            either (const Nothing) pure . version . T.pack $ s
          ghcs = catMaybes $ extractGHCVerFromBinary <$> bins
      pure ghcs
    -- legacy
    VLeft _ -> do
      hlsGHCVersions' hlsVer

