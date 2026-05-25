{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module GHCup.Query.DB.HLS where

import GHCup.Legacy.Utils
import GHCup.Prelude (isWindows)
import GHCup.Query.DB
import GHCup.Types
import GHCup.Types.Optics

import Control.Monad.Reader ( MonadReader )
import Data.List            ( stripPrefix )
import Data.Maybe           ( catMaybes )
import Data.Variant.Excepts ( pattern VLeft, pattern VRight, runE )
import Data.Versions        ( Version, version )

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
      let extractGHCVerFromBinary (stripExe -> bin) = do
            prefix <- splitIt '~' bin
            s <- stripPrefix "haskell-language-server-" prefix
            -- stripExe here because we might have a binary of the form
            -- haskell-language-server-9.12.2.exe~2.14.0.0
            either (const Nothing) pure . version . T.pack . stripExe $ s
          ghcs = catMaybes $ extractGHCVerFromBinary <$> bins
      pure ghcs
    -- legacy
    VLeft _ -> do
      hlsGHCVersions' hlsVer
 where
  splitIt ch = go []
   where
    go prefix (x:xs)
      | x == ch = Just prefix
      | otherwise = go (prefix <> [x]) xs
    go _ [] = Nothing

  stripExe f
    | isWindows = case reverse f of
                    ('e':'x':'e':'.':r) -> reverse r
                    _ -> f
    | otherwise = f
