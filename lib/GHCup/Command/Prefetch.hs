{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : GHCup.Command.Prefetch
Description : GHCup installation functions
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Command.Prefetch where


import GHCup.Download
import GHCup.Errors
import GHCup.Prelude
import GHCup.Types
import GHCup.Types.JSON
    ()
import GHCup.Types.Optics

import Control.Exception.Safe
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Resource hiding ( throwM )
import Data.Maybe
import Data.Variant.Excepts
import Optics
import Prelude                      hiding ( abs, writeFile )



    ---------------------
    --[ Tool fetching ]--
    ---------------------


fetchToolBindist :: ( MonadFail m
                    , MonadMask m
                    , MonadCatch m
                    , MonadReader env m
                    , HasDirs env
                    , HasSettings env
                    , HasPlatformReq env
                    , HasGHCupInfo env
                    , HasLog env
                    , MonadResource m
                    , MonadIO m
                    , MonadUnliftIO m
                    )
                 => TargetVersion
                 -> Tool
                 -> Maybe FilePath
                 -> Excepts
                      '[ DigestError
                       , ContentLengthError
                       , GPGError
                       , DownloadFailed
                       , NoDownload
                       , URIParseError
                       ]
                      m
                      FilePath
fetchToolBindist v t mfp = do
  dlinfo <- liftE $ getDownloadInfo' t v
  liftE $ downloadCached' dlinfo Nothing mfp


fetchToolSrc :: ( MonadFail m
               , MonadMask m
               , MonadCatch m
               , MonadReader env m
               , HasDirs env
               , HasSettings env
               , HasPlatformReq env
               , HasGHCupInfo env
               , HasLog env
               , MonadResource m
               , MonadIO m
               , MonadUnliftIO m
               )
            => Tool
            -> TargetVersion
            -> Maybe FilePath
            -> Excepts
                 '[ DigestError
                  , ContentLengthError
                  , GPGError
                  , DownloadFailed
                  , NoDownload
                  , URIParseError
                  ]
                 m
                 FilePath
fetchToolSrc tool v mfp = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  dlInfo <-
    preview (ix tool % toolVersions % ix v % viSourceDL % _Just) dls
      ?? NoDownload v ghc Nothing
  liftE $ downloadCached' dlInfo Nothing mfp
