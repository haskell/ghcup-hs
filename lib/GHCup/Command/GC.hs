{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : GHCup.Command.GC
Description : GHCup gc
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Command.GC where


import GHCup.Command.List
import GHCup.Command.Rm
import GHCup.Errors
import GHCup.Legacy.HLS
import GHCup.Legacy.Utils
import GHCup.Prelude
import GHCup.Prelude.String.QQ
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.Query.Metadata
import GHCup.System.Directory
import GHCup.Types
import GHCup.Types.Optics

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Resource hiding ( throwM )
import Data.ByteString              ( ByteString )
import Data.Foldable.WithIndex
import Data.List
import Data.Maybe
import Data.Variant.Excepts
import Optics
import Prelude                      hiding ( abs, writeFile )
import System.FilePath
import Text.Regex.Posix

import qualified Data.Text as T



    --------------------------
    --[ Garbage collection ]--
    --------------------------


rmOldGHC :: ( MonadReader env m
            , HasGHCupInfo env
            , HasPlatformReq env
            , HasDirs env
            , HasLog env
            , MonadIOish m
            )
         => Excepts '[NotInstalled, UninstallFailed, ParseError, MalformedInstallInfo] m ()
rmOldGHC = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  let oldGHCs = toListOf (ix ghc % toolVersions % getTagged Old % to fst) dls
  ghcs <- lift $ getInstalledVersions' ghc
  forM_ ghcs $ \ghcVer -> when (ghcVer `elem` oldGHCs) $ rmToolVersion ghc ghcVer


rmUnsetTools :: ( MonadReader env m
                , HasGHCupInfo env
                , HasPlatformReq env
                , HasDirs env
                , HasLog env
                , MonadIO m
                , MonadFail m
                , MonadMask m
                , MonadUnliftIO m
                )
             => Excepts '[NotInstalled, UninstallFailed, ParseError, MalformedInstallInfo] m ()
rmUnsetTools = do
  vers <- liftE $ listVersions Nothing [ListInstalled True, ListSet False] False True (Nothing, Nothing)
  iforM_ vers $ \tool (_, ls) -> forM_ ls $ \ListResult{..} -> liftE $ rmToolVersion tool (TargetVersion lCross lVer)


rmProfilingLibs :: ( MonadReader env m
                   , HasDirs env
                   , HasLog env
                   , MonadIO m
                   , MonadFail m
                   , MonadMask m
                   , MonadUnliftIO m
                   )
                => m ()
rmProfilingLibs = do
  ghcs <- getInstalledVersions' ghc

  let regexes :: [ByteString]
      regexes = [[s|.*_p\.a$|], [s|.*\.p_hi$|]]

  forM_ regexes $ \regex ->
    forM_ ghcs $ \ghc' -> do
      d <- ghcupGHCDir ghc'
      -- TODO: audit findFilesDeep
      matches <- liftIO $ handleIO (\_ -> pure []) $ findFilesDeep
        d
        (makeRegexOpts compExtended
                       execBlank
                       regex
        )
      forM_ matches $ \m -> do
        let p = fromGHCupPath d </> m
        logDebug $ "rm " <> T.pack p
        rmFile p



rmShareDir :: ( MonadReader env m
              , HasDirs env
              , HasLog env
              , MonadIO m
              , MonadFail m
              , MonadMask m
              , MonadUnliftIO m
              )
           => m ()
rmShareDir = do
  ghcs <- getInstalledVersions' ghc
  forM_ ghcs $ \ghc' -> do
    d <- ghcupGHCDir ghc'
    let p = d `appendGHCupPath` "share"
    logDebug $ "rm -rf " <> T.pack (fromGHCupPath p)
    rmPathForcibly p


rmHLSNoGHC :: ( MonadReader env m
              , HasDirs env
              , HasLog env
              , MonadIO m
              , MonadMask m
              , MonadFail m
              , MonadUnliftIO m
              )
           => Excepts '[NotInstalled, UninstallFailed] m ()
rmHLSNoGHC = do
  Dirs {..} <- getDirs
  ghcs <- lift $ getInstalledVersions' ghc
  hlses <- lift $ getInstalledVersions hls Nothing -- TODO: we don't support cross HLS
  forM_ hlses $ \hls' -> do
    hlsGHCs <- fmap mkTVer <$> hlsGHCVersions' hls'
    let candidates = filter (`notElem` ghcs) hlsGHCs
    if (length hlsGHCs - length candidates) <= 0
    then rmHLSVer hls'
    else
      -- TODO: uff
      forM_ candidates $ \ghc' -> do
        bins1 <- fmap (binDir </>) <$> hlsServerBinaries hls' (Just $ _tvVersion ghc')
        bins2 <- ifM (isLegacyHLS hls') (pure []) $ do
          shs <- hlsInternalServerScripts hls' (Just $ _tvVersion ghc')
          bins <- hlsInternalServerBinaries hls' (Just $ _tvVersion ghc')
          libs <- hlsInternalServerLibs hls' (_tvVersion ghc')
          pure (shs ++ bins ++ libs)
        forM_ (bins1 ++ bins2) $ \f -> do
          logDebug $ "rm " <> T.pack f
          rmFile f
    pure ()


rmCache :: ( MonadReader env m
           , HasDirs env
           , HasLog env
           , MonadIO m
           , MonadMask m
           )
        => m ()
rmCache = do
  Dirs {..} <- getDirs
  contents <- liftIO $ listDirectory (fromGHCupPath cacheDir)
  forM_ contents $ \f -> do
    let p = fromGHCupPath cacheDir </> f
    logDebug $ "rm " <> T.pack p
    rmFile p


rmTmp :: ( MonadReader env m
         , HasDirs env
         , HasLog env
         , MonadIO m
         , MonadMask m
         )
      => m ()
rmTmp = do
  ghcup_dirs <- liftIO getGHCupTmpDirs
  forM_ ghcup_dirs $ \f -> do
    logDebug $ "rm -rf " <> T.pack (fromGHCupPath f)
    rmPathForcibly f

