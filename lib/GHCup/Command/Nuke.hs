{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GHCup.Command.Nuke
Description : GHCup nuke
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

This module contains the main functions that correspond
to the command line interface, like installation, listing versions
and so on.

These are the entry points.
-}
module GHCup.Command.Nuke where


import GHCup.Prelude
import GHCup.Query.GHCupDirs
import GHCup.Types
import GHCup.Types.Optics

import Conduit                ( sourceToList )
import Control.Applicative
import Control.Exception.Safe
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Resource hiding ( throwM )
import Data.List
import Prelude                      hiding ( abs, writeFile )
import System.FilePath
import System.IO.Error

import qualified Data.Text as T





    ------------
    --[ Nuke ]--
    ------------


rmGhcupDirs :: ( MonadReader env m
               , HasDirs env
               , MonadIO m
               , MonadUnliftIO m
               , HasLog env
               , MonadCatch m
               , MonadMask m )
            => m [FilePath]
rmGhcupDirs = do
  Dirs
    { baseDir
    , binDir
    , logsDir
    , cacheDir
    , recycleDir
    , dbDir
    , tmpDir
    } <- getDirs

  let envFilePath = fromGHCupPath baseDir </> "env"

  confFilePath <- getConfigFilePath

  handleRm $ rmEnvFile  envFilePath
  handleRm $ rmConfFile confFilePath

  -- for xdg dirs, the order matters here
  handleRm $ rmPathForcibly logsDir
  handleRm $ rmPathForcibly tmpDir
  handleRm $ rmPathForcibly cacheDir

  handleRm $ rmBinDir binDir
  handleRm $ rmPathForcibly recycleDir
  handleRm $ rmPathForcibly dbDir
  when isWindows $ do
    logInfo $ "removing " <> T.pack (fromGHCupPath baseDir </> "msys64")
    handleRm $ rmPathForcibly (baseDir `appendGHCupPath` "msys64")

  handleRm $ removeEmptyDirsRecursive (fromGHCupPath baseDir)

  -- report files in baseDir that are left-over after
  -- the standard location deletions above
  hideErrorDef [doesNotExistErrorType] [] $ reportRemainingFiles (fromGHCupPath baseDir)

  where
    handleRm :: (MonadReader env m, MonadCatch m, HasLog env, MonadIO m)  => m () -> m ()
    handleRm = handleIO (\e -> logDebug $ "Part of the cleanup action failed with error: " <> T.pack (displayException e) <> "\n"
                                <> "continuing regardless...")

    rmEnvFile :: (HasLog env, MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmEnvFile enFilePath = do
      logInfo "Removing Ghcup Environment File"
      hideErrorDef [permissionErrorType] () $ rmFileForce enFilePath

    rmConfFile :: (HasLog env, MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmConfFile confFilePath = do
      logInfo "removing Ghcup Config File"
      hideErrorDef [permissionErrorType] () $ rmFileForce confFilePath

    rmBinDir :: (MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmBinDir binDir
      | isWindows = removeDirIfEmptyOrIsSymlink binDir
      | otherwise = do
          isXDGStyle <- liftIO useXDG
          when (not isXDGStyle) $
            removeDirIfEmptyOrIsSymlink binDir

    reportRemainingFiles :: (MonadMask m, MonadIO m, MonadUnliftIO m) => FilePath -> m [FilePath]
    reportRemainingFiles dir = do
      remainingFiles <- runResourceT $ sourceToList $ getDirectoryContentsRecursiveUnsafe dir
      let normalizedFilePaths = fmap normalise remainingFiles
      let sortedByDepthRemainingFiles = sortBy (flip compareFn) normalizedFilePaths
      let remainingFilesAbsolute = fmap (dir </>) sortedByDepthRemainingFiles

      pure remainingFilesAbsolute

      where
        calcDepth :: FilePath -> Int
        calcDepth = length . filter isPathSeparator

        compareFn :: FilePath -> FilePath -> Ordering
        compareFn fp1 fp2 = compare (calcDepth fp1) (calcDepth fp2)


