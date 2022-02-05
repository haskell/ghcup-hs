{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns          #-}

{-|
Module      : GHCup.Utils.Dirs
Description : Definition of GHCup directories
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Utils.Dirs
  ( getAllDirs
  , ghcupBaseDir
  , ghcupConfigFile
  , ghcupCacheDir
  , ghcupGHCBaseDir
  , ghcupGHCDir
  , ghcupHLSBaseDir
  , ghcupHLSDir
  , mkGhcupTmpDir
  , parseGHCupGHCDir
  , parseGHCupHLSDir
  , relativeSymlink
  , withGHCupTmpDir
  , getConfigFilePath
  , useXDG
  , cleanupTrash
  )
where


import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils.MegaParsec
import           GHCup.Utils.Logger
import           GHCup.Utils.Prelude

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource hiding (throwM)
import           Data.Bifunctor
import           Data.Maybe
import           Data.Versions
import           GHC.IO.Exception               ( IOErrorType(NoSuchThing) )
import           Haskus.Utils.Variant.Excepts
import           Optics
import           System.Directory                                                
import           System.DiskSpace                                                
import           System.Environment
import           System.FilePath
import           System.IO.Temp

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Yaml.Aeson               as Y
import qualified Text.Megaparsec               as MP
import Control.Concurrent (threadDelay)



    ------------------------------
    --[ GHCup base directories ]--
    ------------------------------


-- | ~/.ghcup by default
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_DATA_HOME/ghcup' as per xdg spec.
ghcupBaseDir :: IO FilePath
ghcupBaseDir
  | isWindows = do
      bdir <- fromMaybe "C:\\" <$> lookupEnv "GHCUP_INSTALL_BASE_PREFIX"
      pure (bdir </> "ghcup")
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          bdir <- lookupEnv "XDG_DATA_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".local" </> "share")
          pure (bdir </> "ghcup")
        else do
          bdir <- lookupEnv "GHCUP_INSTALL_BASE_PREFIX" >>= \case
            Just r  -> pure r
            Nothing -> liftIO getHomeDirectory
          pure (bdir </> ".ghcup")


-- | ~/.ghcup by default
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CONFIG_HOME/ghcup' as per xdg spec.
ghcupConfigDir :: IO FilePath
ghcupConfigDir
  | isWindows = ghcupBaseDir
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          bdir <- lookupEnv "XDG_CONFIG_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".config")
          pure (bdir </> "ghcup")
        else do
          bdir <- lookupEnv "GHCUP_INSTALL_BASE_PREFIX" >>= \case
            Just r  -> pure r
            Nothing -> liftIO getHomeDirectory
          pure (bdir </> ".ghcup")


-- | If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_BIN_HOME' env var or defaults to '~/.local/bin'
-- (which, sadly is not strictly xdg spec).
ghcupBinDir :: IO FilePath
ghcupBinDir
  | isWindows = ghcupBaseDir <&> (</> "bin")
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          lookupEnv "XDG_BIN_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".local" </> "bin")
        else ghcupBaseDir <&> (</> "bin")


-- | Defaults to '~/.ghcup/cache'.
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CACHE_HOME/ghcup' as per xdg spec.
ghcupCacheDir :: IO FilePath
ghcupCacheDir
  | isWindows = ghcupBaseDir <&> (</> "cache")
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          bdir <- lookupEnv "XDG_CACHE_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".cache")
          pure (bdir </> "ghcup")
        else ghcupBaseDir <&> (</> "cache")


-- | Defaults to '~/.ghcup/logs'.
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CACHE_HOME/ghcup/logs' as per xdg spec.
ghcupLogsDir :: IO FilePath
ghcupLogsDir
  | isWindows = ghcupBaseDir <&> (</> "logs")
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          bdir <- lookupEnv "XDG_CACHE_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".cache")
          pure (bdir </> "ghcup" </> "logs")
        else ghcupBaseDir <&> (</> "logs")


-- | '~/.ghcup/trash'.
-- Mainly used on windows to improve file removal operations
ghcupRecycleDir :: IO FilePath
ghcupRecycleDir = ghcupBaseDir <&> (</> "trash")



getAllDirs :: IO Dirs
getAllDirs = do
  baseDir    <- ghcupBaseDir
  binDir     <- ghcupBinDir
  cacheDir   <- ghcupCacheDir
  logsDir    <- ghcupLogsDir
  confDir    <- ghcupConfigDir
  recycleDir <- ghcupRecycleDir
  pure Dirs { .. }



    -------------------
    --[ GHCup files ]--
    -------------------

getConfigFilePath :: (MonadIO m) => m FilePath
getConfigFilePath = do
  confDir <- liftIO ghcupConfigDir
  pure $ confDir </> "config.yaml"

ghcupConfigFile :: (MonadIO m)
                => Excepts '[JSONError] m UserSettings
ghcupConfigFile = do
  filepath <- getConfigFilePath
  contents <- liftIO $ handleIO' NoSuchThing (\_ -> pure Nothing) $ Just <$> BS.readFile filepath
  case contents of
      Nothing -> pure defaultUserSettings
      Just contents' -> lE' JSONDecodeError . first displayException . Y.decodeEither' $ contents'


    -------------------------
    --[ GHCup directories ]--
    -------------------------


-- | ~/.ghcup/ghc by default.
ghcupGHCBaseDir :: (MonadReader env m, HasDirs env) => m FilePath
ghcupGHCBaseDir = do
  Dirs {..}  <- getDirs
  pure (baseDir </> "ghc")


-- | Gets '~/.ghcup/ghc/<ghcupGHCDir>'.
-- The dir may be of the form
--   * armv7-unknown-linux-gnueabihf-8.8.3
--   * 8.8.4
ghcupGHCDir :: (MonadReader env m, HasDirs env, MonadThrow m)
            => GHCTargetVersion
            -> m FilePath
ghcupGHCDir ver = do
  ghcbasedir <- ghcupGHCBaseDir
  let verdir = T.unpack $ tVerToText ver
  pure (ghcbasedir </> verdir)


-- | See 'ghcupToolParser'.
parseGHCupGHCDir :: MonadThrow m => FilePath -> m GHCTargetVersion
parseGHCupGHCDir (T.pack -> fp) =
  throwEither $ MP.parse ghcTargetVerP "" fp

parseGHCupHLSDir :: MonadThrow m => FilePath -> m Version
parseGHCupHLSDir (T.pack -> fp) =
  throwEither $ MP.parse version' "" fp

-- | ~/.ghcup/hls by default, for new-style installs.
ghcupHLSBaseDir :: (MonadReader env m, HasDirs env) => m FilePath
ghcupHLSBaseDir = do
  Dirs {..}  <- getDirs
  pure (baseDir </> "hls")

-- | Gets '~/.ghcup/hls/<hls-ver>' for new-style installs.
ghcupHLSDir :: (MonadReader env m, HasDirs env, MonadThrow m)
            => Version
            -> m FilePath
ghcupHLSDir ver = do
  basedir <- ghcupHLSBaseDir
  let verdir = T.unpack $ prettyVer ver
  pure (basedir </> verdir)

mkGhcupTmpDir :: ( MonadReader env m
                 , HasDirs env
                 , MonadUnliftIO m
                 , HasLog env
                 , MonadCatch m
                 , MonadThrow m
                 , MonadMask m
                 , MonadIO m)
              => m FilePath
mkGhcupTmpDir = do
  tmpdir <- liftIO getCanonicalTemporaryDirectory

  let minSpace = 5000 -- a rough guess, aight?
  space <- handleIO (\_ -> pure Nothing) $ fmap Just $ liftIO $ getAvailSpace tmpdir
  when (maybe False (toBytes minSpace >) space) $ do
    logWarn ("Possibly insufficient disk space on "
      <> T.pack tmpdir
      <> ". At least "
      <> T.pack (show minSpace)
      <> " MB are recommended, but only "
      <> toMB (fromJust space)
      <> " are free. Consider freeing up disk space or setting TMPDIR env variable.")
    logWarn
      "...waiting for 10 seconds before continuing anyway, you can still abort..."
    liftIO $ threadDelay 10000000 -- give the user a sec to intervene

  liftIO $ createTempDirectory tmpdir "ghcup"
 where
  toBytes mb = mb * 1024 * 1024
  toMB b = T.pack $ show (truncate' (fromIntegral b / (1024 * 1024) :: Double) 2)
  truncate' :: Double -> Int -> Double
  truncate' x n = fromIntegral (floor (x * t) :: Integer) / t
      where t = 10^n


withGHCupTmpDir :: ( MonadReader env m
                   , HasDirs env
                   , HasLog env
                   , HasSettings env
                   , MonadUnliftIO m
                   , MonadCatch m
                   , MonadResource m
                   , MonadThrow m
                   , MonadMask m
                   , MonadIO m)
                => m FilePath
withGHCupTmpDir = snd <$> withRunInIO (\run ->
  run
    $ allocate
        (run mkGhcupTmpDir)
        (\fp ->
            handleIO (\e -> run
                $ logDebug ("Resource cleanup failed for " <> T.pack fp <> ", error was: " <> T.pack (displayException e)))
            . rmPathForcibly
            $ fp))




    --------------
    --[ Others ]--
    --------------


useXDG :: IO Bool
useXDG = isJust <$> lookupEnv "GHCUP_USE_XDG_DIRS"


relativeSymlink :: FilePath  -- ^ the path in which to create the symlink
                -> FilePath  -- ^ the symlink destination
                -> FilePath
relativeSymlink p1 p2 =
  let d1      = splitDirectories p1
      d2      = splitDirectories p2
      common  = takeWhile (\(x, y) -> x == y) $ zip d1 d2
      cPrefix = drop (length common) d1
  in  joinPath (replicate (length cPrefix) "..")
        <> joinPath ([pathSeparator] : drop (length common) d2)


cleanupTrash :: ( MonadIO m
                , MonadMask m
                , MonadReader env m
                , HasLog env
                , HasDirs env
                , HasSettings env
                )
             => m ()
cleanupTrash = do
  Dirs { recycleDir } <- getDirs
  contents <- liftIO $ listDirectory recycleDir
  if null contents
  then pure ()
  else do
    logWarn ("Removing leftover files in " <> T.pack recycleDir)
    forM_ contents (\fp -> handleIO (\e ->
        logDebug ("Resource cleanup failed for " <> T.pack fp <> ", error was: " <> T.pack (displayException e))
      ) $ liftIO $ removePathForcibly (recycleDir </> fp))

