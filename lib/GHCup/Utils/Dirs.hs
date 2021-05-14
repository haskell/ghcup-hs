{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TemplateHaskell       #-}

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
  ( getDirs
  , ghcupConfigFile
  , ghcupCacheDir
  , ghcupGHCBaseDir
  , ghcupGHCDir
  , mkGhcupTmpDir
  , parseGHCupGHCDir
  , relativeSymlink
  , withGHCupTmpDir
  )
where


import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.MegaParsec
import           GHCup.Utils.Prelude

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource hiding (throwM)
import           Data.Bifunctor
import           Data.Maybe
import           Data.String.Interpolate
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
import qualified Data.Yaml                     as Y
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
ghcupBaseDir = do
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
ghcupConfigDir = do
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
ghcupBinDir = do
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
ghcupCacheDir = do
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
ghcupLogsDir = do
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


getDirs :: IO Dirs
getDirs = do
  baseDir  <- ghcupBaseDir
  binDir   <- ghcupBinDir
  cacheDir <- ghcupCacheDir
  logsDir  <- ghcupLogsDir
  confDir  <- ghcupConfigDir
  pure Dirs { .. }



    -------------------
    --[ GHCup files ]--
    -------------------


ghcupConfigFile :: (MonadIO m)
                => Excepts '[JSONError] m UserSettings
ghcupConfigFile = do
  confDir <- liftIO ghcupConfigDir
  let file = confDir </> "config.yaml"
  contents <- liftIO $ handleIO' NoSuchThing (\_ -> pure Nothing) $ Just <$> BS.readFile file
  case contents of
      Nothing -> pure defaultUserSettings
      Just contents' -> lE' JSONDecodeError . first show . Y.decodeEither' $ contents'


    -------------------------
    --[ GHCup directories ]--
    -------------------------


-- | ~/.ghcup/ghc by default.
ghcupGHCBaseDir :: (MonadReader AppState m) => m FilePath
ghcupGHCBaseDir = do
  AppState { dirs = Dirs {..} } <- ask
  pure (baseDir </> "ghc")


-- | Gets '~/.ghcup/ghc/<ghcupGHCDir>'.
-- The dir may be of the form
--   * armv7-unknown-linux-gnueabihf-8.8.3
--   * 8.8.4
ghcupGHCDir :: (MonadReader AppState m, MonadThrow m)
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


mkGhcupTmpDir :: (MonadUnliftIO m, MonadLogger m, MonadCatch m, MonadThrow m, MonadIO m) => m FilePath
mkGhcupTmpDir = do
  tmpdir <- liftIO getCanonicalTemporaryDirectory

  let minSpace = 5000 -- a rough guess, aight?
  space <- handleIO (\_ -> pure Nothing) $ fmap Just $ liftIO $ getAvailSpace tmpdir
  when (maybe False (toBytes minSpace >) space) $ do
    $(logWarn) [i|Possibly insufficient disk space on #{tmpdir}. At least #{minSpace} MB are recommended, but only #{toMB (fromJust space)} are free. Consider freeing up disk space or setting TMPDIR env variable.|]
    $(logWarn)
      "...waiting for 10 seconds before continuing anyway, you can still abort..."
    liftIO $ threadDelay 10000000 -- give the user a sec to intervene

  liftIO $ createTempDirectory tmpdir "ghcup"
 where
  toBytes mb = mb * 1024 * 1024
  toMB b = show (truncate' (fromIntegral b / (1024 * 1024) :: Double) 2)
  truncate' :: Double -> Int -> Double
  truncate' x n = fromIntegral (floor (x * t) :: Integer) / t
      where t = 10^n


withGHCupTmpDir :: (MonadUnliftIO m, MonadLogger m, MonadCatch m, MonadResource m, MonadThrow m, MonadIO m) => m FilePath
withGHCupTmpDir = snd <$> withRunInIO (\run -> run $ allocate (run mkGhcupTmpDir) removeDirectoryRecursive)




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


