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
Portability : POSIX
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

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource hiding (throwM)
import           Data.Bifunctor
import           Data.ByteString                ( ByteString )
import           Data.Maybe
import           Data.String.Interpolate
import           GHC.IO.Exception               ( IOErrorType(NoSuchThing) )
import           Haskus.Utils.Variant.Excepts
import           HPath
import           HPath.IO
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.DiskSpace                                                
import           System.Posix.Env.ByteString    ( getEnv
                                                , getEnvDefault
                                                )
import           System.Posix.FilePath   hiding ( (</>) )
import           System.Posix.Temp.ByteString   ( mkdtemp )

import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Yaml                     as Y
import qualified System.Posix.FilePath         as FP
import qualified System.Posix.User             as PU
import qualified Text.Megaparsec               as MP
import Control.Concurrent (threadDelay)



    ------------------------------
    --[ GHCup base directories ]--
    ------------------------------


-- | ~/.ghcup by default
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_DATA_HOME/ghcup' as per xdg spec.
ghcupBaseDir :: IO (Path Abs)
ghcupBaseDir = do
  xdg <- useXDG
  if xdg
    then do
      bdir <- getEnv "XDG_DATA_HOME" >>= \case
        Just r  -> parseAbs r
        Nothing -> do
          home <- liftIO getHomeDirectory
          pure (home </> [rel|.local/share|])
      pure (bdir </> [rel|ghcup|])
    else do
      bdir <- getEnv "GHCUP_INSTALL_BASE_PREFIX" >>= \case
        Just r  -> parseAbs r
        Nothing -> liftIO getHomeDirectory
      pure (bdir </> [rel|.ghcup|])


-- | ~/.ghcup by default
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CONFIG_HOME/ghcup' as per xdg spec.
ghcupConfigDir :: IO (Path Abs)
ghcupConfigDir = do
  xdg <- useXDG
  if xdg
    then do
      bdir <- getEnv "XDG_CONFIG_HOME" >>= \case
        Just r  -> parseAbs r
        Nothing -> do
          home <- liftIO getHomeDirectory
          pure (home </> [rel|.config|])
      pure (bdir </> [rel|ghcup|])
    else do
      bdir <- getEnv "GHCUP_INSTALL_BASE_PREFIX" >>= \case
        Just r  -> parseAbs r
        Nothing -> liftIO getHomeDirectory
      pure (bdir </> [rel|.ghcup|])


-- | If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_BIN_HOME' env var or defaults to '~/.local/bin'
-- (which, sadly is not strictly xdg spec).
ghcupBinDir :: IO (Path Abs)
ghcupBinDir = do
  xdg <- useXDG
  if xdg
    then do
      getEnv "XDG_BIN_HOME" >>= \case
        Just r  -> parseAbs r
        Nothing -> do
          home <- liftIO getHomeDirectory
          pure (home </> [rel|.local/bin|])
    else ghcupBaseDir <&> (</> [rel|bin|])


-- | Defaults to '~/.ghcup/cache'.
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CACHE_HOME/ghcup' as per xdg spec.
ghcupCacheDir :: IO (Path Abs)
ghcupCacheDir = do
  xdg <- useXDG
  if xdg
    then do
      bdir <- getEnv "XDG_CACHE_HOME" >>= \case
        Just r  -> parseAbs r
        Nothing -> do
          home <- liftIO getHomeDirectory
          pure (home </> [rel|.cache|])
      pure (bdir </> [rel|ghcup|])
    else ghcupBaseDir <&> (</> [rel|cache|])


-- | Defaults to '~/.ghcup/logs'.
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CACHE_HOME/ghcup/logs' as per xdg spec.
ghcupLogsDir :: IO (Path Abs)
ghcupLogsDir = do
  xdg <- useXDG
  if xdg
    then do
      bdir <- getEnv "XDG_CACHE_HOME" >>= \case
        Just r  -> parseAbs r
        Nothing -> do
          home <- liftIO getHomeDirectory
          pure (home </> [rel|.cache|])
      pure (bdir </> [rel|ghcup/logs|])
    else ghcupBaseDir <&> (</> [rel|logs|])


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
  let file = confDir </> [rel|config.yaml|]
  bs <- liftIO $ handleIO' NoSuchThing (\_ -> pure Nothing) $ Just <$> readFile file
  case bs of
      Nothing -> pure defaultUserSettings
      Just bs' -> lE' JSONDecodeError . first show . Y.decodeEither' . L.toStrict $ bs'


    -------------------------
    --[ GHCup directories ]--
    -------------------------


-- | ~/.ghcup/ghc by default.
ghcupGHCBaseDir :: (MonadReader AppState m) => m (Path Abs)
ghcupGHCBaseDir = do
  AppState { dirs = Dirs {..} } <- ask
  pure (baseDir </> [rel|ghc|])


-- | Gets '~/.ghcup/ghc/<ghcupGHCDir>'.
-- The dir may be of the form
--   * armv7-unknown-linux-gnueabihf-8.8.3
--   * 8.8.4
ghcupGHCDir :: (MonadReader AppState m, MonadThrow m)
            => GHCTargetVersion
            -> m (Path Abs)
ghcupGHCDir ver = do
  ghcbasedir    <- ghcupGHCBaseDir
  verdir        <- parseRel $ E.encodeUtf8 (tVerToText ver)
  pure (ghcbasedir </> verdir)


-- | See 'ghcupToolParser'.
parseGHCupGHCDir :: MonadThrow m => Path Rel -> m GHCTargetVersion
parseGHCupGHCDir (toFilePath -> f) = do
  fp <- throwEither $ E.decodeUtf8' f
  throwEither $ MP.parse ghcTargetVerP "" fp


mkGhcupTmpDir :: (MonadUnliftIO m, MonadLogger m, MonadCatch m, MonadThrow m, MonadIO m) => m (Path Abs)
mkGhcupTmpDir = do
  tmpdir <- liftIO $ getEnvDefault "TMPDIR" "/tmp"
  let fp = T.unpack $ decUTF8Safe tmpdir

  let minSpace = 5000 -- a rough guess, aight?
  space <- handleIO (\_ -> pure Nothing) $ fmap Just $ liftIO $ getAvailSpace fp
  when (maybe False (toBytes minSpace >) space) $ do
    $(logWarn) [i|Possibly insufficient disk space on #{fp}. At least #{minSpace} MB are recommended, but only #{toMB (fromJust space)} are free. Consider freeing up disk space or setting TMPDIR env variable.|]
    $(logWarn)
      "...waiting for 10 seconds before continuing anyway, you can still abort..."
    liftIO $ threadDelay 10000000 -- give the user a sec to intervene

  tmp <- liftIO $ mkdtemp (tmpdir FP.</> "ghcup-")
  parseAbs tmp
 where
  toBytes mb = mb * 1024 * 1024
  toMB b = show (truncate' (fromIntegral b / (1024 * 1024) :: Double) 2)
  truncate' :: Double -> Int -> Double
  truncate' x n = fromIntegral (floor (x * t) :: Integer) / t
      where t = 10^n


withGHCupTmpDir :: (MonadUnliftIO m, MonadLogger m, MonadCatch m, MonadResource m, MonadThrow m, MonadIO m) => m (Path Abs)
withGHCupTmpDir = snd <$> withRunInIO (\run -> run $ allocate (run mkGhcupTmpDir) deleteDirRecursive)




    --------------
    --[ Others ]--
    --------------


getHomeDirectory :: IO (Path Abs)
getHomeDirectory = do
  e <- getEnv "HOME"
  case e of
    Just fp -> parseAbs fp
    Nothing -> do
      h <- PU.homeDirectory <$> (PU.getEffectiveUserID >>= PU.getUserEntryForID)
      parseAbs $ UTF8.fromString h -- this is a guess


useXDG :: IO Bool
useXDG = isJust <$> getEnv "GHCUP_USE_XDG_DIRS"


relativeSymlink :: Path Abs  -- ^ the path in which to create the symlink
                -> Path Abs  -- ^ the symlink destination
                -> ByteString
relativeSymlink (toFilePath -> p1) (toFilePath -> p2) =
  let d1      = splitDirectories p1
      d2      = splitDirectories p2
      common  = takeWhile (\(x, y) -> x == y) $ zip d1 d2
      cPrefix = drop (length common) d1
  in  joinPath (replicate (length cPrefix) "..")
        <> joinPath ("/" : drop (length common) d2)


