{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE QuasiQuotes           #-}

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

  , GHCupPath
  , appendGHCupPath
  , fromGHCupPath
  , createTempGHCupDirectory
  , getGHCupTmpDirs

  , removeDirectory
  , removeDirectoryRecursive
  , removePathForcibly

  -- System.Directory re-exports
  , createDirectory
  , createDirectoryIfMissing
  , renameDirectory
  , listDirectory
  , getDirectoryContents
  , getCurrentDirectory
  , setCurrentDirectory
  , withCurrentDirectory
  , getHomeDirectory
  , XdgDirectory(..)
  , getXdgDirectory
  , XdgDirectoryList(..)
  , getXdgDirectoryList
  , getAppUserDataDirectory
  , getUserDocumentsDirectory
  , getTemporaryDirectory
  , removeFile
  , renameFile
  , renamePath
  , getFileSize
  , canonicalizePath
  , makeAbsolute
  , makeRelativeToCurrentDirectory
  , doesPathExist
  , doesFileExist
  , doesDirectoryExist
  , findExecutable
  , findExecutables
  , findExecutablesInDirectories
  , findFile
  , findFileWith
  , findFilesWith
  , exeExtension
  , createFileLink
  , createDirectoryLink
  , removeDirectoryLink
  , pathIsSymbolicLink
  , getSymbolicLinkTarget
  , Permissions
  , emptyPermissions
  , readable
  , writable
  , executable
  , searchable
  , setOwnerReadable
  , setOwnerWritable
  , setOwnerExecutable
  , setOwnerSearchable
  , getPermissions
  , setPermissions
  , copyPermissions
  , getAccessTime
  , getModificationTime
  , setAccessTime
  , setModificationTime
  , isSymbolicLink
  )
where


import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils.MegaParsec
import           GHCup.Utils.Logger
import           GHCup.Utils.Prelude
import           GHCup.Utils.File.Common
import           GHCup.Utils.String.QQ

import           Control.DeepSeq (NFData, rnf)
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource hiding (throwM)
import           Data.List
import           Data.ByteString                ( ByteString )
import           Data.Bifunctor
import           Data.Maybe
import           Data.Versions
import           GHC.IO.Exception               ( IOErrorType(NoSuchThing) )
import           Haskus.Utils.Variant.Excepts
import           Optics
import           System.Directory hiding ( removeDirectory
                                         , removeDirectoryRecursive
                                         , removePathForcibly
                                         , findFiles
                                         )
import qualified System.Directory              as SD

import           System.DiskSpace
import           System.Environment
import           System.FilePath
import           System.IO.Temp
import           Text.Regex.Posix

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Yaml.Aeson               as Y
import qualified Text.Megaparsec               as MP
import Control.Concurrent (threadDelay)



    ---------------------------
    --[ GHCupPath utilities ]--
    ---------------------------

-- | A 'GHCupPath' is a safe sub-path that can be recursively deleted.
--
-- The constructor is not exported.
newtype GHCupPath = GHCupPath FilePath
  deriving (Show, Eq, Ord)

instance NFData GHCupPath where
  rnf (GHCupPath fp) = rnf fp

appendGHCupPath :: GHCupPath -> FilePath -> GHCupPath
appendGHCupPath (GHCupPath gp) fp = GHCupPath (gp </> fp)

fromGHCupPath :: GHCupPath -> FilePath
fromGHCupPath (GHCupPath gp) = gp

createTempGHCupDirectory :: GHCupPath -> FilePath -> IO GHCupPath
createTempGHCupDirectory (GHCupPath gp) d = GHCupPath <$> createTempDirectory gp d


getGHCupTmpDirs :: IO [GHCupPath]
getGHCupTmpDirs = do
  tmpdir <- getCanonicalTemporaryDirectory
  ghcup_dirs <- handleIO (\_ -> pure []) $ findFiles
    tmpdir
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^ghcup-.*$|] :: ByteString)
    )
  pure (fmap (\p -> GHCupPath (tmpdir </> p)) $ filter (("ghcup-" `isPrefixOf`)  . takeDirectory) $ ghcup_dirs)


    ------------------------------
    --[ GHCup base directories ]--
    ------------------------------


-- | ~/.ghcup by default
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_DATA_HOME/ghcup' as per xdg spec.
ghcupBaseDir :: IO GHCupPath
ghcupBaseDir
  | isWindows = do
      bdir <- fromMaybe "C:\\" <$> lookupEnv "GHCUP_INSTALL_BASE_PREFIX"
      pure (GHCupPath (bdir </> "ghcup"))
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          bdir <- lookupEnv "XDG_DATA_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".local" </> "share")
          pure (GHCupPath (bdir </> "ghcup"))
        else do
          bdir <- lookupEnv "GHCUP_INSTALL_BASE_PREFIX" >>= \case
            Just r  -> pure r
            Nothing -> liftIO getHomeDirectory
          pure (GHCupPath (bdir </> ".ghcup"))


-- | ~/.ghcup by default
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CONFIG_HOME/ghcup' as per xdg spec.
ghcupConfigDir :: IO GHCupPath
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
          pure (GHCupPath (bdir </> "ghcup"))
        else do
          bdir <- lookupEnv "GHCUP_INSTALL_BASE_PREFIX" >>= \case
            Just r  -> pure r
            Nothing -> liftIO getHomeDirectory
          pure (GHCupPath (bdir </> ".ghcup"))


-- | If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_BIN_HOME' env var or defaults to '~/.local/bin'
-- (which, sadly is not strictly xdg spec).
ghcupBinDir :: IO FilePath
ghcupBinDir
  | isWindows = (fromGHCupPath <$> ghcupBaseDir) <&> (</> "bin")
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          lookupEnv "XDG_BIN_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".local" </> "bin")
        else (fromGHCupPath <$> ghcupBaseDir) <&> (</> "bin")


-- | Defaults to '~/.ghcup/cache'.
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CACHE_HOME/ghcup' as per xdg spec.
ghcupCacheDir :: IO GHCupPath
ghcupCacheDir
  | isWindows = ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "cache"))
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          bdir <- lookupEnv "XDG_CACHE_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".cache")
          pure (GHCupPath (bdir </> "ghcup"))
        else ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "cache"))


-- | Defaults to '~/.ghcup/logs'.
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CACHE_HOME/ghcup/logs' as per xdg spec.
ghcupLogsDir :: IO GHCupPath
ghcupLogsDir
  | isWindows = ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "logs"))
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          bdir <- lookupEnv "XDG_CACHE_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".cache")
          pure (GHCupPath (bdir </> "ghcup" </> "logs"))
        else ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "logs"))


-- | Defaults to '~/.ghcup/db.
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CACHE_HOME/ghcup/db as per xdg spec.
ghcupDbDir :: IO GHCupPath
ghcupDbDir
  | isWindows = ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "db"))
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          bdir <- lookupEnv "XDG_CACHE_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".cache")
          pure (GHCupPath (bdir </> "ghcup" </> "db"))
        else ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "db"))


-- | '~/.ghcup/trash'.
-- Mainly used on windows to improve file removal operations
ghcupRecycleDir :: IO GHCupPath
ghcupRecycleDir = ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "trash"))



getAllDirs :: IO Dirs
getAllDirs = do
  baseDir    <- ghcupBaseDir
  binDir     <- ghcupBinDir
  cacheDir   <- ghcupCacheDir
  logsDir    <- ghcupLogsDir
  confDir    <- ghcupConfigDir
  recycleDir <- ghcupRecycleDir
  dbDir      <- ghcupDbDir
  pure Dirs { .. }



    -------------------
    --[ GHCup files ]--
    -------------------

getConfigFilePath :: (MonadIO m) => m FilePath
getConfigFilePath = do
  confDir <- liftIO ghcupConfigDir
  pure $ fromGHCupPath confDir </> "config.yaml"

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
ghcupGHCBaseDir :: (MonadReader env m, HasDirs env) => m GHCupPath
ghcupGHCBaseDir = do
  Dirs {..}  <- getDirs
  pure (baseDir `appendGHCupPath` "ghc")


-- | Gets '~/.ghcup/ghc/<ghcupGHCDir>'.
-- The dir may be of the form
--   * armv7-unknown-linux-gnueabihf-8.8.3
--   * 8.8.4
ghcupGHCDir :: (MonadReader env m, HasDirs env, MonadThrow m)
            => GHCTargetVersion
            -> m GHCupPath
ghcupGHCDir ver = do
  ghcbasedir <- ghcupGHCBaseDir
  let verdir = T.unpack $ tVerToText ver
  pure (ghcbasedir `appendGHCupPath` verdir)


-- | See 'ghcupToolParser'.
parseGHCupGHCDir :: MonadThrow m => FilePath -> m GHCTargetVersion
parseGHCupGHCDir (T.pack -> fp) =
  throwEither $ MP.parse ghcTargetVerP "" fp

parseGHCupHLSDir :: MonadThrow m => FilePath -> m Version
parseGHCupHLSDir (T.pack -> fp) =
  throwEither $ MP.parse version' "" fp

-- | ~/.ghcup/hls by default, for new-style installs.
ghcupHLSBaseDir :: (MonadReader env m, HasDirs env) => m GHCupPath
ghcupHLSBaseDir = do
  Dirs {..}  <- getDirs
  pure (baseDir `appendGHCupPath` "hls")

-- | Gets '~/.ghcup/hls/<hls-ver>' for new-style installs.
ghcupHLSDir :: (MonadReader env m, HasDirs env, MonadThrow m)
            => Version
            -> m GHCupPath
ghcupHLSDir ver = do
  basedir <- ghcupHLSBaseDir
  let verdir = T.unpack $ prettyVer ver
  pure (basedir `appendGHCupPath` verdir)

mkGhcupTmpDir :: ( MonadReader env m
                 , HasDirs env
                 , MonadUnliftIO m
                 , HasLog env
                 , MonadCatch m
                 , MonadThrow m
                 , MonadMask m
                 , MonadIO m)
              => m GHCupPath
mkGhcupTmpDir = GHCupPath <$> do
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
                => m GHCupPath
withGHCupTmpDir = snd <$> withRunInIO (\run ->
  run
    $ allocate
        (run mkGhcupTmpDir)
        (\fp ->
            handleIO (\e -> run
                $ logDebug ("Resource cleanup failed for " <> T.pack (fromGHCupPath fp) <> ", error was: " <> T.pack (displayException e)))
            . rmPathForcibly
            $ fp))




    --------------
    --[ Others ]--
    --------------


useXDG :: IO Bool
useXDG = isJust <$> lookupEnv "GHCUP_USE_XDG_DIRS"


-- | Like 'relpath'. Assumes the inputs are resolved in case of symlinks.
relativeSymlink :: FilePath  -- ^ the path in which to create the symlink
                -> FilePath  -- ^ the symlink destination
                -> FilePath
relativeSymlink p1 p2
  | isWindows = p2 -- windows quickly gets into MAX_PATH issues so we don't care about relative symlinks
  | otherwise =
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
  contents <- liftIO $ listDirectory (fromGHCupPath recycleDir)
  if null contents
  then pure ()
  else do
    logWarn ("Removing leftover files in " <> T.pack (fromGHCupPath recycleDir))
    forM_ contents (\fp -> handleIO (\e ->
        logDebug ("Resource cleanup failed for " <> T.pack fp <> ", error was: " <> T.pack (displayException e))
      ) $ liftIO $ removePathForcibly (recycleDir `appendGHCupPath` fp))



-- System.Directory re-exports with GHCupPath

removeDirectory :: GHCupPath -> IO ()
removeDirectory (GHCupPath fp) = SD.removeDirectory fp

removeDirectoryRecursive :: GHCupPath -> IO ()
removeDirectoryRecursive (GHCupPath fp) = SD.removeDirectoryRecursive fp

removePathForcibly :: GHCupPath -> IO ()
removePathForcibly (GHCupPath fp) = SD.removePathForcibly fp



