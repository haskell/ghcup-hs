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
  , getConfigFilePath'
  , useXDG
  , cleanupTrash
  , ghcupMsys2BinDirs
  , ghcupMsys2BinDirs'

  , GHCupPath
  , appendGHCupPath
  , fromGHCupPath
  , createTempGHCupDirectory
  , getGHCupTmpDirs

  , removeDirectory
  , removeDirectoryRecursive
  , removePathForcibly

  , listDirectoryFiles
  , listDirectoryDirs

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
import           GHCup.Prelude.MegaParsec
import           GHCup.Prelude.File.Search
import           GHCup.Prelude.String.QQ
import           GHCup.Prelude.Logger.Internal (logWarn, logDebug)
#if defined(IS_WINDOWS)
import           GHCup.Prelude.Windows ( isWindows )
#else
import           GHCup.Prelude.Posix   ( isWindows )
#endif

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
import           Optics hiding ( uncons )
import           Safe
import           System.Info
import           System.Directory hiding ( removeDirectory
                                         , removeDirectoryRecursive
                                         , removePathForcibly
                                         , findFiles
                                         , makeAbsolute
                                         )
import qualified System.Directory              as SD

import           System.Environment
import           System.FilePath
import           System.IO.Temp
import           Text.Regex.Posix

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Yaml.Aeson               as Y
import qualified Text.Megaparsec               as MP
import System.IO.Error (ioeGetErrorType)



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
  tmpdir <- fromGHCupPath <$> ghcupTMPDir
  ghcup_dirs <- handleIO (\_ -> pure []) $ findFiles
    tmpdir
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^ghcup-.*$|] :: ByteString)
    )
  pure (fmap (\p -> GHCupPath (tmpdir </> p)) $ filter (maybe False ("ghcup-" `isPrefixOf`) . lastMay . splitPath) ghcup_dirs)


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
          pure (GHCupPath (bdir </> "ghcup" </> "cache"))
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
ghcupDbDir = ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "db"))


-- | '~/.ghcup/trash'.
-- Mainly used on windows to improve file removal operations
ghcupRecycleDir :: IO GHCupPath
ghcupRecycleDir = ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "trash"))


-- | Defaults to '~/.ghcup/tmp.
--
-- If 'GHCUP_USE_XDG_DIRS' is set (to anything),
-- then uses 'XDG_CACHE_HOME/ghcup/tmp as per xdg spec.
ghcupTMPDir :: IO GHCupPath
ghcupTMPDir
  | isWindows = ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "tmp"))
  | otherwise = do
      xdg <- useXDG
      if xdg
        then do
          bdir <- lookupEnv "XDG_CACHE_HOME" >>= \case
            Just r  -> pure r
            Nothing -> do
              home <- liftIO getHomeDirectory
              pure (home </> ".cache")
          pure (GHCupPath (bdir </> "ghcup" </> "tmp"))
        else ghcupBaseDir <&> (\(GHCupPath gp) -> GHCupPath (gp </> "tmp"))


ghcupMsys2Dir :: IO FilePath
ghcupMsys2Dir =
  lookupEnv "GHCUP_MSYS2" >>= \case
    Just fp -> pure fp
    Nothing -> do
      baseDir <- liftIO ghcupBaseDir
      pure (fromGHCupPath baseDir </> "msys64")

ghcupMsys2BinDirs :: (MonadFail m, MonadIO m, MonadReader env m, HasDirs env) => m [FilePath]
ghcupMsys2BinDirs = do
  Dirs{..} <- getDirs
  liftIO $ ghcupMsys2BinDirs_ msys2Dir

ghcupMsys2BinDirs' :: IO [FilePath]
ghcupMsys2BinDirs' = do
  msys2Dir <- ghcupMsys2Dir
  ghcupMsys2BinDirs_ msys2Dir

ghcupMsys2BinDirs_ :: FilePath -> IO [FilePath]
ghcupMsys2BinDirs_ msys2Dir' = do
  env <- liftIO (lookupEnv "GHCUP_MSYS2_ENV") >>= \case
    Just env -> maybe (fail parseFailMsg) pure $ readMay @MSYS2Env env
    Nothing
      | "x86_64"  <- arch -> pure MINGW64
      | "i386"    <- arch -> pure MINGW32
      | "aarch64" <- arch -> pure CLANGARM64
      | otherwise -> fail "No compatible architecture for msys2"
  pure [msys2Dir' </> toEnvDir env </> "bin", msys2Dir' </> toEnvDir MSYS </> "bin"]
 where
  -- https://www.msys2.org/docs/environments/
  toEnvDir :: MSYS2Env -> FilePath
  toEnvDir MSYS       = "usr"
  toEnvDir UCRT64     = "ucrt64"
  toEnvDir CLANG64    = "clang64"
  toEnvDir CLANGARM64 = "clangarm64"
  toEnvDir CLANG32    = "clang32"
  toEnvDir MINGW64    = "mingw64"
  toEnvDir MINGW32    = "mingw32"

  parseFailMsg = "Invalid value for GHCUP_MSYS2_ENV. Valid values are: MSYS, UCRT64, CLANG64, CLANGARM64, CLANG32, MINGW64, MINGW32"


getAllDirs :: IO Dirs
getAllDirs = do
  baseDir    <- ghcupBaseDir
  binDir     <- ghcupBinDir
  cacheDir   <- ghcupCacheDir
  logsDir    <- ghcupLogsDir
  confDir    <- ghcupConfigDir
  recycleDir <- ghcupRecycleDir
  tmpDir     <- ghcupTMPDir
  dbDir      <- ghcupDbDir
  msys2Dir   <- ghcupMsys2Dir
  pure Dirs { .. }



    -------------------
    --[ GHCup files ]--
    -------------------

getConfigFilePath :: (MonadIO m) => m FilePath
getConfigFilePath = do
  confDir <- liftIO ghcupConfigDir
  pure $ fromGHCupPath confDir </> "config.yaml"

getConfigFilePath' :: (MonadReader env m, HasDirs env) => m FilePath
getConfigFilePath' = do
  Dirs {..} <- getDirs
  pure $ fromGHCupPath confDir </> "config.yaml"


ghcupConfigFile :: (MonadIO m)
                => Excepts '[JSONError] m UserSettings
ghcupConfigFile = do
  filepath <- getConfigFilePath
  contents <- liftIO $ handleIO (\e -> if NoSuchThing == ioeGetErrorType e then pure Nothing else liftIO $ ioError e) $ Just <$> BS.readFile filepath
  case contents of
      Nothing -> pure defaultUserSettings
      Just contents' -> liftE
        . veitherToExcepts @_ @'[JSONError]
        . either (VLeft . V) VRight
        . first (JSONDecodeError . displayException)
        . Y.decodeEither'
        $ contents'


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

-- TODO: inlined from GHCup.Prelude
throwEither :: (Exception a, MonadThrow m) => Either a b -> m b
throwEither a = case a of
  Left  e -> throwM e
  Right r -> pure r

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
  Dirs { tmpDir } <- getDirs
  liftIO $ createTempDirectory (fromGHCupPath tmpDir) "ghcup"


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
withGHCupTmpDir = do
  Settings{keepDirs} <- getSettings
  snd <$> withRunInIO (\run ->
    run
      $ allocate
          (run mkGhcupTmpDir)
          (\fp -> if -- we don't know whether there was a failure, so can only
                     -- decide for 'Always'
                     | keepDirs == Always -> pure ()
                     | otherwise -> handleIO (\e -> run
                        $ logDebug ("Resource cleanup failed for "
                                   <> T.pack (fromGHCupPath fp)
                                   <> ", error was: "
                                   <> T.pack (displayException e)))
                        . removePathForcibly
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


-- | List *actual files* in a directory, ignoring empty files and a couple
-- of blacklisted files, such as '.DS_Store' on mac.
listDirectoryFiles :: FilePath -> IO [FilePath]
listDirectoryFiles fp = do
  listDirectory fp >>= filterM (doesFileExist . (fp </>)) <&> filter (\fp' -> not (isHidden fp') && not (isBlacklisted fp'))

-- | List *actual directories* in a directory, ignoring empty directories and a couple
-- of blacklisted files, such as '.DS_Store' on mac.
listDirectoryDirs :: FilePath -> IO [FilePath]
listDirectoryDirs fp = do
  listDirectory fp >>= filterM (doesDirectoryExist . (fp </>)) <&> filter (\fp' -> not (isHidden fp') && not (isBlacklisted fp'))

isHidden :: FilePath -> Bool
isHidden fp'
  | isWindows = False
  | Just ('.', _) <- uncons fp' = True
  | otherwise = False

isBlacklisted :: FilePath -> Bool
{- HLINT ignore "Use ==" -}
isBlacklisted fp' = fp' `elem` [".DS_Store"]



-- System.Directory re-exports with GHCupPath

removeDirectory :: GHCupPath -> IO ()
removeDirectory (GHCupPath fp) = SD.removeDirectory fp

removeDirectoryRecursive :: GHCupPath -> IO ()
removeDirectoryRecursive (GHCupPath fp) = SD.removeDirectoryRecursive fp

removePathForcibly :: GHCupPath -> IO ()
removePathForcibly (GHCupPath fp) = SD.removePathForcibly fp




