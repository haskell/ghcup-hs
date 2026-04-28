{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Command.Rm where

import GHCup.Errors
import GHCup.Input.SymlinkSpec
import GHCup.Legacy.Cabal
import GHCup.Legacy.HLS
import GHCup.Legacy.Stack
import GHCup.Legacy.Utils
import GHCup.Prelude
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.Query.Symlink
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
import Control.Monad.Trans.Resource   hiding ( throwM )
import Data.List
import Data.Maybe
import Data.Variant.Excepts
import GHC.IO.Exception
import Prelude                        hiding ( abs, writeFile )
import System.Environment
import System.FilePath
import System.IO.Error
import System.IO.Temp
import Text.PrettyPrint.HughesPJClass ( prettyShow )

import qualified Data.Text as T


-- | Remove a tool completely.
--
-- This has two phases:
--
-- 1. the files installed from the bindist
-- 2. the files symlinked into the GHCup bin directory
--
-- Both are stored on the DB, but in different files/formats.
rmToolVersion ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , HasPlatformReq env
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
  -> Excepts '[NotInstalled, UninstallFailed, ParseError, MalformedInstallInfo] m ()
rmToolVersion tool tver = do
  logInfo $ "Removing " <> prettyText tool <> " version " <> prettyText tver
  let target = _tvTarget tver
      ver' = _tvVersion tver

  unlessM (lift $ isInstalled tool tver) $ throwE (NotInstalled tool tver)

  mset  <- liftE $ getSetVersion' tool target

  vspec <- lift $ runE $ getSymlinkSpec' tool tver

  case vspec of
    VLeft _ -> do
      -- legacy, no spec files
      pure ()
    VRight specRaw -> do
      let spec = substituteSpec <$> specRaw
      lift $ logInfo "Removing executable symlinks"

      toolDir <- fromGHCupPath <$> lift (toolInstallDestination tool tver)

      case mset of
        Just (setVersion, mSetFile) -> do
          lift $ logDebug $ "Set version: " <> T.pack (prettyShow setVersion)
          when (setVersion == ver') $ lift $ do
              unqualSyml <- getUnqualifiedSymlinks spec toolDir
              forM_ unqualSyml (rmLink . snd)
              forM_ mSetFile recycleFile
              rmGHCShareDir
        Nothing -> pure ()

      pvpSyml <- lift $ getPVPSymlinks spec toolDir
      forM_ pvpSyml (lift . rmLink . snd)
      pvpMajorSyml <- liftE $ getPVPMajorSymlinks specRaw ver' toolDir
      forM_ pvpMajorSyml (rmLink . snd)
      f <- recordedInstallationSpecFile tool tver
      hideError NoSuchThing $ recycleFile f

  dir' <- lift $ toolInstallDestination tool tver
  let dir = fromGHCupPath dir'

  lift (getInstalledFiles tool tver) >>= \case
    Just files -> do
      -- @.spec@ file was added after the DB record files, so it's possible
      -- we may need to delete symlinks manually.
      -- We need to do this before the actual removal. The legacy methods
      -- rely on inspecting the internal GHC directories.
      case vspec of
        -- nothing to do, was already removed earlier
        VRight _ -> pure ()
        -- handle legacy symlink removal
        VLeft _
          | tool == ghc -> do
              liftE $ rmMinorGHCSymlinks tver
              liftE $ rmMajorGHCSymlinks tver
              set <- lift $ ghcSet target
              when (Just tver == set) $ do
                liftE $ rmPlainGHC target
                lift rmGHCShareDir

          | tool == hls -> do
              liftE $ rmMinorHLSSymlinks ver'
              set <- lift hlsSet
              when (Just ver' == set) $ liftE rmPlainHLS
          -- cabal and stack don't need special handling
          | otherwise -> pure ()

      lift $ logInfo $ "Removing files safely from: " <> T.pack dir
      forM_ files (lift . hideError NoSuchThing . recycleFile . (\f -> dir </> dropDrive f))
      hideError UnsatisfiedConstraints $ removeEmptyDirsRecursive dir
      survivors <- liftIO $ hideErrorDef [doesNotExistErrorType] [] $ listDirectory dir
      f <- recordedInstallationFile tool tver
      lift $ logDebug $ T.pack (show f)
      lift $ recycleFile f
      when (not (null survivors)) $ throwE $ UninstallFailed dir survivors

    Nothing
      | tool == ghc -> legacyGHCRm
      | tool == cabal -> liftE $ rmCabalVer ver'
      | tool == hls -> liftE $ rmHLSVer ver'
      | tool == stack -> liftE $ rmStackVer ver'
      | tool == ghcup -> lift rmGhcup
      | otherwise -> fail "Could not find installed files... your DB seems corrupted"
 where
  rmGHCShareDir = do
    Dirs {..}  <- getDirs
    hideError doesNotExistErrorType $ rmDirectoryLink (fromGHCupPath baseDir </> "share")
  legacyGHCRm = do
    liftE $ rmMinorGHCSymlinks tver
    liftE $ rmMajorGHCSymlinks tver
    set <- lift $ ghcSet (_tvTarget tver)
    when (Just tver == set) $ do
      liftE $ rmPlainGHC (_tvTarget tver)
      lift rmGHCShareDir
    ghcdir <- ghcupGHCDir tver
    let dir = fromGHCupPath ghcdir
    isDir <- liftIO $ doesDirectoryExist dir
    isSyml <- liftIO $ handleIO (\_ -> pure False) $ pathIsSymbolicLink dir
    when (isDir && not isSyml) $ do
      lift $ logInfo $ "Removing legacy directory recursively: " <> T.pack dir
      recyclePathForcibly ghcdir


-- assuming the current scheme of having just 1 ghcup bin, no version info is required.
rmGhcup :: ( MonadReader env m
           , HasDirs env
           , MonadIO m
           , MonadCatch m
           , HasLog env
           , MonadMask m
           , MonadUnliftIO m
           )
        => m ()
rmGhcup = do
  Dirs { .. } <- getDirs
  let ghcupFilename = "ghcup" <> exeExt
  let ghcupFilepath = binDir </> ghcupFilename

  currentRunningExecPath <- liftIO getExecutablePath

  -- if paths do no exist, warn user, and continue to compare them, as is,
  -- which should eventually fail and result in a non-standard install warning

  p1 <- handleIO' doesNotExistErrorType
                  (handlePathNotPresent currentRunningExecPath)
                  (liftIO $ canonicalizePath currentRunningExecPath)

  p2 <- handleIO' doesNotExistErrorType
                  (handlePathNotPresent ghcupFilepath)
                  (liftIO $ canonicalizePath ghcupFilepath)

  let areEqualPaths = equalFilePath p1 p2

  unless areEqualPaths $ logWarn $ nonStandardInstallLocationMsg currentRunningExecPath

  if isWindows
  then do
    -- since it doesn't seem possible to delete a running exe on windows
    -- we move it to system temp dir, to be deleted at next reboot
    tmp <- liftIO $ getCanonicalTemporaryDirectory >>= \t -> createTempDirectory t "ghcup"
    hideError UnsupportedOperation $
              liftIO $ hideError NoSuchThing $
              moveFile ghcupFilepath (tmp </> "ghcup")
  else
    -- delete it.
    hideError doesNotExistErrorType $ rmFile ghcupFilepath

  where
    handlePathNotPresent fp _err = do
      logDebug $ "Error: The path does not exist, " <> T.pack fp
      pure fp

    nonStandardInstallLocationMsg path = T.pack $
      "current ghcup is invoked from a non-standard location: \n"
      <> path <>
      "\n you may have to uninstall it manually."
