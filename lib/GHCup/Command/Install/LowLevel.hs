{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Command.Install.LowLevel where

import GHCup.Errors
import GHCup.Input.SymlinkSpec
import GHCup.Prelude
import GHCup.Prelude.Process
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.Query.Symlink
import GHCup.System.Cmd
import GHCup.System.Directory
import GHCup.Types
import GHCup.Types.Optics

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import           Control.Monad.Reader
import           Data.Aeson                   ( encodeFile )
import           Data.Maybe
import qualified Data.Text                    as T
import           Data.Variant.Excepts
import           Data.Versions                hiding ( patch )
import           Data.Void
import           Prelude                      hiding ( abs )
import           System.FilePath
import qualified System.FilePath.Posix        as Posix
import           System.FilePattern.Directory
import qualified Text.Megaparsec              as MP





mergeToFileSystem ::
  ( MonadMask m
  , MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadCatch m
  , MonadIOish m
  )
  => Tool
  -> GHCTargetVersion
  -> InstallDirResolved   -- ^ Path to install to
  -> GHCupPath            -- ^ DESTDIR
  -> Bool
  -> Bool
  -> Bool
  -> Excepts '[MergeFileTreeError] m ()
mergeToFileSystem tool tver installDest tmpInstallDest preserveMtimes forceInstall append = do
  mergeFileTree
    (tmpInstallDest `appendGHCupPath` dropDrive (fromInstallDir installDest))
    installDest
    tool
    tver
    (\f t -> if preserveMtimes
             then liftIO $ copyFuncMtime f t
             else liftIO $ install f t (not forceInstall)


    )
    append
 where
  copyFuncMtime f t = do
    mtime <- ifM (pathIsSymbolicLink f) (pure Nothing) (Just <$> getModificationTime f)
    install f t (not forceInstall)
    forM_ mtime $ setModificationTime t

installTheSpec ::
  ( HasLog env
  , HasDirs env
  , HasSettings env
  , MonadReader env m
  , MonadIOish m
  )
  => InstallationSpecInput
  -> FilePath             -- ^ Path to the unpacked bindist
  -> InstallDirResolved   -- ^ Path to install to
  -> GHCupPath            -- ^ DESTDIR
  -> [String]
  -> Maybe [String]
  -> Bool
  -> Excepts '[ CopyError
              , MergeFileTreeError
              , ProcessError
              , ParseError
              , MalformedInstallInfo
              ] m ()
installTheSpec InstallationSpec{..} workdir installDest tmpInstallDest extraArgs installTargets forceInstall = do
  -- run configure first
  case _isConfigure of
    Just ConfigSpec{..} -> do
      addConfArgs <- lift $ sanitizefConfOptions extraArgs

      let configFile = fromMaybe "configure" _csConfigFile
      processedConfArgs <- forM _csConfigArgs $ \a -> either (throwE . ParseError . show) pure . parseDomain $ a
      newEnv <- forM _csConfigEnv $ \EnvSpec{..} -> liftIO (augmentEnvironment _esEnv _esUnion)
      liftE $ execWithWrapper "sh"
                       (("." Posix.</> configFile) : processedConfArgs <> addConfArgs
                       )
                       (Just workdir)
                       "ghc-configure"
                       newEnv
    _ -> pure ()

  -- then make
  case _isMake of
    Just MakeSpec{..} -> do
      processedMakeArgs <- either (throwE . ParseError . show) pure $ forM (fromMaybe _msMakeArgs installTargets) parseDomain
      newEnv <- forM _msMakeEnv $ \EnvSpec{..} -> liftIO (augmentEnvironment _esEnv _esUnion)
      liftE $ makeWithWrapper processedMakeArgs (Just workdir) "make" newEnv
    _ -> pure ()

  -- then copy files manually, if any
  forM_ _isExeRules $ \case
    InstallFileRule{..} -> do
      f <- copy _ibrInstallSource _ibrInstallDest
      lift $ chmod_755 f
    InstallFilePatternRule{..} -> do
      files <- liftIO $ getDirectoryFiles workdir _ibrInstallPattern
      forM_ files $ \file -> do
        f <- copy file Nothing
        lift $ chmod_755 f

  forM_ _isDataRules $ \case
    InstallFileRule{..} ->
      void $ copy _ibrInstallSource _ibrInstallDest
    InstallFilePatternRule{..} -> do
      files <- liftIO $ getDirectoryFiles workdir _ibrInstallPattern
      forM_ files $ \file -> copy file Nothing

 where
  copy source destFile = do
    let from = workdir </> source
        destDir = fromGHCupPath tmpInstallDest </> dropDrive (fromInstallDir installDest)
        into = destDir </> fromMaybe source destFile
    liftIO $ createDirRecursive' (takeDirectory into)
    copyFileE from into (not forceInstall)
    logDebug $ "cp " <> (if forceInstall then "-f " else "") <> T.pack from <> " " <> T.pack into
    pure into

  parseDomain = MP.parse domainParser ""

  domainParser :: MP.Parsec Void String String
  domainParser = concat <$> many anyOrKnownVars
   where
    -- TODO: make this more efficient
    anyOrKnownVars :: MP.Parsec Void String String
    anyOrKnownVars = MP.try (fmap (const (fromInstallDir installDest)) (MP.chunk "${PREFIX}"))
                 <|> MP.try (fmap (const (fromGHCupPath tmpInstallDest)) (MP.chunk "${TMPDIR}"))
                 <|> fmap (:[]) MP.anySingle

makeWithWrapper ::
  ( MonadIOish m
  , HasSettings env
  , HasLog env
  , HasDirs env
  , MonadReader env m
  )
  => [String]
  -> Maybe FilePath
  -> FilePath
  -> Maybe [(String, String)]
  -> Excepts
       '[ ProcessError
        ]
       m
       ()
makeWithWrapper args' mWorkDir logLabel mEnv = do
  mymake <- liftIO getBestMake
  liftE $ execWithWrapper mymake args' mWorkDir logLabel mEnv

execWithWrapper ::
  ( MonadIOish m
  , HasSettings env
  , HasLog env
  , HasDirs env
  , MonadReader env m
  )
  => String
  -> [String]
  -> Maybe FilePath
  -> FilePath
  -> Maybe [(String, String)]
  -> Excepts
       '[ ProcessError
        ]
       m
       ()
execWithWrapper cmd' args' mWorkDir logLabel mEnv = do
  Settings{ buildWrapper } <- lift getSettings
  case buildWrapper of
    Just ProcessSpec{..} ->
      lEM $ execLogged cmd (cmdArgs <> ["--"] <> (cmd':args')) mWorkDir logLabel mEnv
    Nothing ->
      lEM $ execLogged cmd' args' mWorkDir logLabel mEnv

-- | Write InstallationInfo to the disk.
recordInstallationInfo ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => InstallDirResolved
  -> Tool
  -> GHCTargetVersion
  -> DownloadInfo
  -> InstallationSpecResolved
  -> m ()
recordInstallationInfo installDest tool tver dlInfo instSpec
  | isSafeDir installDest = do
      spec <- recordedInstallationSpecFile tool tver
      liftIO $ createDirectoryIfMissing True (takeDirectory spec)
      let metadata = InstallMetadata dlInfo instSpec
      logDebug2 $ "Writing install metadata to " <> T.pack spec <> "\n  " <> T.pack (show metadata)
      liftIO $ encodeFile spec metadata
  | otherwise = do
      logDebug2 $ "Skipping spec installation, because installing into isolated dir: " <> T.pack (show installDest)
      pure ()

sanitizefConfOptions :: MonadFail m => [String] -> m [String]
sanitizefConfOptions args
  | "--prefix" `elem` fmap (takeWhile (/= '=')) args = fail "Don't explicitly set --prefix ...aborting"
  | otherwise = pure args


-- TODO: move to other module
-- | Symlink e.g. @~/.ghcup/ghc/9.6.7/bin/ghc@ to
-- @~/.ghcup/bin/ghc-9.6.7@.
--
-- But we follow the spec from the metadata.
symlinkBinaries ::
  ( HasLog env
  , MonadReader env m
  , HasDirs env   -- createLink needs it on windows
  , MonadIOish m
  )
  => InstallDirResolved                    -- ^ base dir of the tool
  -> [SymlinkSpec [Either Char Version]]   -- ^ symlink spec
  -> InstallDirResolved                    -- ^ binary dir
  -> Tool
  -> GHCTargetVersion
  -> Excepts '[MalformedInstallInfo] m ()
symlinkBinaries (IsolateDirResolved _) _ _ _ _ = pure ()
symlinkBinaries (fromInstallDir -> toolDir) rawSpec bindir tool tver = do
  let spec = substituteSpec <$> rawSpec
  pvpSyms <- lift $ getPVPSymlinks' (fromInstallDir bindir) spec toolDir
  forM_ pvpSyms $ lift . uncurry createLink
  case bindir of
    IsolateDirResolved d -> linkMajor d
    GHCupDir d           -> linkMajor (fromGHCupPath d)
    GHCupBinDir d        -> whenM (lift isLatest) $ linkMajor d
 where
  linkMajor d = do
    let ver = _tvVersion tver
    pvpMajorSyms <- liftE $ getPVPMajorSymlinks' d rawSpec ver toolDir
    forM_ pvpMajorSyms $ lift . uncurry createLink
  isLatest = do
    vers <- getInstalledVersions tool (_tvTarget tver)
    let latestVer = maximum vers
    pure $ latestVer == _tvVersion tver

