{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Command.Install.LowLevel where

import GHCup.Errors
import GHCup.Input.Parsers.Domain
import GHCup.Input.SymlinkSpec
import GHCup.Prelude
import GHCup.Prelude.Process
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.Query.Symlink
import GHCup.System.Cmd
import GHCup.System.Directory
import GHCup.Types
import GHCup.Types.JSON (safePath, safeFilename)
import GHCup.Types.Optics

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import Data.Yaml.Pretty
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Data.Maybe
import Data.Variant.Excepts
import Data.Versions                hiding ( patch )
import Prelude                      hiding ( abs )
import System.FilePath
import System.FilePattern.Directory

import qualified Data.ByteString       as B
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import qualified System.FilePath.Posix as Posix
import qualified Text.Megaparsec       as MP
import Safe (maximumMay)





mergeToFileSystem ::
  ( MonadMask m
  , MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadCatch m
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
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
              , InvalidBuildConfig
              ] m ()
installTheSpec InstallationSpec{..} workdir installDest tmpInstallDest extraArgs installTargets forceInstall = do
  -- run configure first
  case _isConfigure of
    Just ConfigSpec{..} -> do
      addConfArgs <- lift $ sanitizefConfOptions extraArgs

      let configFile = fromMaybe "configure" _csConfigFile
      processedConfArgs <- forM _csConfigArgs $ \a -> liftE $ throwOnParseError . parseDomain $ a
      newEnv <- forM _csConfigEnv $ \EnvSpec{..} -> do
        processedEnv <- liftE $ throwOnParseError $ forM _esEnv (\(var, val) -> (var,) <$> parseDomain val)
        liftIO (augmentEnvironment processedEnv _esUnion)
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
      processedMakeArgs <- case installTargets of
        Just it -> do
          -- we need to check whether applying 'installTargets' would wipe out PREFIX/DESTDIR
          origArgs <- liftE $ throwOnParseError $ forM _msMakeArgs parseDomain
          if origArgs /= _msMakeArgs
          then throwE $ InvalidBuildConfig $ "Can't overwrite install targets " <> T.pack (show _msMakeArgs) <> " since they contain domain variables."
          else liftE $ throwOnParseError $ forM it parseDomain
        Nothing ->
          liftE $ throwOnParseError $ forM _msMakeArgs parseDomain

      newEnv <- forM _msMakeEnv $ \EnvSpec{..} -> do
        processedEnv <- liftE $ throwOnParseError $ forM _esEnv (\(var, val) -> (var,) <$> parseDomain val)
        liftIO (augmentEnvironment processedEnv _esUnion)
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
    handleIO (throwE . CopyError . show) . liftIO . install from into  $ not forceInstall
    logDebug $ "cp " <> (if forceInstall then "-f " else "") <> T.pack from <> " " <> T.pack into
    pure into

  env = M.fromList [(PREFIX, fromInstallDir installDest)
                   ,(TMPDIR, fromGHCupPath tmpInstallDest)
                   ]
  parseDomain = MP.parse (domainParser env) "installTheSpec"

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
  -> Maybe ToolDescription
  -> TargetVersionRev
  -> DownloadInfo
  -> InstallationSpecResolved
  -> m ()
recordInstallationInfo installDest tool toolDesc TargetVersionRev{..} dlInfo instSpec
  | isSafeDir installDest = do
      spec <- recordedInstallationSpecFile tool _tvrTargetVer
      liftIO $ createDirectoryIfMissing True (takeDirectory spec)
      let metadata = InstallMetadata dlInfo instSpec toolDesc _tvrRev
      logDebug2 $ "Writing install metadata to " <> T.pack spec <> "\n  " <> T.pack (show metadata)
      liftIO $ encodeFilePretty spec metadata
  | otherwise = do
      logDebug2 $ "Skipping spec installation, because installing into isolated dir: " <> T.pack (show installDest)
      pure ()
 where
  encodeFilePretty file json =
    let encoded = encodePretty defConfig json
    in B.writeFile file encoded

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
  -> TargetVersion
  -> Excepts '[MalformedInstallInfo] m ()
symlinkBinaries (IsolateDirResolved _) _ _ _ _ = pure ()
symlinkBinaries idr@(fromInstallDir -> toolDir) rawSpec bindir tool tver = do
  let spec = substituteSpec <$> rawSpec
  forM_ spec safeSpec
  pvpSyms <- lift $ getPVPSymlinks' (fromInstallDir bindir) spec toolDir
  forM_ pvpSyms $ lift . uncurry createLink
  symlinkMajorMinor idr rawSpec bindir tool tver
 where
  safeSpec SymlinkSpec{..} = do
    checkSafePath _slTarget
    checkSafeFilename _slLinkName
    forM_ _slSetName checkSafeFilename

  checkSafePath fp = unless (safePath fp) $ throwE (MalformedInstallInfo "'..' or '.' are not allowed")

  checkSafeFilename fp = unless (safeFilename fp) $ throwE (MalformedInstallInfo "'..' or '.' are not allowed and filepath must have no path separators")


-- | For the given X.Y version, we
-- find the latest X.Y.Z and create symlinks.
fixMajorMinorSymlink ::
  ( HasLog env
  , MonadReader env m
  , HasDirs env   -- createLink needs it on windows
  , HasPlatformReq env
  , MonadIOish m
  )
  => Tool
  -> Maybe T.Text
  -> (Int, Int)
  -> Excepts '[MalformedInstallInfo, ParseError] m ()
fixMajorMinorSymlink tool mtarget majorMinor = do
  Dirs {..}  <- getDirs
  vers <- lift $ getInstalledVersions tool mtarget
  let filterMajor = filter (\(getMajorMinorV  . _vrVersion -> mj) -> mj == Just majorMinor)
  let mLatestMajorVer = fmap _vrVersion $ maximumMay $ filterMajor vers
  case mLatestMajorVer of
    Nothing -> pure ()
    Just latestMajorVer -> do
      let tver = TargetVersion mtarget latestMajorVer
      idr <- toolInstallDestination tool tver
      rawSpec <- liftE $ getSymlinkSpecPortable' tool tver
      liftE $ symlinkMajorMinor (GHCupDir idr) rawSpec (GHCupBinDir binDir) tool tver


-- | Create X.Y major symlinks for the given GHC version, but only
-- if it is the 'latest' version.
--
-- E.g. if we have
--   * ghc-9.8.3
--   * ghc-9.8.4
-- then we want the following symlink:
--   * ghc-9.8 -> ghc-9.8.4
symlinkMajorMinor ::
  ( HasLog env
  , MonadReader env m
  , HasDirs env   -- createLink needs it on windows
  , MonadIOish m
  )
  => InstallDirResolved                    -- ^ base dir of the tool
  -> [SymlinkSpec [Either Char Version]]   -- ^ symlink spec
  -> InstallDirResolved                    -- ^ binary dir
  -> Tool
  -> TargetVersion
  -> Excepts '[MalformedInstallInfo] m ()
symlinkMajorMinor (IsolateDirResolved _) _ _ _ _ = pure ()
symlinkMajorMinor (fromInstallDir -> toolDir) rawSpec bindir tool tver = do
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
    case getMajorMinorV (_tvVersion tver) of
      Just cMJ -> do
        vers <- getInstalledVersions tool (_tvTarget tver)
        let filterMajor = filter (\(getMajorMinorV  . _vrVersion -> mj) -> mj == Just cMJ)
        let latestMajorVer = fmap _vrVersion $ maximumMay $ filterMajor vers
        pure $ latestMajorVer == Just (_tvVersion tver)
      Nothing -> pure False
