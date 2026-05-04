{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Command.Install where

import GHCup.Builder
import GHCup.Command.Install.LowLevel
import GHCup.Command.Rm
import GHCup.Download
import GHCup.Errors
import GHCup.Input.SymlinkSpec
import GHCup.Prelude
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.Query.Metadata
import GHCup.System.Cmd
import GHCup.Types
import GHCup.Types.Optics
import GHCup.Unpack

import Control.Applicative
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Conduit                        ( runConduitRes, (.|) )
import Control.Monad.Reader
import Control.Monad.Trans.Resource   hiding ( throwM )
import Data.Maybe
import Data.Variant.Excepts
import Optics
import Prelude                        hiding ( abs )
import System.FilePath                ( dropDrive )
import System.IO.Error                ( doesNotExistErrorType )
import Text.PrettyPrint.HughesPJClass ( prettyShow )

import qualified Data.Conduit.Combinators as C
import qualified Data.Text                as T



    -------------------------
    --[ Tool installation ]--
    -------------------------

-- | Installs cabal into @~\/.ghcup\/bin/cabal-\<ver\>@ and
-- creates a default @cabal -> cabal-x.y.z.q@ symlink for
-- the latest installed version.
installTool ::
  ( MonadReader env m
  , HasPlatformReq env
  , HasGHCupInfo env  -- this is the main difference to the other functions
  , HasDirs env
  , HasSettings env
  , HasLog env
  , MonadResource m
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
  -> InstallDir
  -> Bool
  -> [String]
  -> Maybe [String]
  -> Excepts
       '[ AlreadyInstalled
        , CopyError
        , DigestError
        , ContentLengthError
        , GPGError
        , DownloadFailed
        , NoDownload
        , NotInstalled
        , UnknownArchive
        , TarDirDoesNotExist
        , ArchiveResult
        , FileAlreadyExistsError
        , URIParseError
        , NoInstallInfo
        , MergeFileTreeError
        , ProcessError
        , ParseError
        , DirNotEmpty
        , UninstallFailed
        , MalformedInstallInfo
        ]
       m
       (InstallationSpecResolved, FilePath)
installTool tool tver installDir forceInstall extraArgs installTargets = do
  GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
  dlinfo <- liftE $ getDownloadInfo' tool tver
  installBindist tool (preview (ix tool % toolDetails % _Just) dls) dlinfo tver installDir forceInstall extraArgs installTargets

-- | Like 'installCabalBin', except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installBindist ::
  ( MonadReader env m
  , HasPlatformReq env
  , HasDirs env
  , HasSettings env
  , HasLog env
  , MonadResource m
  , MonadIOish m
  )
  => Tool
  -> Maybe ToolDescription
  -> DownloadInfo
  -> TargetVersion
  -> InstallDir
  -> Bool           -- ^ Force install
  -> [String]
  -> Maybe [String]
  -> Excepts
       '[ AlreadyInstalled
        , CopyError
        , DigestError
        , ContentLengthError
        , GPGError
        , DownloadFailed
        , NoDownload
        , NotInstalled
        , UnknownArchive
        , TarDirDoesNotExist
        , ArchiveResult
        , FileAlreadyExistsError
        , URIParseError
        , NoInstallInfo
        , MergeFileTreeError
        , ProcessError
        , ParseError
        , DirNotEmpty
        , UninstallFailed
        , MalformedInstallInfo
        ]
       m
       (InstallationSpecResolved, FilePath)
installBindist tool toolDesc dlinfo tver installDir forceInstall extraArgs installTargets = do
  lift $ logDebug $ "Requested to install "
                     <> T.pack (prettyShow tool)
                     <> " version " <> T.pack (prettyShow tver)

  installed <- lift $ isInstalled tool tver
  if
    | not forceInstall
    , installed
    , GHCupInternal <- installDir -> do
        throwE $ AlreadyInstalled tool tver

    | forceInstall
    , installed
    , GHCupInternal <- installDir -> do
        lift $ logInfo "Removing the currently installed version first!"
        liftE $ rmToolVersion tool tver

    | otherwise -> pure ()


  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing

  case installDir of
    IsolateDir isoDir -> do             -- isolated install
      lift $ logInfo $ "isolated installing Cabal to " <> T.pack isoDir
      installSpec <- liftE $ installPackedBindist tool toolDesc dl dlinfo (IsolateDirResolved isoDir) tver forceInstall extraArgs installTargets
      pure (installSpec, isoDir)

    GHCupInternal -> do                 -- regular install
      instDir <- lift $ toolInstallDestination tool tver
      installSpec <- liftE $ installPackedBindist tool toolDesc dl dlinfo (GHCupDir instDir) tver forceInstall extraArgs installTargets
      pure (installSpec, fromGHCupPath instDir)

installPackedBindist ::
  ( MonadReader env m
  , HasDirs env
  , HasPlatformReq env
  , HasSettings env
  , HasLog env
  , MonadResource m
  , MonadIOish m
  )
  => Tool
  -> Maybe ToolDescription
  -> FilePath             -- ^ Path to the tarball
  -> DownloadInfo
  -> InstallDirResolved   -- ^ Path to install to
  -> TargetVersion
  -> Bool
  -> [String]
  -> Maybe [String]
  -> Excepts '[ CopyError
              , FileAlreadyExistsError
              , MergeFileTreeError
              , ProcessError
              , ParseError
              , UnknownArchive
              , TarDirDoesNotExist
              , DirNotEmpty
              , ArchiveResult
              , NoInstallInfo
              , MalformedInstallInfo
              ] m InstallationSpecResolved
installPackedBindist tool toolDesc dl dlInfo inst tver forceInstall extraArgs installTargets = do
  PlatformRequest {..} <- lift getPlatformReq

  unless forceInstall
    (liftE $ installDestSanityCheck inst)

  tmpUnpack <- lift withGHCupTmpDir
  liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform
    (fromGHCupPath tmpUnpack)

  -- the subdir of the archive where we do the work
  workdir <- fromGHCupPath <$> maybe (pure tmpUnpack)
    (liftE . intoSubdir tmpUnpack) (view dlSubdir dlInfo)

  tmpInstallDest <- lift withGHCupTmpDir

  liftE $ runBuildAction tmpUnpack $ installUnpackedBindist tool toolDesc workdir
        inst tmpInstallDest dlInfo tver forceInstall extraArgs installTargets

-- | Install an unpacked distribution.
installUnpackedBindist :: forall m env .
  ( HasLog env
  , HasDirs env
  , HasSettings env
  , HasPlatformReq env
  , MonadReader env m
  , MonadIOish m
  )
  => Tool
  -> Maybe ToolDescription
  -> FilePath             -- ^ Path to the unpacked cabal bindist (where the executable resides)
  -> InstallDirResolved   -- ^ Path to install to
  -> GHCupPath            -- ^ DESTDIR
  -> DownloadInfo
  -> TargetVersion
  -> Bool
  -> [String]
  -> Maybe [String]
  -> Excepts '[ CopyError
              , FileAlreadyExistsError
              , MergeFileTreeError
              , ProcessError
              , ParseError
              , NoInstallInfo
              , MalformedInstallInfo
              ] m InstallationSpecResolved
installUnpackedBindist tool toolDesc workdir installDest tmpInstallDest dlInfo tver forceInstall extraArgs installTargets = do
  instSpec <- liftE $ installationSpecFromMetadata' dlInfo tool tver
  lift $ logInfo $ "Installing " <> T.pack (prettyShow tool)
  liftE $ installTheSpec instSpec workdir installDest tmpInstallDest extraArgs installTargets forceInstall
  let preserveMtimes = _isPreserveMtimes instSpec
  resolvedSymlinkSpecs <- liftE $ forM (view isExeSymLinked instSpec) (resolveSymlinkSpec (tmpInstallDest `appendGHCupPath` dropDrive (fromInstallDir installDest)))
  let resolvedInstSpec = resolveInstallationSpec (mconcat resolvedSymlinkSpecs) instSpec

  -- then merge to the live filesystem
  liftE $ mergeToFileSystem tool tver installDest tmpInstallDest preserveMtimes forceInstall False

  -- TODO: move to optparse install
  -- symlinking
  Dirs {..} <- lift getDirs
  parsedSymlinkSpec <- forM (_isExeSymLinked resolvedInstSpec) (liftE . parseSymlinkSpec (_tvVersion tver))
  liftE $ symlinkBinaries installDest parsedSymlinkSpec (GHCupBinDir binDir) tool tver

  -- write InstallationInfo to the disk
  lift $ recordInstallationInfo installDest tool toolDesc tver dlInfo resolvedInstSpec
  pure resolvedInstSpec


-- | Does basic checks for isolated installs
-- Isolated Directory:
--   1. if it doesn't exist -> proceed
--   2. if it exists and is empty -> proceed
--   3. if it exists and is non-empty -> panic and leave the house
installDestSanityCheck :: ( MonadIOish m
                          ) =>
                          InstallDirResolved ->
                          Excepts '[DirNotEmpty] m ()
installDestSanityCheck (IsolateDirResolved isoDir) = do
  hideErrorDef [doesNotExistErrorType] () $ do
    empty' <- lift $ runConduitRes $ getDirectoryContentsRecursiveUnsafe isoDir .| C.null
    when (not empty') (throwE $ DirNotEmpty isoDir)
installDestSanityCheck _ = pure ()
