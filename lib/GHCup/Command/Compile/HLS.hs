{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : GHCup.Command.Compile.HLS
Description : GHCup compile HLS
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Command.Compile.HLS where

import GHCup.Builder
import GHCup.Command.Install.LowLevel
import GHCup.Download
import GHCup.Errors
import GHCup.Input.SymlinkSpec
import GHCup.Legacy.HLS               ( installHLSUnpackedLegacy )
import GHCup.Prelude
import GHCup.Prelude.Process
import GHCup.Prelude.String.QQ
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.System.Cmd
import GHCup.System.Directory
import GHCup.Types
import GHCup.Types.Optics
import GHCup.Unpack

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Resource                 hiding ( throwM )
import Data.Either
import Data.List
import Data.Maybe
import Data.String                                  ( fromString )
import Data.Text                                    ( Text )
import Data.Variant.Excepts
import Data.Versions                                hiding ( patch )
import Distribution.PackageDescription.Parsec
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.Version                   hiding ( Version )
import Optics
import Prelude                                      hiding ( abs, writeFile )
import System.FilePath
import Text.Regex.Posix
import URI.ByteString

import qualified Data.ByteString    as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T


data HLSVer
  = SourceDist Version
  | GitDist GitBranch
  | HackageDist Version
  | RemoteDist URI
  deriving (Eq, Show)




compileHLS :: ( MonadMask m
              , MonadCatch m
              , MonadReader env m
              , HasDirs env
              , HasSettings env
              , HasPlatformReq env
              , HasGHCupInfo env
              , HasLog env
              , MonadResource m
              , MonadIO m
              , MonadUnliftIO m
              , MonadFail m
              )
           => HLSVer
           -> [Version]
           -> Maybe Int
           -> Maybe [VersionPattern]
           -> InstallDir
           -> Maybe (Either FilePath URI)
           -> Maybe URI
           -> Bool
           -> Maybe (Either FilePath [URI])  -- ^ patches
           -> [Text]                   -- ^ additional args to cabal install
           -> Excepts '[ NoDownload
                       , GPGError
                       , DownloadFailed
                       , DigestError
                       , ContentLengthError
                       , UnknownArchive
                       , TarDirDoesNotExist
                       , ArchiveResult
                       , BuildFailed
                       , NotInstalled
                       , URIParseError
                       ] m Version
compileHLS targetHLS ghcs jobs vps installDir cabalProject cabalProjectLocal updateCabal patches cabalArgs = do
  pfreq@PlatformRequest { .. } <- lift getPlatformReq
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  Dirs { .. } <- lift getDirs

  when updateCabal $ reThrowAll @_ @'[ProcessError] DownloadFailed $ do
    lift $ logInfo "Updating cabal DB"
    liftE $ execWithWrapper "cabal" ["update"] (Just $ fromGHCupPath tmpDir) "cabal" Nothing

  (workdir, tmpUnpack, tver, ov) <- case targetHLS of
    -- unpack from version tarball
    SourceDist tver -> do
      lift $ logDebug $ "Requested to compile: " <> prettyVer tver

      -- download source tarball
      dlInfo <-
        preview (ix hls % toolVersions % ix (mkTVer tver) % viSourceDL % _Just) dls
          ?? NoDownload (mkTVer tver) hls (Just pfreq)
      dl <- liftE $ downloadCached dlInfo Nothing

      -- unpack
      tmpUnpack <- lift mkGhcupTmpDir
      liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
      liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)

      workdir <- maybe (pure tmpUnpack)
                       (liftE . intoSubdir tmpUnpack)
                       (view dlSubdir dlInfo)

      ov <- case vps of
              Just vps' -> fmap Just $ expandVersionPattern (Just tver) "" "" "" "" vps'
              Nothing   -> pure Nothing

      pure (workdir, tmpUnpack, tver, ov)

    HackageDist tver -> do
      lift $ logDebug $ "Requested to compile (from hackage): " <> prettyVer tver

      -- download source tarball
      tmpUnpack <- lift mkGhcupTmpDir
      let hls' = "haskell-language-server-" <> T.unpack (prettyVer tver)
      reThrowAll @_ @'[ProcessError] DownloadFailed $ do
        -- unpack
        liftE $ execWithWrapper "cabal" ["unpack", hls'] (Just $ fromGHCupPath tmpUnpack) "cabal" Nothing

      let workdir = appendGHCupPath tmpUnpack hls'

      ov <- case vps of
              Just vps' -> fmap Just $ expandVersionPattern (Just tver) "" "" "" "" vps'
              Nothing   -> pure Nothing

      pure (workdir, tmpUnpack, tver, ov)

    RemoteDist uri -> do
      lift $ logDebug $ "Requested to compile (from uri): " <> T.pack (show uri)

      -- download source tarball
      tmpDownload <- lift withGHCupTmpDir
      tmpUnpack <- lift mkGhcupTmpDir
      tar <- liftE $ download uri Nothing Nothing Nothing (fromGHCupPath tmpDownload) Nothing False
      (cf, tver) <- liftE $ cleanUpOnError tmpUnpack $ do
        unpackToDir (fromGHCupPath tmpUnpack) tar
        let regex = [s|^(.*/)*haskell-language-server\.cabal$|] :: B.ByteString
        [cabalFile] <- liftIO $ findFilesDeep
          tmpUnpack
          (makeRegexOpts compExtended
                         execBlank
                         regex
          )
        tver <- getCabalVersion (fromGHCupPath tmpUnpack </> cabalFile)
        pure (cabalFile, tver)

      let workdir = appendGHCupPath tmpUnpack (takeDirectory cf)

      ov <- case vps of
              Just vps' -> fmap Just $ expandVersionPattern (Just tver) "" "" "" "" vps'
              Nothing   -> pure Nothing

      pure (workdir, tmpUnpack, tver, ov)

    -- clone from git
    GitDist GitBranch{..} -> do
      tmpUnpack <- lift mkGhcupTmpDir
      let git args = execLogged "git" ("--no-pager":args) (Just $ fromGHCupPath tmpUnpack) "git" Nothing
      cleanUpOnError tmpUnpack $ reThrowAll @_ @'[ProcessError] DownloadFailed $ do
        let rep = fromMaybe "https://github.com/haskell/haskell-language-server.git" repo
        lift $ logInfo $ "Fetching git repo " <> T.pack rep <> " at ref " <> T.pack ref <> " (this may take a while)"
        lEM $ git [ "init" ]
        lEM $ git [ "remote"
                  , "add"
                  , "origin"
                  , fromString rep ]

        -- figure out if we can do a shallow clone
        remoteBranches <- catchE @ProcessError @'[ProcessError] @'[] (\_ -> pure [])
            $ fmap processBranches $ gitOut ["ls-remote", "--heads", "origin"] (fromGHCupPath tmpUnpack)
        let shallow_clone
              | gitDescribeRequested                 = False
              | isCommitHash ref                     = True
              | fromString ref `elem` remoteBranches = True
              | otherwise                            = False

        lift $ logDebug $ "Shallow clone: " <> T.pack (show shallow_clone)

        -- fetch
        let fetch_args
              | shallow_clone = ["fetch", "--depth", "1", "--quiet", "origin", fromString ref]
              | otherwise     = ["fetch", "--tags",       "--quiet", "origin"                ]
        lEM $ git fetch_args

        -- checkout
        lEM $ git [ "checkout", fromString ref ]

        -- gather some info
        git_describe <- if shallow_clone
                        then pure Nothing
                        else fmap Just $ gitOut ["describe", "--tags"] (fromGHCupPath tmpUnpack)
        chash <- gitOut ["rev-parse", "HEAD" ] (fromGHCupPath tmpUnpack)
        branch <- gitOut ["rev-parse", "--abbrev-ref", "HEAD" ] (fromGHCupPath tmpUnpack)
        tver <- getCabalVersion (fromGHCupPath tmpUnpack </> "haskell-language-server.cabal")

        liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)

        ov <- case vps of
                Just vps' -> fmap Just $ expandVersionPattern
                                           (Just tver)
                                           (take 7 $ T.unpack chash)
                                           (T.unpack chash)
                                           (maybe "" T.unpack git_describe)
                                           (T.unpack branch)
                                           vps'
                Nothing -> pure Nothing

        lift $ logInfo $ "Examining git ref " <> T.pack ref <> "\n  " <>
                                    "HLS version (from cabal file): " <> prettyVer tver <>
                                    "\n  branch: " <> branch <>
                                    (if not shallow_clone then "\n  " <> "'git describe' output: " <> fromJust git_describe else mempty) <>
                                    (if isCommitHash ref then mempty else "\n  " <> "commit hash: " <> chash)
        pure (tmpUnpack, tmpUnpack, tver, ov)

  -- the version that's installed may differ from the
  -- compiled version, so the user can overwrite it
  installVer <- maybe (pure tver) pure ov

  liftE $ runBuildAction
    tmpUnpack
    (reThrowAll @_ @'[GPGError, DownloadFailed, DigestError, ContentLengthError, PatchFailed, ProcessError, FileAlreadyExistsError, CopyError, ParseError , MergeFileTreeError , MalformedInstallInfo, FileDoesNotExistError, NoInstallInfo] @'[BuildFailed] (BuildFailed $ fromGHCupPath workdir) $ do
      let tmpInstallDir = fromGHCupPath workdir </> "out"
      liftIO $ createDirRecursive' tmpInstallDir

      -- apply patches
      liftE $ applyAnyPatch patches (fromGHCupPath workdir)

      -- set up project files
      cp <- case cabalProject of
        Just (Left cp)
          | isAbsolute cp -> do
              copyFileE cp (fromGHCupPath workdir </> "cabal.project") False
              pure "cabal.project"
          | otherwise -> pure (takeFileName cp)
        Just (Right uri) -> do
          tmpUnpack' <- lift withGHCupTmpDir
          cp <- liftE $ download uri Nothing Nothing Nothing (fromGHCupPath tmpUnpack') (Just "cabal.project") False
          copyFileE cp (fromGHCupPath workdir </> "cabal.project") False
          pure "cabal.project"
        Nothing
          | HackageDist _ <- targetHLS -> do
              liftIO $ B.writeFile (fromGHCupPath workdir </> "cabal.project") "packages: ./"
              pure "cabal.project"
          | RemoteDist _ <- targetHLS -> do
              let cabalFile = fromGHCupPath workdir </> "cabal.project"
              liftIO $ whenM (not <$> doesFileExist cabalFile) $ B.writeFile cabalFile "packages: ./"
              pure "cabal.project"
          | otherwise -> pure "cabal.project"
      forM_ cabalProjectLocal $ \uri -> do
        tmpUnpack' <- lift withGHCupTmpDir
        cpl <- liftE $ download uri Nothing Nothing Nothing (fromGHCupPath tmpUnpack') (Just (cp <.> "local")) False
        copyFileE cpl (fromGHCupPath workdir </> cp <.> "local") False
      artifactDirs <- forM (sort ghcs) $ \ghc' -> do
        let ghcInstallDir = tmpInstallDir </> T.unpack (prettyVer ghc')
        liftIO $ createDirRecursive' tmpInstallDir
        lift $ logInfo $ "Building HLS " <> prettyVer installVer <> " for GHC version " <> prettyVer ghc'
        liftE $
          execWithWrapper "cabal" ( [ "v2-install"
                               , "-w"
                               , "ghc-" <> T.unpack (prettyVer ghc')
                               , "--install-method=copy"
                               ] ++
                               maybe [] (\j -> ["--jobs=" <> show j]) jobs ++
                               [ "--overwrite-policy=always"
                               , "--disable-profiling"
                               , "--disable-tests"
                               , "--installdir=" <> ghcInstallDir
                               , "--project-file=" <> cp
                               ] ++ fmap T.unpack cabalArgs ++ [
                                 "exe:haskell-language-server"
                               , "exe:haskell-language-server-wrapper"]
                             )
                             (Just $ fromGHCupPath workdir)
                             "cabal"
                             Nothing
        pure ghcInstallDir

      forM_ artifactDirs $ \artifactDir -> do
        logDebug $ "mv " <> T.pack (artifactDir </> "haskell-language-server" <.> exeExt) <> " "
                         <> T.pack (tmpInstallDir </> "haskell-language-server-" <> takeFileName artifactDir <.> exeExt)
        liftIO $ renameFile (artifactDir </> "haskell-language-server" <.> exeExt)
          (tmpInstallDir </> "haskell-language-server-" <> takeFileName artifactDir <.> exeExt)
        logDebug $ "mv " <> T.pack (artifactDir </> "haskell-language-server-wrapper" <.> exeExt) <> " "
                         <> T.pack (tmpInstallDir </> "haskell-language-server-wrapper" <.> exeExt)
        liftIO $ renameFile (artifactDir </> "haskell-language-server-wrapper" <.> exeExt)
          (tmpInstallDir </> "haskell-language-server-wrapper" <.> exeExt)


      installDirResolved <- case installDir of
                              IsolateDir isoDir -> pure $ IsolateDirResolved isoDir
                              GHCupInternal -> GHCupDir <$> lift (toolInstallDestination hls (mkTVer installVer))

      runE (getInstallMetadata hls (mkTVer installVer)) >>= \case
        VRight metadata -> do
          destdir <- lift withGHCupTmpDir
          liftE $ addAdHocBinaries (Just metadata) tmpInstallDir installDirResolved destdir installVer True
        VLeft (V (FileDoesNotExistError _)) -> do
          inst <- lift $ isInstalled hls (mkTVer installVer)
          if inst
          then liftE $ installHLSUnpackedLegacy tmpInstallDir (GHCupBinDir binDir) installVer True
          else do
            destdir <- lift withGHCupTmpDir
            liftE $ addAdHocBinaries Nothing tmpInstallDir installDirResolved destdir installVer True
        VLeft (V (ParseError pe)) -> liftE $ throwE @_ @'[ParseError] (ParseError pe)
        VLeft v -> lift $ fail (prettyHFError v)
    )

  pure installVer
 where
  gitDescribeRequested = maybe False (GitDescribe `elem`) vps

  adHocInstallationSpec :: [FilePath]         -- ^ no .exe extension
                        -> InstallationSpecResolved
  adHocInstallationSpec hlsExes =
    InstallationSpec {
      _isExeRules =
        hlsExes <&> (\exe -> InstallFileRule { _ibrInstallSource = exe <.> exeExt
                                             , _ibrInstallDest = Just $ "bin" </> exe <.> exeExt
                                             })
    , _isDataRules = []
    , _isConfigure = Nothing
    , _isMake = Nothing
    , _isExeSymLinked = hlsExes <&> \hlsExe ->
        if hlsExe == "haskell-language-server-wrapper"
        then SymlinkSpec { _slTarget        = "bin" </> hlsExe <.> exeExt
                         , _slLinkName      = hlsExe <> "-${PKGVER}" <.> exeExt
                         , _slPVPMajorLinks = False
                         , _slSetName       = Just (hlsExe <.> exeExt)
                         }
        else SymlinkSpec { _slTarget        = "bin" </> hlsExe <.> exeExt
                         , _slLinkName      = hlsExe <> "~${PKGVER}" <.> exeExt
                         , _slPVPMajorLinks = False
                         , _slSetName       = Just (hlsExe <.> exeExt)
                         }
    , _isPreserveMtimes = False
    }

  addAdHocBinaries :: forall m env .
    ( HasLog env
    , HasDirs env
    , HasSettings env
    , HasPlatformReq env
    , MonadReader env m
    , MonadIOish m
    )
    => Maybe InstallMetadata
    -> FilePath
    -> InstallDirResolved   -- ^ Path to install to
    -> GHCupPath            -- ^ DESTDIR
    -> Version
    -> Bool
    -> Excepts '[ CopyError
                , MergeFileTreeError
                , ParseError
                , MalformedInstallInfo
                , NoInstallInfo
                , ProcessError
                ] m ()
  addAdHocBinaries mMetadata workdir installDest tmpInstallDest (mkTVer -> tver) forceInstall = do
    binaries <- liftIO $ listDirectoryFiles workdir
    let spec = adHocInstallationSpec (dropSuffix exeExt <$> binaries)

    logDebug $ T.pack (show spec)
    liftE $ installTheSpec (toInstallationInputSpec spec) workdir installDest tmpInstallDest [] Nothing forceInstall

    liftE $ mergeToFileSystem hls tver installDest tmpInstallDest (_isPreserveMtimes spec) forceInstall True


    case installDir of
      -- set and make symlinks for regular (non-isolated) installs
      GHCupInternal -> do
        Dirs {..} <- lift getDirs
        parsedSymlinkSpec <- forM (_isExeSymLinked spec) (liftE . parseSymlinkSpec (_tvVersion tver))
        liftE $ symlinkBinaries installDest parsedSymlinkSpec (GHCupBinDir binDir) hls tver

        -- write InstallationInfo to the disk
        case mMetadata of
          (Just (InstallMetadata { _imResolvedInstallSpec, _imDownloadInfo })) -> do
            lift $ recordInstallationInfo installDest hls tver _imDownloadInfo (manipulateSpec _imResolvedInstallSpec spec)
          Nothing -> do
            let dlInfo = DownloadInfo "" Nothing "" Nothing Nothing Nothing (Just $ toInstallationInputSpec spec)
            lift $ recordInstallationInfo installDest hls tver dlInfo spec
      _ -> pure ()

    pure ()
   where
    -- we install ad-hoc binaries and now need to adjust the exeSymLinked part
    -- so that uninstallation removes the additional symlinks
    manipulateSpec :: InstallationSpecResolved -> InstallationSpecResolved -> InstallationSpecResolved
    manipulateSpec origSpec adhocSpec =
      let oldSymls = _isExeSymLinked origSpec
          newSymls = _isExeSymLinked adhocSpec
      in origSpec { _isExeSymLinked = merge oldSymls newSymls }
     where
      merge :: [SymlinkSpec String] -> [SymlinkSpec String] -> [SymlinkSpec String]
      merge old new = nub (old <> new)



    ---------------
    --[ Removal ]--
    ---------------



getCabalVersion :: (MonadIO m, MonadFail m) => FilePath -> m Version
getCabalVersion fp = do
  contents <- liftIO $ B.readFile fp
  gpd <- case parseGenericPackageDescriptionMaybe contents of
           Nothing -> fail $ "could not parse cabal file: " <> fp
           Just r  -> pure r
  let tver = (\c -> Version Nothing c Nothing Nothing)
           . Chunks
           . NE.fromList
           . fmap (Numeric . fromIntegral)
           . versionNumbers
           . pkgVersion
           . package
           . packageDescription
           $ gpd
  pure tver
