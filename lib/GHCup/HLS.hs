{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

{-|
Module      : GHCup.HLS
Description : GHCup installation functions for HLS
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.HLS where

import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Prelude
import           GHCup.Prelude.File
import           GHCup.Prelude.Logger
import           GHCup.Prelude.Process
import           GHCup.Prelude.String.QQ

import           Codec.Archive                  ( ArchiveResult )
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Data.ByteString                ( ByteString )
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import           Data.Versions                hiding ( patch )
import           Distribution.Types.Version   hiding ( Version )
import           Distribution.Types.PackageId
import           Distribution.Types.PackageDescription
import           Distribution.Types.GenericPackageDescription
import           Distribution.PackageDescription.Parsec
import           GHC.IO.Exception
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , writeFile
                                                )
import           Safe                    hiding ( at )
import           System.FilePath
import           System.IO.Error
import           Text.Regex.Posix
import           URI.ByteString

import qualified Data.List.NonEmpty            as NE
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as MP
import Text.PrettyPrint.HughesPJClass (prettyShow)


data HLSVer = SourceDist Version
            | GitDist GitBranch
            | HackageDist Version
            | RemoteDist URI



    --------------------
    --[ Installation ]--
    --------------------


-- | Like 'installHLSBin, except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installHLSBindist :: ( MonadMask m
                     , MonadCatch m
                     , MonadReader env m
                     , HasPlatformReq env
                     , HasDirs env
                     , HasSettings env
                     , HasLog env
                     , MonadResource m
                     , MonadIO m
                     , MonadUnliftIO m
                     , MonadFail m
                     )
                  => DownloadInfo
                  -> Version
                  -> InstallDir -- ^ isolated install path, if user passed any
                  -> Bool       -- ^ Force install
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
                        , ProcessError
                        , DirNotEmpty
                        , UninstallFailed
                        , MergeFileTreeError
                        ]
                       m
                       ()
installHLSBindist dlinfo ver installDir forceInstall = do
  lift $ logDebug $ "Requested to install hls version " <> prettyVer ver

  PlatformRequest {..} <- lift getPlatformReq
  Dirs {..} <- lift getDirs

  regularHLSInstalled <- lift $ hlsInstalled ver

  if
    | not forceInstall
    , regularHLSInstalled
    , GHCupInternal <- installDir -> do        -- regular install
        throwE $ AlreadyInstalled HLS ver

    | forceInstall
    , regularHLSInstalled
    , GHCupInternal <- installDir -> do        -- regular forced install
        lift $ logInfo "Removing the currently installed version of HLS before force installing!"
        liftE $ rmHLSVer ver

    | otherwise -> pure ()

  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack <- lift withGHCupTmpDir
  liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)

  -- the subdir of the archive where we do the work
  workdir <- fromGHCupPath <$> maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)
  legacy <- liftIO $ isLegacyHLSBindist workdir

  if
    | not forceInstall
    , not legacy
    , (IsolateDir fp) <- installDir -> liftE $ installDestSanityCheck (IsolateDirResolved fp)
    | otherwise -> pure ()

  case installDir of
    IsolateDir isoDir -> do
      lift $ logInfo $ "isolated installing HLS to " <> T.pack isoDir
      if legacy
      then liftE $ installHLSUnpackedLegacy workdir (IsolateDirResolved isoDir) ver forceInstall
      else liftE $ runBuildAction tmpUnpack $ installHLSUnpacked workdir (IsolateDirResolved isoDir) ver forceInstall

    GHCupInternal -> do
      if legacy
      then liftE $ installHLSUnpackedLegacy workdir (GHCupBinDir binDir) ver forceInstall
      else do
        inst <- ghcupHLSDir ver
        liftE $ runBuildAction tmpUnpack
              $ installHLSUnpacked workdir (GHCupDir inst) ver forceInstall
        liftE $ setHLS ver SetHLS_XYZ Nothing


isLegacyHLSBindist :: FilePath -- ^ Path to the unpacked hls bindist
                   -> IO Bool
isLegacyHLSBindist path = do
  not <$> doesFileExist (path </> "GNUmakefile")

-- | Install an unpacked hls distribution.
installHLSUnpacked :: ( MonadMask m
                      , MonadUnliftIO m
                      , MonadReader env m
                      , MonadFail m
                      , HasLog env
                      , HasDirs env
                      , HasSettings env
                      , MonadCatch m
                      , MonadIO m
                      , MonadResource m
                      , HasPlatformReq env
                      )
                   => FilePath      -- ^ Path to the unpacked hls bindist (where the executable resides)
                   -> InstallDirResolved      -- ^ Path to install to
                   -> Version
                   -> Bool
                   -> Excepts '[ProcessError, CopyError, FileAlreadyExistsError, NotInstalled, MergeFileTreeError] m ()
installHLSUnpacked path inst ver forceInstall = do
  PlatformRequest { .. } <- lift getPlatformReq
  lift $ logInfo "Installing HLS"
  tmpInstallDest <- lift withGHCupTmpDir
  lEM $ make ["DESTDIR=" <> fromGHCupPath tmpInstallDest, "PREFIX=" <> fromInstallDir inst, "install"] (Just path)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpInstallDest)
  liftE $ mergeFileTree (tmpInstallDest `appendGHCupPath` dropDrive (fromInstallDir inst))
                   inst
                   HLS
                   (mkTVer ver)
                   (\f t -> liftIO $ do
                       mtime <- ifM (pathIsSymbolicLink f) (pure Nothing) (Just <$> getModificationTime f)
                       install f t (not forceInstall)
                       forM_ mtime $ setModificationTime t)

-- | Install an unpacked hls distribution (legacy).
installHLSUnpackedLegacy :: (MonadReader env m, MonadFail m, HasLog env, MonadCatch m, MonadIO m)
                         => FilePath      -- ^ Path to the unpacked hls bindist (where the executable resides)
                         -> InstallDirResolved      -- ^ Path to install to
                         -> Version
                         -> Bool          -- ^ is it a force install
                         -> Excepts '[CopyError, FileAlreadyExistsError] m ()
installHLSUnpackedLegacy path installDir ver forceInstall = do
  lift $ logInfo "Installing HLS"
  liftIO $ createDirRecursive' (fromInstallDir installDir)

  -- install haskell-language-server-<ghcver>
  bins@(_:_) <- liftIO $ findFiles
    path
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^haskell-language-server-[0-9].*$|] :: ByteString)
    )
  forM_ bins $ \f -> do
    let toF = dropSuffix exeExt f
              <> (case installDir of
                   IsolateDirResolved _ -> ""
                   _ -> ("~" <>) . T.unpack . prettyVer $ ver
                 )
              <> exeExt

    let srcPath = path </> f
    let destPath = fromInstallDir installDir </> toF

    -- destination could be an existing symlink
    -- for new make-based HLSes
    liftIO $ rmFileForce destPath

    copyFileE
      srcPath
      destPath
      (not forceInstall)
    lift $ chmod_755 destPath

  -- install haskell-language-server-wrapper
  let wrapper = "haskell-language-server-wrapper"
      toF = wrapper
            <> (case installDir of
                 IsolateDirResolved _ -> ""
                 _ -> ("-" <>) . T.unpack . prettyVer $ ver
               )
            <> exeExt
      srcWrapperPath = path </> wrapper <> exeExt
      destWrapperPath = fromInstallDir installDir </> toF

  liftIO $ rmFileForce destWrapperPath
  copyFileE
    srcWrapperPath
    destWrapperPath
    (not forceInstall)

  lift $ chmod_755 destWrapperPath



-- | Installs hls binaries @haskell-language-server-\<ghcver\>@
-- into @~\/.ghcup\/bin/@, as well as @haskell-languager-server-wrapper@.
installHLSBin :: ( MonadMask m
                 , MonadCatch m
                 , MonadReader env m
                 , HasPlatformReq env
                 , HasGHCupInfo env
                 , HasDirs env
                 , HasSettings env
                 , HasLog env
                 , MonadResource m
                 , MonadIO m
                 , MonadUnliftIO m
                 , MonadFail m
                 )
              => Version
              -> InstallDir
              -> Bool            -- force install
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
                    , ProcessError
                    , DirNotEmpty
                    , UninstallFailed
                    , MergeFileTreeError
                    ]
                   m
                   ()
installHLSBin ver installDir forceInstall = do
  dlinfo <- liftE $ getDownloadInfo HLS ver
  installHLSBindist dlinfo ver installDir forceInstall


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
           -> Either Bool Version
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
                       ] m Version
compileHLS targetHLS ghcs jobs ov installDir cabalProject cabalProjectLocal updateCabal patches cabalArgs = do
  PlatformRequest { .. } <- lift getPlatformReq
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  Dirs { .. } <- lift getDirs

  when updateCabal $ reThrowAll @_ @'[ProcessError] DownloadFailed $ do
    lift $ logInfo "Updating cabal DB"
    lEM $ exec "cabal" ["update"] (Just $ fromGHCupPath tmpDir) Nothing

  (workdir, tmpUnpack, tver, git_describe) <- case targetHLS of
    -- unpack from version tarball
    SourceDist tver -> do
      lift $ logDebug $ "Requested to compile: " <> prettyVer tver

      -- download source tarball
      dlInfo <-
        preview (ix HLS % ix tver % viSourceDL % _Just) dls
          ?? NoDownload
      dl <- liftE $ downloadCached dlInfo Nothing

      -- unpack
      tmpUnpack <- lift mkGhcupTmpDir
      liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
      liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)

      workdir <- maybe (pure tmpUnpack)
                       (liftE . intoSubdir tmpUnpack)
                       (view dlSubdir dlInfo)

      pure (workdir, tmpUnpack, tver, Nothing)

    HackageDist tver -> do
      lift $ logDebug $ "Requested to compile (from hackage): " <> prettyVer tver

      -- download source tarball
      tmpUnpack <- lift mkGhcupTmpDir
      let hls = "haskell-language-server-" <> T.unpack (prettyVer tver)
      reThrowAll @_ @'[ProcessError] DownloadFailed $ do
        -- unpack
        lEM $ exec "cabal" ["unpack", hls] (Just $ fromGHCupPath tmpUnpack) Nothing

      let workdir = appendGHCupPath tmpUnpack hls

      pure (workdir, tmpUnpack, tver, Nothing)

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

      pure (workdir, tmpUnpack, tver, Nothing)

    -- clone from git
    GitDist GitBranch{..} -> do
      tmpUnpack <- lift mkGhcupTmpDir
      let git args = execLogged "git" ("--no-pager":args) (Just $ fromGHCupPath tmpUnpack) "git" Nothing
      reThrowAll @_ @'[ProcessError] DownloadFailed $ do
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
        tver <- getCabalVersion (fromGHCupPath tmpUnpack </> "haskell-language-server.cabal")

        liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)
        lift $ logInfo $ "Examining git ref " <> T.pack ref <> "\n  " <>
                                    "HLS version (from cabal file): " <> prettyVer tver <>
                                    (if not shallow_clone then "\n  " <> "'git describe' output: " <> fromJust git_describe else mempty) <>
                                    (if isCommitHash ref then mempty else "\n  " <> "commit hash: " <> chash)

        pure (tmpUnpack, tmpUnpack, tver, git_describe)

  -- the version that's installed may differ from the
  -- compiled version, so the user can overwrite it
  installVer <- case ov of
                  Left True -> case git_describe of
                                 -- git describe
                                 Just h -> either (fail . displayException) pure . version $ h
                                 -- git describe, but not building from git, lol
                                 Nothing -> pure tver
                  -- default: use detected version
                  Left False -> pure tver
                  -- overwrite version with users value
                  Right v -> pure v

  liftE $ runBuildAction
    tmpUnpack
    (reThrowAll @_ @'[GPGError, DownloadFailed, DigestError, ContentLengthError, PatchFailed, ProcessError, FileAlreadyExistsError, CopyError] @'[BuildFailed] (BuildFailed $ fromGHCupPath workdir) $ do
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
      artifacts <- forM (sort ghcs) $ \ghc -> do
        let ghcInstallDir = tmpInstallDir </> T.unpack (prettyVer ghc)
        liftIO $ createDirRecursive' tmpInstallDir
        lift $ logInfo $ "Building HLS " <> prettyVer installVer <> " for GHC version " <> prettyVer ghc
        liftE $ lEM @_ @'[ProcessError] $
          execLogged "cabal" ( [ "v2-install"
                               , "-w"
                               , "ghc-" <> T.unpack (prettyVer ghc)
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

      forM_ artifacts $ \artifact -> do
        logDebug $ T.pack (show artifact)
        liftIO $ renameFile (artifact </> "haskell-language-server" <.> exeExt)
          (tmpInstallDir </> "haskell-language-server-" <> takeFileName artifact <.> exeExt)
        liftIO $ renameFile (artifact </> "haskell-language-server-wrapper" <.> exeExt)
          (tmpInstallDir </> "haskell-language-server-wrapper" <.> exeExt)

      case installDir of
        IsolateDir isoDir -> do
          lift $ logInfo $ "isolated installing HLS to " <> T.pack isoDir
          liftE $ installHLSUnpackedLegacy tmpInstallDir (IsolateDirResolved isoDir) installVer True
        GHCupInternal -> do
          liftE $ installHLSUnpackedLegacy tmpInstallDir (GHCupBinDir binDir) installVer True
    )

  pure installVer
 where
  gitDescribeRequested = case ov of
                           Left b -> b
                           _      -> False


    -----------------
    --[ Set/Unset ]--
    -----------------

-- | Set the haskell-language-server symlinks.
setHLS :: ( MonadReader env m
          , HasDirs env
          , HasLog env
          , MonadIO m
          , MonadMask m
          , MonadFail m
          , MonadUnliftIO m
          )
       => Version
       -> SetHLS
       -> Maybe FilePath  -- if set, signals that we're not operating in ~/.ghcup/bin
                          -- and don't want mess with other versions
       -> Excepts '[NotInstalled] m ()
setHLS ver shls mBinDir = do
  whenM (lift $ not <$> hlsInstalled ver) (throwE (NotInstalled HLS (GHCTargetVersion Nothing ver)))

  -- symlink destination
  binDir <- case mBinDir of
    Just x -> pure x
    Nothing -> do
      Dirs {binDir = f} <- lift getDirs
      pure f

  -- first delete the old symlinks
  when (isNothing mBinDir) $
    case shls of
      -- not for legacy
      SetHLS_XYZ -> liftE $ rmMinorHLSSymlinks ver
      -- legacy and new
      SetHLSOnly -> liftE rmPlainHLS

  case shls of
    -- not for legacy
    SetHLS_XYZ -> do
      bins <- lift $ hlsInternalServerScripts ver Nothing

      forM_ bins $ \f -> do
        let fname = takeFileName f
        destL <- binarySymLinkDestination binDir f
        let target = if "haskell-language-server-wrapper" `isPrefixOf` fname
                     then fname <> "-" <> T.unpack (prettyVer ver) <> exeExt
                     else fname <> "~" <> T.unpack (prettyVer ver) <> exeExt
        lift $ createLink destL (binDir </> target)

    -- legacy and new
    SetHLSOnly -> do
      -- set haskell-language-server-<ghcver> symlinks
      bins <- lift $ hlsServerBinaries ver Nothing
      when (null bins) $ throwE $ NotInstalled HLS (GHCTargetVersion Nothing ver)

      forM_ bins $ \f -> do
        let destL = f
        let target = (<> exeExt) . head . splitOn "~" $ f
        lift $ createLink destL (binDir </> target)

      -- set haskell-language-server-wrapper symlink
      let destL = "haskell-language-server-wrapper-" <> T.unpack (prettyVer ver) <> exeExt
      let wrapper = binDir </> "haskell-language-server-wrapper" <> exeExt

      lift $ createLink destL wrapper

      when (isNothing mBinDir) $
        lift warnAboutHlsCompatibility

      liftIO (isShadowed wrapper) >>= \case
        Nothing -> pure ()
        Just pa -> lift $ logWarn $ T.pack $ prettyShow (ToolShadowed HLS pa wrapper ver)


unsetHLS :: ( MonadMask m
            , MonadReader env m
            , HasDirs env
            , MonadIO m)
         => m ()
unsetHLS = do
  Dirs {..} <- getDirs
  let wrapper = binDir </> "haskell-language-server-wrapper" <> exeExt
  bins   <- liftIO $ handleIO (\_ -> pure []) $ findFiles'
    binDir
    (MP.chunk "haskell-language-server-" <* pvp' <* MP.chunk (T.pack exeExt) <* MP.eof)
  forM_ bins (hideError doesNotExistErrorType . rmLink . (binDir </>))
  hideError doesNotExistErrorType $ rmLink wrapper




    ---------------
    --[ Removal ]--
    ---------------


-- | Delete a hls version. Will try to fix the hls symlinks
-- after removal (e.g. setting it to an older version).
rmHLSVer :: ( MonadMask m
            , MonadReader env m
            , HasDirs env
            , MonadThrow m
            , HasLog env
            , MonadIO m
            , MonadFail m
            , MonadCatch m
            , MonadUnliftIO m
            )
         => Version
         -> Excepts '[NotInstalled, UninstallFailed] m ()
rmHLSVer ver = do
  whenM (lift $ fmap not $ hlsInstalled ver) $ throwE (NotInstalled HLS (GHCTargetVersion Nothing ver))

  isHlsSet <- lift hlsSet

  liftE $ rmMinorHLSSymlinks ver

  when (Just ver == isHlsSet) $ do
    -- delete all set symlinks
    liftE rmPlainHLS

  hlsDir' <- ghcupHLSDir ver
  let hlsDir = fromGHCupPath hlsDir'
  lift (getInstalledFiles HLS (mkTVer ver)) >>= \case
    Just files -> do
      lift $ logInfo $ "Removing files safely from: " <> T.pack hlsDir
      forM_ files (lift . hideError NoSuchThing . recycleFile . (\f -> hlsDir </> dropDrive f))
      removeEmptyDirsRecursive hlsDir
      survivors <- liftIO $ hideErrorDef [doesNotExistErrorType] [] $ listDirectory hlsDir
      f <- recordedInstallationFile HLS (mkTVer ver)
      lift $ recycleFile f
      when (not (null survivors)) $ throwE $ UninstallFailed hlsDir survivors
    Nothing -> do
      isDir <- liftIO $ doesDirectoryExist hlsDir
      isSyml <- liftIO $ handleIO (\_ -> pure False) $ pathIsSymbolicLink hlsDir
      when (isDir && not isSyml) $ do
        lift $ logInfo $ "Removing legacy directory recursively: " <> T.pack hlsDir
        recyclePathForcibly hlsDir'

  when (Just ver == isHlsSet) $ do
    -- set latest hls
    hlsVers <- lift $ fmap rights getInstalledHLSs
    case headMay . reverse . sort $ hlsVers of
      Just latestver -> liftE $ setHLS latestver SetHLSOnly Nothing
      Nothing        -> pure ()


getCabalVersion :: (MonadIO m, MonadFail m) => FilePath -> m Version
getCabalVersion fp = do
  contents <- liftIO $ B.readFile fp
  gpd <- case parseGenericPackageDescriptionMaybe contents of
           Nothing -> fail $ "could not parse cabal file: " <> fp
           Just r -> pure r
  let tver = (\c -> Version Nothing c [] Nothing)
           . NE.fromList . fmap (NE.fromList . (:[]) . digits . fromIntegral)
           . versionNumbers
           . pkgVersion
           . package
           . packageDescription
           $ gpd
  pure tver
