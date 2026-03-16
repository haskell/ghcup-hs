{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : GHCup.Command.Compile.GHC
Description : GHCup compile GHC
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Command.Compile.GHC where


import GHCup.Builder
import GHCup.Command.Install
import GHCup.Command.Install.LowLevel
import GHCup.Command.Rm
import GHCup.Command.Set
import GHCup.Download
import GHCup.Errors
import GHCup.Prelude
import GHCup.Prelude.MegaParsec
import GHCup.Prelude.Process
import GHCup.Prelude.String.QQ
import GHCup.Prelude.Version.QQ
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.System.Cmd
import GHCup.System.Directory
import GHCup.Types
import GHCup.Types.Optics
import GHCup.Unpack

import Control.Applicative
import Control.Concurrent     ( threadDelay )
import Control.Exception.Safe
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Resource   hiding ( throwM )
import Data.ByteString                ( ByteString )
import Data.Either
import Data.List
import Data.Maybe
import Data.String                    ( fromString )
import Data.Text                      ( Text )
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Data.Variant.Excepts
import Data.Versions                  hiding ( patch )
import GHC.IO.Exception
import Language.Haskell.TH
import Language.Haskell.TH.Syntax     ( Quasi (qAddDependentFile) )
import Optics
import Prelude                        hiding ( abs, writeFile )
import System.Environment
import System.FilePath
import System.FilePattern ((?==))
import System.IO.Error
import Text.PrettyPrint.HughesPJClass ( prettyShow )
import Text.Regex.Posix
import URI.ByteString

import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import qualified Data.Text.IO           as T
import qualified System.FilePath.Posix  as Posix
import qualified Text.Megaparsec        as MP


data GHCVer
  = SourceDist Version
  | GitDist GitBranch
  | RemoteDist URI
  deriving (Eq, Show)




    -------------------------
    --[ Tool installation ]--
    -------------------------




mergeGHCFileTree :: ( MonadReader env m
                    , HasPlatformReq env
                    , HasDirs env
                    , HasSettings env
                    , MonadThrow m
                    , HasLog env
                    , MonadIO m
                    , MonadUnliftIO m
                    , MonadMask m
                    , MonadResource m
                    , MonadFail m
                    )
                 => GHCupPath           -- ^ Path to the root of the tree
                 -> InstallDirResolved  -- ^ Path to install to
                 -> GHCTargetVersion    -- ^ The GHC version
                 -> Bool                -- ^ Force install
                 -> Excepts '[MergeFileTreeError] m ()
mergeGHCFileTree root inst tver forceInstall
  | isWindows = do
      liftE $ mergeFileTree root inst ghc tver (\source dest -> do
        mtime <- liftIO $ ifM (pathIsSymbolicLink source) (pure Nothing) (Just <$> getModificationTime source)
        when forceInstall $ hideError doesNotExistErrorType $ hideError InappropriateType $ recycleFile dest
        liftIO $ moveFilePortable source dest
        forM_ mtime $ liftIO . setModificationTime dest
        )
        False
  | otherwise = do
      liftE $ mergeFileTree root
        inst
        ghc
        tver
        (\f t -> liftIO $ do
            mtime <- ifM (pathIsSymbolicLink f) (pure Nothing) (Just <$> getModificationTime f)
            install f t (not forceInstall)
            forM_ mtime $ setModificationTime t)
        False




    ---------------
    --[ Compile ]--
    ---------------


-- | Compile a GHC from source. This behaves wrt symlinks and installation
-- the same as 'installGHCBin'.
compileGHC :: ( MonadMask m
              , MonadReader env m
              , HasDirs env
              , HasPlatformReq env
              , HasGHCupInfo env
              , HasSettings env
              , MonadThrow m
              , MonadResource m
              , HasLog env
              , MonadIO m
              , MonadUnliftIO m
              , MonadFail m
              )
           => GHCVer
           -> Maybe Text               -- ^ cross target
           -> Maybe [VersionPattern]
           -> Either Version FilePath  -- ^ GHC version to bootstrap with
           -> Maybe (Either Version FilePath)  -- ^ GHC version to compile hadrian with
           -> Maybe Int                -- ^ jobs
           -> Maybe FilePath           -- ^ build config
           -> Maybe (Either FilePath [URI])  -- ^ patches
           -> [String]                   -- ^ additional args to ./configure
           -> Maybe String             -- ^ build flavour
           -> Maybe BuildSystem
           -> InstallDir
           -> Maybe [String]
           -> Excepts
                '[ AlreadyInstalled
                 , BuildFailed
                 , DigestError
                 , ContentLengthError
                 , GPGError
                 , DownloadFailed
                 , GHCupSetError
                 , NoDownload
                 , NotFoundInPATH
                 , PatchFailed
                 , UnknownArchive
                 , TarDirDoesNotExist
                 , NotInstalled
                 , DirNotEmpty
                 , ArchiveResult
                 , FileDoesNotExistError
                 , HadrianNotFound
                 , InvalidBuildConfig
                 , ProcessError
                 , CopyError
                 , BuildFailed
                 , UninstallFailed
                 , MergeFileTreeError
                 , URIParseError
                 , FileAlreadyExistsError
                 , ParseError
                 , NoInstallInfo
                 , MalformedInstallInfo
                 ]
                m
                GHCTargetVersion
compileGHC targetGhc crossTarget vps bstrap hghc jobs mbuildConfig patches aargs buildFlavour buildSystem installDir installTargets
  = do
    pfreq@PlatformRequest { .. } <- lift getPlatformReq
    GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo

    (workdir, tmpUnpack, tver, ov) <- case targetGhc of
      -- unpack from version tarball
      SourceDist ver -> do
        lift $ logDebug $ "Requested to compile: " <> prettyVer ver <> " with " <> either prettyVer T.pack bstrap

        -- download source tarball
        let tver = mkTVer ver
        dlInfo <-
          preview (ix ghc % toolVersions % ix tver % viSourceDL % _Just) dls
            ?? NoDownload tver ghc (Just pfreq)
        dl <- liftE $ downloadCached dlInfo Nothing

        -- unpack
        tmpUnpack <- lift mkGhcupTmpDir
        liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
        liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform $ fromGHCupPath tmpUnpack

        workdir <- maybe (pure tmpUnpack)
                         (liftE . intoSubdir tmpUnpack)
                         (view dlSubdir dlInfo)
        liftE $ applyAnyPatch patches (fromGHCupPath workdir)

        ov <- case vps of
                Just vps' -> fmap Just $ expandVersionPattern (Just ver) "" "" "" "" vps'
                Nothing   -> pure Nothing

        pure (workdir, tmpUnpack, Just (GHCTargetVersion crossTarget ver), ov)

      RemoteDist uri -> do
        lift $ logDebug $ "Requested to compile (from uri): " <> T.pack (show uri)

        -- download source tarball
        tmpDownload <- lift withGHCupTmpDir
        tmpUnpack <- lift mkGhcupTmpDir
        tar <- liftE $ download uri Nothing Nothing Nothing (fromGHCupPath tmpDownload) Nothing False
        (bf, tver) <- liftE $ cleanUpOnError @'[UnknownArchive, ArchiveResult, ProcessError] tmpUnpack $ do
          liftE $ unpackToDir (fromGHCupPath tmpUnpack) tar
          let regex = [s|^(.*/)*boot$|] :: B.ByteString
          [bootFile] <- liftIO $ findFilesDeep
            tmpUnpack
            (makeRegexOpts compExtended
                           execBlank
                           regex
            )
          tver <- liftE $ catchAllE @_ @'[ProcessError, ParseError, NotFoundInPATH] @'[] (\_ -> pure Nothing) $ fmap Just $ getGHCVer
            (appendGHCupPath tmpUnpack (takeDirectory bootFile))
          pure (bootFile, tver)

        let workdir = appendGHCupPath tmpUnpack (takeDirectory bf)

        ov <- case vps of
                Just vps' -> fmap Just $ expandVersionPattern tver "" "" "" "" vps'
                Nothing   -> pure Nothing

        pure (workdir, tmpUnpack, GHCTargetVersion crossTarget <$> tver, ov)

      -- clone from git
      GitDist GitBranch{..} -> do
        tmpUnpack <- lift mkGhcupTmpDir
        let git args = execLogged "git" ("--no-pager":args) (Just $ fromGHCupPath tmpUnpack) "git" Nothing
        (tver, ov) <- cleanUpOnError tmpUnpack $ reThrowAll @_ @'[PatchFailed, ProcessError, NotFoundInPATH, DigestError, ContentLengthError, DownloadFailed, GPGError] DownloadFailed $ do
          let rep = fromMaybe "https://gitlab.haskell.org/ghc/ghc.git" repo
          lift $ logInfo $ "Fetching git repo " <> T.pack rep <> " at ref " <> T.pack ref <> " (this may take a while)"
          lEM $ git [ "init" ]
          lEM $ git [ "remote"
                    , "add"
                    , "origin"
                    , fromString rep ]

          -- figure out if we can do a shallow clone
          remoteBranches <- catchE @ProcessError @'[PatchFailed, ProcessError, NotFoundInPATH, DigestError, ContentLengthError, DownloadFailed, GPGError] @'[PatchFailed, NotFoundInPATH, DigestError, DownloadFailed, GPGError] (\(_ :: ProcessError) -> pure [])
              $ fmap processBranches $ gitOut ["ls-remote", "--heads", "origin"] (fromGHCupPath tmpUnpack)
          let shallow_clone
                | isCommitHash ref                     = True
                | fromString ref `elem` remoteBranches = True
                | otherwise                            = False
          lift $ logDebug $ "Shallow clone: " <> T.pack (show shallow_clone)

          -- fetch
          let fetch_args
                | shallow_clone = ["fetch", "--depth", "1", "--quiet", "origin", fromString ref]
                | otherwise     = ["fetch", "--tags",       "--quiet", "origin"                ]
          lEM $ git fetch_args

          -- initial checkout
          lEM $ git [ "checkout", fromString ref ]

          -- gather some info
          git_describe <- if shallow_clone
                          then pure Nothing
                          else fmap Just $ liftE $ gitOut ["describe", "--tags"] (fromGHCupPath tmpUnpack)
          chash <- liftE $ gitOut ["rev-parse", "HEAD" ] (fromGHCupPath tmpUnpack)
          branch <- liftE $ gitOut ["rev-parse", "--abbrev-ref", "HEAD" ] (fromGHCupPath tmpUnpack)

          -- clone submodules
          lEM $ git [ "submodule", "update", "--init", "--depth", "1" ]

          -- apply patches
          liftE $ applyAnyPatch patches (fromGHCupPath tmpUnpack)

          -- bootstrap
          tver <- liftE $ catchAllE @_ @'[ProcessError, ParseError, NotFoundInPATH] @'[] (\_ -> pure Nothing) $ fmap Just $ getGHCVer
            tmpUnpack
          liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)
          lift $ logInfo $ "Examining git ref " <> T.pack ref <> "\n  " <>
                           "GHC version (from Makefile): " <> T.pack (show (prettyVer <$> tver)) <>
                           (if not shallow_clone then "\n  " <> "'git describe' output: " <> fromJust git_describe else mempty) <>
                           (if isCommitHash ref then mempty else "\n  " <> "commit hash: " <> chash)
          liftIO $ threadDelay 5000000 -- give the user a sec to intervene

          ov <- case vps of
                  Just vps' -> fmap Just $ expandVersionPattern
                                             tver
                                             (take 7 $ T.unpack chash)
                                             (T.unpack chash)
                                             (maybe "" T.unpack git_describe)
                                             (T.unpack branch)
                                             vps'
                  Nothing -> pure Nothing

          pure (tver, ov)

        pure (tmpUnpack, tmpUnpack, GHCTargetVersion crossTarget <$> tver, ov)
    -- the version that's installed may differ from the
    -- compiled version, so the user can overwrite it
    installVer <- if | Just ov'   <- ov   -> pure (GHCTargetVersion crossTarget ov')
                     | Just tver' <- tver -> pure tver'
                     | otherwise          -> fail "No GHC version given and couldn't detect version. Giving up..."

    alreadyInstalled <- lift $ isInstalled ghc installVer
    alreadySet <- liftE $ isSet ghc installVer

    when alreadyInstalled $ do
      case installDir of
        IsolateDir isoDir ->
          lift $ logWarn $ "GHC " <> T.pack (prettyShow installVer) <> " already installed. Isolate installing to " <> T.pack isoDir
        GHCupInternal ->
          lift $ logWarn $ "GHC " <> T.pack (prettyShow installVer) <> " already installed. Will overwrite existing version."
      lift $ logWarn
        "...waiting for 10 seconds before continuing, you can still abort..."
      liftIO $ threadDelay 10000000 -- give the user a sec to intervene

    ghcdir <- case installDir of
      IsolateDir isoDir -> pure $ IsolateDirResolved isoDir
      GHCupInternal     -> GHCupDir <$> lift (ghcupGHCDir installVer)

    mBindist <- liftE $ runBuildAction
      tmpUnpack
      (do
        -- prefer 'tver', because the real version carries out compatibility checks
        -- we don't want the user to do funny things with it
        let doHadrian = Just <$> compileHadrianBindist (fromMaybe installVer tver) (fromGHCupPath workdir) ghcdir
            doMake    = compileMakeBindist (fromMaybe installVer tver) (fromGHCupPath workdir) ghcdir
        case buildSystem of
          Just Hadrian -> do
            lift $ logInfo "Requested to use Hadrian"
            liftE doHadrian
          Just Make -> do
            lift $ logInfo "Requested to use Make"
            doMake
          Nothing -> do
            supportsHadrian <- liftE $ catchE @HadrianNotFound @'[HadrianNotFound] @'[] (\_ -> return False)
                                 $ fmap (const True)
                                 $ findHadrianFile (fromGHCupPath workdir)
            if supportsHadrian
            then do
              lift $ logInfo "Detected Hadrian"
              liftE doHadrian
            else do
              lift $ logInfo "Detected Make"
              doMake
      )

    case installDir of
      GHCupInternal ->
        -- only remove old ghc in regular installs
        when alreadyInstalled $ do
          lift $ logInfo "Deleting existing installation"
          liftE $ rmToolVersion ghc installVer

      _ -> pure ()

    case mBindist of
      Just bindist -> do
        spec <- liftE $ compileInstallSpec bindist installVer
        cSize <- liftIO $ getFileSize bindist
        cDigest <- liftIO $ getFileDigest bindist
        liftE $ void $ installPackedBindist ghc bindist
                                 (DownloadInfo {
                                   _dlUri = "file:" <> T.pack bindist
                                 , _dlSubdir = Just $ RegexDir "ghc-.*"
                                 , _dlHash = cDigest
                                 , _dlCSize = Just cSize
                                 , _dlOutput = Nothing
                                 , _dlTag = Nothing
                                 , _dlInstallSpec = Just (toInstallationInputSpec spec)
                                 })
                                 ghcdir
                                 installVer
                                 False       -- not a force install, since we already overwrite when compiling.
                                 aargs
                                 installTargets
      -- for old Make cross installations we don't get a bindist,
      -- so this needs to be done manually
      Nothing -> do
        spec <- liftE $ compileInstallSpec' (fromInstallDir ghcdir </> "bin") installVer
        let dlInfo = (DownloadInfo {
                       _dlUri = ""
                     , _dlSubdir = Just $ RegexDir "ghc-.*"
                     , _dlHash = ""
                     , _dlCSize = Nothing
                     , _dlOutput = Nothing
                     , _dlTag = Nothing
                     , _dlInstallSpec = Just (toInstallationInputSpec spec)
                     })
        lift $ recordInstallationInfo ghcdir ghc installVer dlInfo spec

    case installDir of
      -- set and make symlinks for regular (non-isolated) installs
      GHCupInternal -> do
        symSpec <- liftE $ getSymlinkSpec' ghc installVer
        Dirs {..} <- lift getDirs
        liftE $ symlinkBinaries ghcdir symSpec (GHCupBinDir binDir) ghc installVer
        -- restore
        when alreadySet $ liftE $ void $ setToolVersion ghc installVer

      _ -> pure ()

    pure installVer

 where
  -- Infer the installation spec from the binary directory
  compileInstallSpec' ::
    ( MonadIOish m
    , MonadReader env m
    , HasPlatformReq env
    )
    => FilePath                   -- ^ Binary dir
    -> GHCTargetVersion
    -> Excepts '[ UnknownArchive
                , ArchiveResult
                ] m InstallationSpecResolved
  compileInstallSpec' bindir installVer = do
    pfreq <- lift getPlatformReq
    let pred' f = not (null f) && not (prettyVer (_tvVersion installVer) `T.isSuffixOf` T.pack f)
    files <- liftIO $ listDirectoryFiles bindir
    let binaries = filter pred' . fmap takeFileName $ files
        -- ghcup whereis picks the first binary, so we need to ensure 'ghc' is at the front
        binariesSorted = nub (maybe id (\(T.unpack -> t) a -> t <> "-" <> a) (_tvTarget installVer) "ghc" <.> exeExt:binaries)
    pure $ (defaultGHCInstallSpec pfreq installVer) { _isExeSymLinked = syml binariesSorted }
   where
    syml binaries =
      (\b -> SymlinkSpec (b <.> exeExt)
                         (takeFileName b <> "-${PKGVER}" <.> exeExt)
                         True
                         (Just $ takeFileName b <.> exeExt)
      ) . (\b -> "bin" Posix.</> b) <$> binaries

  -- Infer the installation spec from the tarball
  compileInstallSpec ::
    ( MonadIOish m
    , MonadReader env m
    , HasPlatformReq env
    )
    => FilePath                   -- ^ archive path
    -> GHCTargetVersion
    -> Excepts '[ UnknownArchive
                , ArchiveResult
                ] m InstallationSpecResolved
  compileInstallSpec tarball installVer = do
    pfreq <- lift getPlatformReq
    archiveFiles <- liftE $ getArchiveFiles tarball
    let pred' f = not (null f) && not (prettyVer (_tvVersion installVer) `T.isSuffixOf` T.pack f)
    let binaries = filter pred' . fmap takeFileName . filter ("*/bin/*" ?==) $ archiveFiles
        -- ghcup whereis picks the first binary, so we need to ensure 'ghc' is at the front
        binariesSorted = nub (maybe id (\(T.unpack -> t) a -> t <> "-" <> a) (_tvTarget installVer) "ghc" <.> exeExt:binaries)
    pure $ (defaultGHCInstallSpec pfreq installVer) { _isExeSymLinked = syml binariesSorted }
   where
    syml binaries =
      (\b -> SymlinkSpec (b <.> exeExt)
                         (takeFileName b <> "-${PKGVER}" <.> exeExt)
                         True
                         (Just $ takeFileName b <.> exeExt)
      ) . (\b -> "bin" Posix.</> b) <$> binaries

  getGHCVer :: ( MonadReader env m
               , HasSettings env
               , HasDirs env
               , HasLog env
               , MonadIOish m
               )
            => GHCupPath
            -> Excepts '[ProcessError, ParseError, NotFoundInPATH] m Version
  getGHCVer tmpUnpack = do
    liftE $ execWithWrapper "python3" ["./boot"] (Just $ fromGHCupPath tmpUnpack) "ghc-bootstrap" Nothing
    liftE $ configureWithGhcBoot Nothing [] (Just $ fromGHCupPath tmpUnpack) "ghc-bootstrap"
    let versionFile = fromGHCupPath tmpUnpack </> "VERSION"
    hasVersionFile <- liftIO $ doesFileExist versionFile
    if hasVersionFile
    then do
      lift $ logDebug "Detected VERSION file, trying to extract"
      contents <- liftIO $ readFile versionFile
      either (throwE . ParseError . show) pure . MP.parse version' "" . T.pack . stripNewlineEnd $ contents
    else do
      lift $ logDebug "Didn't detect VERSION file, trying to extract via legacy 'make'"
      CapturedProcess {..} <- lift $ makeOut
        ["show!", "--quiet", "VALUE=ProjectVersion" ] (Just $ fromGHCupPath tmpUnpack)
      case _exitCode of
        ExitSuccess -> either (throwE . ParseError . show) pure . MP.parse ghcProjectVersion "" . T.pack . stripNewlineEnd . T.unpack . decUTF8Safe' $ _stdOut
        ExitFailure c -> throwE $ NonZeroExit c "make" ["show!", "--quiet", "VALUE=ProjectVersion" ]

  defaultConf =
    let cross_mk = $(LitE . StringL <$> (qAddDependentFile "data/build_mk/cross" >> runIO (readFile "data/build_mk/cross")))
        default_mk = $(LitE . StringL <$> (qAddDependentFile "data/build_mk/default" >> runIO (readFile "data/build_mk/default")))
    in case crossTarget of
         Just _ -> cross_mk
         _      -> default_mk

  compileHadrianBindist :: ( MonadReader env m
                           , HasDirs env
                           , HasSettings env
                           , HasPlatformReq env
                           , HasLog env
                           , MonadIOish m
                           )
                        => GHCTargetVersion
                        -> FilePath
                        -> InstallDirResolved
                        -> Excepts
                             '[ FileDoesNotExistError
                              , HadrianNotFound
                              , InvalidBuildConfig
                              , PatchFailed
                              , ProcessError
                              , NotFoundInPATH
                              , CopyError]
                             m
                             FilePath  -- ^ output path of bindist
  compileHadrianBindist tver workdir ghcdir = do
    liftE $ configureBindist tver workdir ghcdir

    lift $ logInfo $ "Building GHC version " <> tVerToText tver <> " (this may take a while)..."
    hadrian_build <- liftE $ findHadrianFile workdir
    hEnv <- case hghc of
              Nothing -> pure Nothing
              Just hghc' -> do
                cEnv <- Map.fromList <$> liftIO getEnvironment
                ghc' <- liftE $ resolveGHC hghc'
                pure . Just . Map.toList . Map.insert "GHC" ghc' $ cEnv


    liftE $ execWithWrapper hadrian_build
                          ( maybe [] (\j  -> ["-j" <> show j]         ) jobs
                         ++ maybe [] (\bf -> ["--flavour=" <> bf]) buildFlavour
                         ++ ["binary-dist"]
                          )
                          (Just workdir) "ghc-make"
                          hEnv
    [tar] <- liftIO $ findFiles
      (workdir </> "_build" </> "bindist")
      (makeRegexOpts compExtended
                     execBlank
                     ([s|^ghc-.*\.tar\..*$|] :: ByteString)
      )
    liftE $ copyBindist tver tar (workdir </> "_build" </> "bindist")

  findHadrianFile :: (MonadIO m)
                  => FilePath
                  -> Excepts
                       '[HadrianNotFound]
                       m
                       FilePath
  findHadrianFile workdir = do
    let possible_files = if isWindows
                         then ((workdir </> "hadrian") </>) <$> ["build.bat"]
                         else ((workdir </> "hadrian") </>) <$> ["build", "build.sh"]
    exists <- forM possible_files (\f -> liftIO (doesFileExist f) <&> (,f))
    case filter fst exists of
      []         -> throwE HadrianNotFound
      ((_, x):_) -> pure x

  compileMakeBindist :: ( MonadReader env m
                        , HasDirs env
                        , HasSettings env
                        , HasPlatformReq env
                        , MonadThrow m
                        , MonadCatch m
                        , HasLog env
                        , MonadIO m
                        , MonadFail m
                        , MonadMask m
                        , MonadUnliftIO m
                        , MonadResource m
                        )
                     => GHCTargetVersion
                     -> FilePath
                     -> InstallDirResolved
                     -> Excepts
                          '[ FileDoesNotExistError
                           , HadrianNotFound
                           , InvalidBuildConfig
                           , PatchFailed
                           , ProcessError
                           , NotFoundInPATH
                           , MergeFileTreeError
                           , CopyError]
                          m
                       (Maybe FilePath)  -- ^ output path of bindist, None for cross
  compileMakeBindist tver workdir ghcdir = do
    liftE $ configureBindist tver workdir ghcdir

    case mbuildConfig of
      Just bc -> liftIOException
        doesNotExistErrorType
        (FileDoesNotExistError bc)
        (liftIO $ copyFile bc (build_mk workdir) False)
      Nothing ->
        liftIO $ T.writeFile (build_mk workdir) (addBuildFlavourToConf defaultConf)

    liftE $ checkBuildConfig (build_mk workdir)

    lift $ logInfo $ "Building GHC version " <> tVerToText tver <> " (this may take a while)..."
    liftE $ makeWithWrapper (maybe [] (\j -> ["-j" <> fS (show j)]) jobs) (Just workdir) "ghc-make" Nothing

    if | isCross tver -> do -- this is effectively legacy too (and does not install a .spec)
          lift $ logInfo "Installing cross toolchain..."
          tmpInstallDest <- lift withGHCupTmpDir
          liftE $ makeWithWrapper ["DESTDIR=" <> fromGHCupPath tmpInstallDest, "install"] (Just workdir) "ghc-make" Nothing
          liftE $ mergeGHCFileTree (tmpInstallDest `appendGHCupPath` dropDrive (fromInstallDir ghcdir)) ghcdir tver True
          pure Nothing
       | otherwise -> do
          lift $ logInfo "Creating bindist..."
          liftE $ makeWithWrapper ["binary-dist"] (Just workdir) "ghc-make" Nothing
          [tar] <- liftIO $ findFiles
            workdir
            (makeRegexOpts compExtended
                           execBlank
                           ([s|^ghc-.*\.tar\..*$|] :: ByteString)
            )
          liftE $ fmap Just $ copyBindist tver tar workdir

  build_mk workdir = workdir </> "mk" </> "build.mk"

  copyBindist :: ( MonadReader env m
                 , HasDirs env
                 , HasSettings env
                 , HasPlatformReq env
                 , MonadIO m
                 , MonadThrow m
                 , MonadCatch m
                 , HasLog env
                 )
              => GHCTargetVersion
              -> FilePath           -- ^ tar file
              -> FilePath           -- ^ workdir
              -> Excepts
                   '[CopyError]
                   m
                   FilePath
  copyBindist tver tar workdir = do
    Dirs {..} <- lift getDirs
    pfreq <- lift getPlatformReq
    c       <- liftIO $ BL.readFile (workdir </> tar)
    cDigest <-
      fmap (T.take 8)
      . lift
      . throwEither
      . E.decodeUtf8'
      . B16.encode
      . SHA256.hashlazy
      $ c
    cTime <- liftIO getCurrentTime
    let tarName = makeValid ("ghc-"
                            <> T.unpack (tVerToText tver)
                            <> "-"
                            <> pfReqToString pfreq
                            <> "-"
                            <> iso8601Show cTime
                            <> "-"
                            <> T.unpack cDigest
                            <> ".tar"
                            <> takeExtension tar)
    let tarPath = fromGHCupPath cacheDir </> tarName
    copyFileE (workdir </> tar) tarPath False
    lift $ logInfo $ "Copied bindist to " <> T.pack tarPath
    pure tarPath

  checkBuildConfig :: (MonadReader env m, MonadCatch m, MonadIO m, HasLog env)
                   => FilePath
                   -> Excepts
                        '[FileDoesNotExistError, InvalidBuildConfig]
                        m
                        ()
  checkBuildConfig bc = do
    c <- liftIOException
           doesNotExistErrorType
           (FileDoesNotExistError bc)
           (liftIO $ B.readFile bc)
    let lines' = fmap T.strip . T.lines $ decUTF8Safe c

   -- for cross, we need Stage1Only
    case crossTarget of
      Just _ -> when ("Stage1Only = YES" `notElem` lines') $ throwE
        (InvalidBuildConfig
          [s|Cross compiling needs to be a Stage1 build, add "Stage1Only = YES" to your config!|]
        )
      _ -> pure ()

    forM_ buildFlavour $ \bf ->
      when (T.pack ("BuildFlavour = " <> bf) `notElem` lines') $ do
        lift $ logWarn $ "Customly specified build config overwrites --flavour=" <> T.pack bf <> " switch! Waiting 5 seconds..."
        liftIO $ threadDelay 5000000

  addBuildFlavourToConf bc = case buildFlavour of
    Just bf -> "BuildFlavour = " <> T.pack bf <> "\n" <> bc
    Nothing -> bc

  isCross :: GHCTargetVersion -> Bool
  isCross = isJust . _tvTarget


  configureBindist :: ( MonadReader env m
                      , HasDirs env
                      , HasSettings env
                      , HasPlatformReq env
                      , HasLog env
                      , MonadIOish m
                      )
                   => GHCTargetVersion
                   -> FilePath
                   -> InstallDirResolved
                   -> Excepts
                        '[ FileDoesNotExistError
                         , InvalidBuildConfig
                         , PatchFailed
                         , ProcessError
                         , NotFoundInPATH
                         , CopyError
                         ]
                        m
                        ()
  configureBindist tver workdir (fromInstallDir -> ghcdir) = do
    PlatformRequest { .. } <- lift getPlatformReq
    lift $ logInfo [s|configuring build|]
    liftE $ configureWithGhcBoot (Just tver)
      (maybe mempty
                (\x -> ["--target=" <> T.unpack x])
                (_tvTarget tver)
      ++ ["--prefix=" <> ghcdir]
      ++ (if isWindows then ["--enable-tarballs-autodownload"] else [])
      -- https://github.com/haskell/ghcup-hs/issues/1032
      ++ ldOverride (_tvVersion tver) _rPlatform
      ++ aargs
      )
      (Just workdir)
      "ghc-conf"
    pure ()

  configureWithGhcBoot :: ( MonadReader env m
                          , HasSettings env
                          , HasDirs env
                          , HasLog env
                          , MonadIOish m
                          )
                       => Maybe GHCTargetVersion
                       -> [String]         -- ^ args for configure
                       -> Maybe FilePath   -- ^ optionally chdir into this
                       -> FilePath         -- ^ log filename (opened in append mode)
                       -> Excepts '[ProcessError, NotFoundInPATH] m ()
  configureWithGhcBoot mtver args dir logf = do
    bghc <- liftE $ resolveGHC bstrap
    let execNew = execWithWrapper
                    "sh"
                    ("./configure" : ("GHC=" <> bghc) : args)
                    dir
                    logf
                    Nothing
        execOld = execWithWrapper
                   "sh"
                   ("./configure" : ("--with-ghc=" <> bghc) : args)
                   dir
                   logf
                   Nothing
    if | Just tver <- mtver
       , _tvVersion tver >= [vver|8.8.0|] -> liftE execNew
       | Nothing   <- mtver               -> liftE execNew -- need some default for git checkouts where we don't know yet
       | otherwise                        -> liftE execOld

  resolveGHC :: MonadIO m => Either Version FilePath -> Excepts '[NotFoundInPATH] m FilePath
  resolveGHC = \case
           Right g    -> pure g
           Left  bver -> do
             let ghc' = "ghc-" <> (T.unpack . prettyVer $ bver) <> exeExt
             -- https://gitlab.haskell.org/ghc/ghc/-/issues/24682
             makeAbsolute ghc'




    -------------
    --[ Other ]--
    -------------


ldOverride ::  Version -> Platform -> [String]
ldOverride ver _plat
  | ver >= [vver|8.2.2|]
  = ["--disable-ld-override"]
  | otherwise
  = []

sanitizefGHCconfOptions :: MonadFail m => [String] -> m [String]
sanitizefGHCconfOptions args
  | "--prefix" `elem` fmap (takeWhile (/= '=')) args = fail "Don't explicitly set --prefix ...aborting"
  | otherwise = pure args

