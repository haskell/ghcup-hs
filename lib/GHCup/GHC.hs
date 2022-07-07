{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

{-|
Module      : GHCup.GHC
Description : GHCup installation functions for GHC
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.GHC where


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
import           GHCup.Prelude.Version.QQ
import           GHCup.Prelude.MegaParsec

import           Codec.Archive                  ( ArchiveResult )
import           Control.Applicative
import           Control.Concurrent             ( threadDelay )
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
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import           Data.Time.Clock
import           Data.Time.Format.ISO8601
import           Data.Versions                hiding ( patch )
import           GHC.IO.Exception
import           Haskus.Utils.Variant.Excepts
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( Quasi(qAddDependentFile) )
import           Optics
import           Prelude                 hiding ( abs
                                                , writeFile
                                                )
import           System.Environment
import           System.FilePath
import           System.IO.Error
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           Text.Regex.Posix
import           URI.ByteString

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as E
import qualified Text.Megaparsec               as MP


    ---------------------
    --[ Tool fetching ]--
    ---------------------



fetchGHCSrc :: ( MonadFail m
               , MonadMask m
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
               )
            => Version
            -> Maybe FilePath
            -> Excepts
                 '[ DigestError
                  , GPGError
                  , DownloadFailed
                  , NoDownload
                  ]
                 m
                 FilePath
fetchGHCSrc v mfp = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  dlInfo <-
    preview (ix GHC % ix v % viSourceDL % _Just) dls
      ?? NoDownload
  liftE $ downloadCached' dlInfo Nothing mfp



    -------------------------
    --[ Tool installation ]--
    -------------------------


-- | Like 'installGHCBin', except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installGHCBindist :: ( MonadFail m
                     , MonadMask m
                     , MonadCatch m
                     , MonadReader env m
                     , HasDirs env
                     , HasSettings env
                     , HasPlatformReq env
                     , HasLog env
                     , MonadResource m
                     , MonadIO m
                     , MonadUnliftIO m
                     )
                  => DownloadInfo    -- ^ where/how to download
                  -> Version         -- ^ the version to install
                  -> InstallDir
                  -> Bool            -- ^ Force install
                  -> [T.Text]        -- ^ additional configure args for bindist
                  -> Excepts
                       '[ AlreadyInstalled
                        , BuildFailed
                        , DigestError
                        , GPGError
                        , DownloadFailed
                        , NoDownload
                        , NotInstalled
                        , UnknownArchive
                        , TarDirDoesNotExist
                        , DirNotEmpty
                        , ArchiveResult
                        , ProcessError
                        , UninstallFailed
                        , MergeFileTreeError
                        ]
                       m
                       ()
installGHCBindist dlinfo ver installDir forceInstall addConfArgs = do
  let tver = mkTVer ver

  lift $ logDebug $ "Requested to install GHC with " <> prettyVer ver

  regularGHCInstalled <- lift $ ghcInstalled tver

  if
    | not forceInstall
    , regularGHCInstalled
    , GHCupInternal <- installDir -> do
        throwE $ AlreadyInstalled GHC ver

    | forceInstall
    , regularGHCInstalled
    , GHCupInternal <- installDir -> do
        lift $ logInfo "Removing the currently installed GHC version first!"
        liftE $ rmGHCVer tver

    | otherwise -> pure ()

  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing


  toolchainSanityChecks

  case installDir of
    IsolateDir isoDir -> do                        -- isolated install
      lift $ logInfo $ "isolated installing GHC to " <> T.pack isoDir
      liftE $ installPackedGHC dl (view dlSubdir dlinfo) (IsolateDirResolved isoDir) ver forceInstall addConfArgs
    GHCupInternal -> do                            -- regular install
      -- prepare paths
      ghcdir <- lift $ ghcupGHCDir tver

      liftE $ installPackedGHC dl (view dlSubdir dlinfo) (GHCupDir ghcdir) ver forceInstall addConfArgs

      -- make symlinks & stuff when regular install,
      liftE $ postGHCInstall tver

 where
  toolchainSanityChecks = do
    r <- forM ["CC", "LD"] (liftIO . lookupEnv)
    case catMaybes r of
      [] -> pure ()
      _ -> do
        lift $ logWarn $ "CC/LD environment variable is set. This will change the compiler/linker"
         <> "\n" <> "GHC uses internally and can cause defunct GHC in some cases (e.g. in Anaconda"
         <> "\n" <> "environments). If you encounter problems, unset CC and LD and reinstall."


-- | Install a packed GHC distribution. This only deals with unpacking and the GHC
-- build system and nothing else.
installPackedGHC :: ( MonadMask m
                    , MonadCatch m
                    , MonadReader env m
                    , HasDirs env
                    , HasPlatformReq env
                    , HasSettings env
                    , MonadThrow m
                    , HasLog env
                    , MonadIO m
                    , MonadUnliftIO m
                    , MonadFail m
                    , MonadResource m
                    )
                 => FilePath          -- ^ Path to the packed GHC bindist
                 -> Maybe TarDir      -- ^ Subdir of the archive
                 -> InstallDirResolved
                 -> Version           -- ^ The GHC version
                 -> Bool              -- ^ Force install
                 -> [T.Text]          -- ^ additional configure args for bindist
                 -> Excepts
                      '[ BuildFailed
                       , UnknownArchive
                       , TarDirDoesNotExist
                       , DirNotEmpty
                       , ArchiveResult
                       , ProcessError
                       , MergeFileTreeError
                       ] m ()
installPackedGHC dl msubdir inst ver forceInstall addConfArgs = do
  PlatformRequest {..} <- lift getPlatformReq

  unless forceInstall
    (liftE $ installDestSanityCheck inst)

  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack)
                   (liftE . intoSubdir tmpUnpack)
                   msubdir

  liftE $ runBuildAction tmpUnpack
                         (installUnpackedGHC workdir inst ver forceInstall addConfArgs)


-- | Install an unpacked GHC distribution. This only deals with the GHC
-- build system and nothing else.
installUnpackedGHC :: ( MonadReader env m
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
                   => GHCupPath           -- ^ Path to the unpacked GHC bindist (where the configure script resides)
                   -> InstallDirResolved  -- ^ Path to install to
                   -> Version             -- ^ The GHC version
                   -> Bool                -- ^ Force install
                   -> [T.Text]          -- ^ additional configure args for bindist
                   -> Excepts '[ProcessError, MergeFileTreeError] m ()
installUnpackedGHC path inst ver forceInstall addConfArgs
  | isWindows = do
      lift $ logInfo "Installing GHC (this may take a while)"
      -- Windows bindists are relocatable and don't need
      -- to run configure.
      -- We also must make sure to preserve mtime to not confuse ghc-pkg.
      liftE $ mergeFileTree path inst GHC (mkTVer ver) $ \source dest -> do
        mtime <- liftIO $ ifM (pathIsSymbolicLink source) (pure Nothing) (Just <$> getModificationTime source)
        when forceInstall $ hideError doesNotExistErrorType $ hideError InappropriateType $ recycleFile dest
        liftIO $ moveFilePortable source dest
        forM_ mtime $ liftIO . setModificationTime dest
  | otherwise = do
      PlatformRequest {..} <- lift getPlatformReq

      let alpineArgs
           | ver >= [vver|8.2.2|], Linux Alpine <- _rPlatform
           = ["--disable-ld-override"]
           | otherwise
           = []

      lift $ logInfo "Installing GHC (this may take a while)"
      lEM $ execLogged "sh"
                       ("./configure" : ("--prefix=" <> fromInstallDir inst)
                        : (alpineArgs <> (T.unpack <$> addConfArgs))
                       )
                       (Just $ fromGHCupPath path)
                       "ghc-configure"
                       Nothing
      tmpInstallDest <- lift withGHCupTmpDir
      lEM $ make ["DESTDIR=" <> fromGHCupPath tmpInstallDest, "install"] (Just $ fromGHCupPath path)
      liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpInstallDest)
      liftE $ mergeFileTree (tmpInstallDest `appendGHCupPath` dropDrive (fromInstallDir inst))
        inst
        GHC
        (mkTVer ver)
        (\f t -> liftIO $ do
            mtime <- ifM (pathIsSymbolicLink f) (pure Nothing) (Just <$> getModificationTime f)
            install f t (not forceInstall)
            forM_ mtime $ setModificationTime t)

      pure ()


-- | Installs GHC into @~\/.ghcup\/ghc/\<ver\>@ and places the
-- following symlinks in @~\/.ghcup\/bin@:
--
--   * @ghc-x.y.z -> ..\/ghc\/x.y.z\/bin/ghc@
--   * @ghc-x.y   -> ..\/ghc\/x.y.z\/bin/ghc@ (if x.y.z is the latest x.y version)
installGHCBin :: ( MonadFail m
                 , MonadMask m
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
                 )
              => Version         -- ^ the version to install
              -> InstallDir
              -> Bool            -- ^ force install
              -> [T.Text]        -- ^ additional configure args for bindist
              -> Excepts
                   '[ AlreadyInstalled
                    , BuildFailed
                    , DigestError
                    , GPGError
                    , DownloadFailed
                    , NoDownload
                    , NotInstalled
                    , UnknownArchive
                    , TarDirDoesNotExist
                    , DirNotEmpty
                    , ArchiveResult
                    , ProcessError
                    , UninstallFailed
                    , MergeFileTreeError
                    ]
                   m
                   ()
installGHCBin ver installDir forceInstall addConfArgs = do
  dlinfo <- liftE $ getDownloadInfo GHC ver
  liftE $ installGHCBindist dlinfo ver installDir forceInstall addConfArgs





    ---------------
    --[ Set GHC ]--
    ---------------



-- | Set GHC symlinks in @~\/.ghcup\/bin@ for the requested GHC version. The behavior depends
-- on `SetGHC`:
--
--   * SetGHCOnly: @~\/.ghcup\/bin\/ghc -> ~\/.ghcup\/ghc\/\<ver\>\/bin\/ghc@
--   * SetGHC_XY: @~\/.ghcup\/bin\/ghc-X.Y -> ~\/.ghcup\/ghc\/\<ver\>\/bin\/ghc@
--   * SetGHC_XYZ: @~\/.ghcup\/bin\/ghc-\<ver\> -> ~\/.ghcup\/ghc\/\<ver\>\/bin\/ghc@
--
-- Additionally creates a @~\/.ghcup\/share -> ~\/.ghcup\/ghc\/\<ver\>\/share symlink@
-- for 'SetGHCOnly' constructor.
setGHC :: ( MonadReader env m
          , HasDirs env
          , HasLog env
          , MonadThrow m
          , MonadFail m
          , MonadIO m
          , MonadCatch m
          , MonadMask m
          , MonadUnliftIO m
          )
       => GHCTargetVersion
       -> SetGHC
       -> Maybe FilePath  -- if set, signals that we're not operating in ~/.ghcup/bin
                          -- and don't want mess with other versions
       -> Excepts '[NotInstalled] m GHCTargetVersion
setGHC ver sghc mBinDir = do
  let verS = T.unpack $ prettyVer (_tvVersion ver)
  ghcdir                        <- lift $ ghcupGHCDir ver

  whenM (lift $ not <$> ghcInstalled ver) (throwE (NotInstalled GHC ver))

  -- symlink destination
  binDir <- case mBinDir of
    Just x -> pure x
    Nothing -> do
      Dirs {binDir = f} <- lift getDirs
      pure f

  -- first delete the old symlinks (this fixes compatibility issues
  -- with old ghcup)
  when (isNothing mBinDir) $
    case sghc of
      SetGHCOnly -> liftE $ rmPlainGHC (_tvTarget ver)
      SetGHC_XY  -> liftE $ rmMajorGHCSymlinks ver
      SetGHC_XYZ -> liftE $ rmMinorGHCSymlinks ver

  -- for ghc tools (ghc, ghci, haddock, ...)
  verfiles <- ghcToolFiles ver
  forM_ verfiles $ \file -> do
    mTargetFile <- case sghc of
      SetGHCOnly -> pure $ Just file
      SetGHC_XY  -> do
        handle
            (\(e :: ParseError) -> lift $ logWarn (T.pack $ displayException e) >> pure Nothing)
          $ do
            (mj, mi) <- getMajorMinorV (_tvVersion ver)
            let major' = intToText mj <> "." <> intToText mi
            pure $ Just (file <> "-" <> T.unpack major')
      SetGHC_XYZ ->
        pure $ Just (file <> "-" <> verS)

    -- create symlink
    forM_ mTargetFile $ \targetFile -> do
      bindir <- ghcInternalBinDir ver
      let fullF = binDir </> targetFile  <> exeExt
          fileWithExt = bindir </> file <> exeExt
      destL <- binarySymLinkDestination binDir fileWithExt
      lift $ createLink destL fullF

      when (targetFile == "ghc") $
        liftIO (isShadowed fullF) >>= \case
          Nothing -> pure ()
          Just pa -> lift $ logWarn $ T.pack $ prettyShow (ToolShadowed GHC pa fullF (_tvVersion ver))

  when (isNothing mBinDir) $ do
    -- create symlink for share dir
    when (isNothing . _tvTarget $ ver) $ lift $ symlinkShareDir (fromGHCupPath ghcdir) verS

    when (sghc == SetGHCOnly) $ lift warnAboutHlsCompatibility

  pure ver

 where

  symlinkShareDir :: ( MonadReader env m
                     , HasDirs env
                     , MonadIO m
                     , HasLog env
                     , MonadCatch m
                     , MonadMask m
                     )
                  => FilePath
                  -> String
                  -> m ()
  symlinkShareDir ghcdir ver' = do
    Dirs {..} <- getDirs
    let destdir = fromGHCupPath baseDir
    case sghc of
      SetGHCOnly -> do
        let sharedir     = "share"
        let fullsharedir = ghcdir </> sharedir
        logDebug $ "Checking for sharedir existence: " <> T.pack fullsharedir
        whenM (liftIO $ doesDirectoryExist fullsharedir) $ do
          let fullF   = destdir </> sharedir
          let targetF = "." </> "ghc" </> ver' </> sharedir
          logDebug $ "rm -f " <> T.pack fullF
          hideError doesNotExistErrorType $ rmDirectoryLink fullF
          logDebug $ "ln -s " <> T.pack targetF <> " " <> T.pack fullF

          if isWindows
          then liftIO
                 -- On windows we need to be more permissive
                 -- in case symlinks can't be created, be just
                 -- give up here. This symlink isn't strictly necessary.
                 $ hideError permissionErrorType
                 $ hideError illegalOperationErrorType
                 $ createDirectoryLink targetF fullF
          else liftIO
                 $ createDirectoryLink targetF fullF
      _ -> pure ()

unsetGHC :: ( MonadReader env m
            , HasDirs env
            , HasLog env
            , MonadThrow m
            , MonadFail m
            , MonadIO m
            , MonadMask m
            )
         => Maybe Text
         -> Excepts '[NotInstalled] m ()
unsetGHC = rmPlainGHC





    --------------
    --[ GHC rm ]--
    --------------


-- | Delete a ghc version and all its symlinks.
--
-- This may leave GHCup without a "set" version.
-- Will try to fix the ghc-x.y symlink after removal (e.g. to an
-- older version).
rmGHCVer :: ( MonadReader env m
            , HasDirs env
            , MonadThrow m
            , HasLog env
            , MonadIO m
            , MonadFail m
            , MonadCatch m
            , MonadMask m
            , MonadUnliftIO m
            )
         => GHCTargetVersion
         -> Excepts '[NotInstalled, UninstallFailed] m ()
rmGHCVer ver = do
  isSetGHC <- lift $ fmap (== Just ver) $ ghcSet (_tvTarget ver)

  whenM (lift $ fmap not $ ghcInstalled ver) (throwE (NotInstalled GHC ver))

  -- this isn't atomic, order matters
  when isSetGHC $ do
    lift $ logInfo "Removing ghc symlinks"
    liftE $ rmPlainGHC (_tvTarget ver)

  lift $ logInfo "Removing ghc-x.y.z symlinks"
  liftE $ rmMinorGHCSymlinks ver

  lift $ logInfo "Removing/rewiring ghc-x.y symlinks"
  -- first remove
  handle (\(_ :: ParseError) -> pure ()) $ liftE $ rmMajorGHCSymlinks ver
  -- then fix them (e.g. with an earlier version)

  dir' <- lift $ ghcupGHCDir ver
  let dir = fromGHCupPath dir'
  lift (getInstalledFiles GHC ver) >>= \case
    Just files -> do
      lift $ logInfo $ "Removing files safely from: " <> T.pack dir
      forM_ files (lift . hideError NoSuchThing . recycleFile . (\f -> dir </> dropDrive f))
      removeEmptyDirsRecursive dir
      survivors <- liftIO $ hideErrorDef [doesNotExistErrorType] [] $ listDirectory dir
      f <- recordedInstallationFile GHC ver
      lift $ recycleFile f
      when (not (null survivors)) $ throwE $ UninstallFailed dir survivors
    Nothing -> do
      isDir <- liftIO $ doesDirectoryExist dir
      isSyml <- liftIO $ handleIO (\_ -> pure False) $ pathIsSymbolicLink dir
      when (isDir && not isSyml) $ do
        lift $ logInfo $ "Removing legacy directory recursively: " <> T.pack dir
        recyclePathForcibly dir'

  v' <-
    handle
      (\(e :: ParseError) -> lift $ logWarn (T.pack $ displayException e) >> pure Nothing)
    $ fmap Just
    $ getMajorMinorV (_tvVersion ver)
  forM_ v' $ \(mj, mi) -> lift (getGHCForPVP (PVP (fromIntegral mj :| [fromIntegral mi])) (_tvTarget ver))
    >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY Nothing)

  Dirs {..} <- lift getDirs

  lift $ hideError doesNotExistErrorType $ rmDirectoryLink (fromGHCupPath baseDir </> "share")




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
           => Either GHCTargetVersion GitBranch          -- ^ version to install
           -> Maybe Version            -- ^ overwrite version
           -> Either Version FilePath  -- ^ version to bootstrap with
           -> Maybe Int                -- ^ jobs
           -> Maybe FilePath           -- ^ build config
           -> Maybe (Either FilePath [URI])  -- ^ patches
           -> [Text]                   -- ^ additional args to ./configure
           -> Maybe String             -- ^ build flavour
           -> Bool
           -> InstallDir
           -> Excepts
                '[ AlreadyInstalled
                 , BuildFailed
                 , DigestError
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
                 ]
                m
                GHCTargetVersion
compileGHC targetGhc ov bstrap jobs mbuildConfig patches aargs buildFlavour hadrian installDir
  = do
    PlatformRequest { .. } <- lift getPlatformReq
    GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo

    (workdir, tmpUnpack, tver) <- case targetGhc of
      -- unpack from version tarball
      Left tver -> do
        lift $ logDebug $ "Requested to compile: " <> tVerToText tver <> " with " <> either prettyVer T.pack bstrap

        -- download source tarball
        dlInfo <-
          preview (ix GHC % ix (tver ^. tvVersion) % viSourceDL % _Just) dls
            ?? NoDownload
        dl <- liftE $ downloadCached dlInfo Nothing

        -- unpack
        tmpUnpack <- lift mkGhcupTmpDir
        liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
        liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform $ fromGHCupPath tmpUnpack

        workdir <- maybe (pure tmpUnpack)
                         (liftE . intoSubdir tmpUnpack)
                         (view dlSubdir dlInfo)
        liftE $ applyAnyPatch patches (fromGHCupPath workdir)

        pure (workdir, tmpUnpack, tver)

      -- clone from git
      Right GitBranch{..} -> do
        tmpUnpack <- lift mkGhcupTmpDir
        let git args = execLogged "git" ("--no-pager":args) (Just $ fromGHCupPath tmpUnpack) "git" Nothing
        tver <- reThrowAll @_ @'[PatchFailed, ProcessError, NotFoundInPATH, DigestError, DownloadFailed, GPGError] DownloadFailed $ do
          let rep = fromMaybe "https://gitlab.haskell.org/ghc/ghc.git" repo
          lift $ logInfo $ "Fetching git repo " <> T.pack rep <> " at ref " <> T.pack ref <> " (this may take a while)"
          lEM $ git [ "init" ]
          lEM $ git [ "remote"
                    , "add"
                    , "origin"
                    , fromString rep ]

          -- figure out if we can do a shallow clone
          remoteBranches <- catchE @ProcessError @'[PatchFailed, ProcessError, NotFoundInPATH, DigestError, DownloadFailed, GPGError] @'[PatchFailed, NotFoundInPATH, DigestError, DownloadFailed, GPGError] (\(_ :: ProcessError) -> pure [])
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

          -- clone submodules
          lEM $ git [ "submodule", "update", "--init", "--depth", "1" ]

          -- apply patches
          liftE $ applyAnyPatch patches (fromGHCupPath tmpUnpack)

          -- bootstrap
          lEM $ execWithGhcEnv "python3" ["./boot"] (Just $ fromGHCupPath tmpUnpack) "ghc-bootstrap"
          lEM $ execWithGhcEnv "sh" ["./configure"] (Just $ fromGHCupPath tmpUnpack) "ghc-bootstrap"
          CapturedProcess {..} <- lift $ makeOut
            ["show!", "--quiet", "VALUE=ProjectVersion" ] (Just $ fromGHCupPath tmpUnpack)
          tver <- case _exitCode of
            ExitSuccess -> throwEither . MP.parse ghcProjectVersion "" . T.pack . stripNewlineEnd . T.unpack . decUTF8Safe' $ _stdOut
            ExitFailure c -> fail ("Could not figure out GHC project version. Exit code was: " <> show c <> ". Error was: " <> T.unpack (decUTF8Safe' _stdErr))

          liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)
          lift $ logInfo $ "Examining git ref " <> T.pack ref <> "\n  " <>
                           "GHC version (from Makefile): " <> prettyVer tver <>
                           (if not shallow_clone then ("\n  " <> "'git describe' output: " <> fromJust git_describe) else mempty) <>
                           (if isCommitHash ref then mempty else ("\n  " <> "commit hash: " <> chash))
          liftIO $ threadDelay 5000000 -- give the user a sec to intervene

          pure tver

        pure (tmpUnpack, tmpUnpack, GHCTargetVersion Nothing tver)
    -- the version that's installed may differ from the
    -- compiled version, so the user can overwrite it
    let installVer = maybe tver (\ov' -> tver { _tvVersion = ov' }) ov

    alreadyInstalled <- lift $ ghcInstalled installVer
    alreadySet <- fmap (== Just installVer) $ lift $ ghcSet (_tvTarget installVer)

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
      GHCupInternal -> GHCupDir <$> lift (ghcupGHCDir installVer)

    (mBindist, bmk) <- liftE $ runBuildAction
      tmpUnpack
      (do
        b <- if hadrian
             then compileHadrianBindist tver (fromGHCupPath workdir) ghcdir
             else compileMakeBindist tver (fromGHCupPath workdir) ghcdir
        bmk <- liftIO $ handleIO (\_ -> pure "") $ B.readFile (build_mk $ fromGHCupPath workdir)
        pure (b, bmk)
      )

    case installDir of
      GHCupInternal ->
        -- only remove old ghc in regular installs
        when alreadyInstalled $ do
          lift $ logInfo "Deleting existing installation"
          liftE $ rmGHCVer installVer

      _ -> pure ()

    forM_ mBindist $ \bindist -> do
      liftE $ installPackedGHC bindist
                               (Just $ RegexDir "ghc-.*")
                               ghcdir
                               (installVer ^. tvVersion)
                               False       -- not a force install, since we already overwrite when compiling.
                               []

    liftIO $ B.writeFile (fromInstallDir ghcdir </> ghcUpSrcBuiltFile) bmk

    case installDir of
      -- set and make symlinks for regular (non-isolated) installs
      GHCupInternal -> do
        reThrowAll GHCupSetError $ postGHCInstall installVer
        -- restore
        when alreadySet $ liftE $ void $ setGHC installVer SetGHCOnly Nothing

      _ -> pure ()

    pure installVer

 where
  defaultConf =
    let cross_mk = $(LitE . StringL <$> (qAddDependentFile "data/build_mk/cross" >> runIO (readFile "data/build_mk/cross")))
        default_mk = $(LitE . StringL <$> (qAddDependentFile "data/build_mk/default" >> runIO (readFile "data/build_mk/default")))
    in case targetGhc of
         Left (GHCTargetVersion (Just _) _) -> cross_mk
         _ -> default_mk

  compileHadrianBindist :: ( MonadReader env m
                           , HasDirs env
                           , HasSettings env
                           , HasPlatformReq env
                           , MonadThrow m
                           , MonadCatch m
                           , HasLog env
                           , MonadIO m
                           , MonadFail m
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
                             (Maybe FilePath)  -- ^ output path of bindist, None for cross
  compileHadrianBindist tver workdir ghcdir = do
    lEM $ execWithGhcEnv "python3" ["./boot"] (Just workdir) "ghc-bootstrap"

    liftE $ configureBindist tver workdir ghcdir

    lift $ logInfo "Building (this may take a while)..."
    hadrian_build <- liftE $ findHadrianFile workdir
    lEM $ execWithGhcEnv hadrian_build
                          ( maybe [] (\j  -> ["-j" <> show j]         ) jobs
                         ++ maybe [] (\bf -> ["--flavour=" <> bf]) buildFlavour
                         ++ ["binary-dist"]
                          )
                          (Just workdir) "ghc-make"
    [tar] <- liftIO $ findFiles
      (workdir </> "_build" </> "bindist")
      (makeRegexOpts compExtended
                     execBlank
                     ([s|^ghc-.*\.tar\..*$|] :: ByteString)
      )
    liftE $ fmap Just $ copyBindist tver tar (workdir </> "_build" </> "bindist")

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
    exsists <- forM possible_files (\f -> liftIO (doesFileExist f) <&> (,f))
    case filter fst exsists of
      [] -> throwE HadrianNotFound
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

    lift $ logInfo "Building (this may take a while)..."
    lEM $ make (maybe [] (\j -> ["-j" <> fS (show j)]) jobs) (Just workdir)

    if | isCross tver -> do
          lift $ logInfo "Installing cross toolchain..."
          lEM $ make ["install"] (Just workdir)
          pure Nothing
       | otherwise -> do
          lift $ logInfo "Creating bindist..."
          lEM $ make ["binary-dist"] (Just workdir)
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
    case targetGhc of
      Left (GHCTargetVersion (Just _) _) -> when ("Stage1Only = YES" `notElem` lines') $ throwE
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
                      , MonadThrow m
                      , MonadCatch m
                      , HasLog env
                      , MonadIO m
                      , MonadFail m
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
    lift $ logInfo [s|configuring build|]

    if | _tvVersion tver >= [vver|8.8.0|] -> do
          lEM $ execWithGhcEnv
            "sh"
            ("./configure" :  maybe mempty
                      (\x -> ["--target=" <> T.unpack x])
                      (_tvTarget tver)
            ++ ["--prefix=" <> ghcdir]
            ++ (if isWindows then ["--enable-tarballs-autodownload"] else [])
            ++ fmap T.unpack aargs
            )
            (Just workdir)
            "ghc-conf"
       | otherwise -> do
        lEM $ execLogged
          "sh"
          (  [ "./configure", "--with-ghc=" <> either id id bghc
             ]
          ++ maybe mempty
                   (\x -> ["--target=" <> T.unpack x])
                   (_tvTarget tver)
          ++ ["--prefix=" <> ghcdir]
          ++ (if isWindows then ["--enable-tarballs-autodownload"] else [])
          ++ fmap T.unpack aargs
          )
          (Just workdir)
          "ghc-conf"
          Nothing
    pure ()

  execWithGhcEnv :: ( MonadReader env m
                    , HasSettings env
                    , HasDirs env
                    , HasLog env
                    , MonadIO m
                    , MonadThrow m)
                 => FilePath         -- ^ thing to execute
                 -> [String]         -- ^ args for the thing
                 -> Maybe FilePath   -- ^ optionally chdir into this
                 -> FilePath         -- ^ log filename (opened in append mode)
                 -> m (Either ProcessError ())
  execWithGhcEnv fp args dir logf = do
    env <- ghcEnv
    execLogged fp args dir logf (Just env)

  bghc = case bstrap of
           Right g    -> Right g
           Left  bver -> Left ("ghc-" <> (T.unpack . prettyVer $ bver) <> exeExt)

  ghcEnv :: (MonadThrow m, MonadIO m) => m [(String, String)]
  ghcEnv = do
    cEnv <- liftIO getEnvironment
    bghcPath <- case bghc of
      Right ghc' -> pure ghc'
      Left  bver -> do
        spaths <- liftIO getSearchPath
        throwMaybeM (NotFoundInPATH bver) $ liftIO (searchPath spaths bver)
    pure (("GHC", bghcPath) : cEnv)




    -------------
    --[ Other ]--
    -------------



-- | Creates @ghc-x.y.z@ and @ghc-x.y@ symlinks. This is used for
-- both installing from source and bindist.
postGHCInstall :: ( MonadReader env m
                  , HasDirs env
                  , HasLog env
                  , MonadThrow m
                  , MonadFail m
                  , MonadIO m
                  , MonadCatch m
                  , MonadMask m
                  , MonadUnliftIO m
                  )
               => GHCTargetVersion
               -> Excepts '[NotInstalled] m ()
postGHCInstall ver@GHCTargetVersion {..} = do
  void $ liftE $ setGHC ver SetGHC_XYZ Nothing

  -- Create ghc-x.y symlinks. This may not be the current
  -- version, create it regardless.
  v' <-
    handle (\(e :: ParseError) -> lift $ logWarn (T.pack $ displayException e) >> pure Nothing)
    $ fmap Just
    $ getMajorMinorV _tvVersion
  forM_ v' $ \(mj, mi) -> lift (getGHCForPVP (PVP (fromIntegral mj :| [fromIntegral mi])) _tvTarget)
    >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY Nothing)

