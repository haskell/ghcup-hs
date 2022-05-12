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
Module      : GHCup
Description : GHCup installation functions
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

This module contains the main functions that correspond
to the command line interface, like installation, listing versions
and so on.

These are the entry points.
-}
module GHCup where


import           GHCup.Download
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Utils.File
import           GHCup.Utils.Logger
import           GHCup.Utils.Prelude
import           GHCup.Utils.String.QQ
import           GHCup.Utils.Version.QQ
import           GHCup.Version

import           Codec.Archive                  ( ArchiveResult )
import           Control.Applicative
import           Control.DeepSeq                ( force )
import           Control.Exception              ( evaluate )
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO( withRunInIO ) )
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
import           Distribution.Types.Version   hiding ( Version )
import           Distribution.Types.PackageId
import           Distribution.Types.PackageDescription
import           Distribution.Types.GenericPackageDescription
import           Distribution.PackageDescription.Parsec
import           GHC.IO.Exception
import           Haskus.Utils.Variant.Excepts
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( Quasi(qAddDependentFile) )
import           Optics
import           Prelude                 hiding ( abs
                                                , writeFile
                                                )
import           Safe                    hiding ( at )
import           System.Directory        hiding ( findFiles, copyFile )
import           System.Environment
import           System.FilePath
import           System.IO.Error
import           System.IO.Temp
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           Text.Regex.Posix
import           URI.ByteString

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.List.NonEmpty            as NE
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as E
import qualified Text.Megaparsec               as MP
import GHCup.Utils.MegaParsec
import Control.Concurrent (threadDelay)


    ---------------------
    --[ Tool fetching ]--
    ---------------------


fetchToolBindist :: ( MonadFail m
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
                 -> Tool
                 -> Maybe FilePath
                 -> Excepts
                      '[ DigestError
                       , GPGError
                       , DownloadFailed
                       , NoDownload
                       ]
                      m
                      FilePath
fetchToolBindist v t mfp = do
  dlinfo <- liftE $ getDownloadInfo t v
  liftE $ downloadCached' dlinfo Nothing mfp


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
                        ]
                       m
                       ()
installGHCBindist dlinfo ver installDir forceInstall = do
  let tver = mkTVer ver

  lift $ logDebug $ "Requested to install GHC with " <> prettyVer ver

  regularGHCInstalled <- lift $ checkIfToolInstalled GHC ver

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
      liftE $ installPackedGHC dl (view dlSubdir dlinfo) (IsolateDirResolved isoDir) ver forceInstall
    GHCupInternal -> do                            -- regular install
      -- prepare paths
      ghcdir <- lift $ ghcupGHCDir tver

      liftE $ installPackedGHC dl (view dlSubdir dlinfo) (GHCupDir ghcdir) ver forceInstall

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
                 -> Excepts
                      '[ BuildFailed
                       , UnknownArchive
                       , TarDirDoesNotExist
                       , DirNotEmpty
                       , ArchiveResult
                       , ProcessError
                       ] m ()
installPackedGHC dl msubdir inst ver forceInstall = do
  PlatformRequest {..} <- lift getPlatformReq

  unless forceInstall
    (liftE $ installDestSanityCheck inst)

  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ cleanUpOnError tmpUnpack (unpackToDir tmpUnpack dl)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack)
                   (liftE . intoSubdir tmpUnpack)
                   msubdir

  liftE $ runBuildAction tmpUnpack
                         (installUnpackedGHC workdir inst ver forceInstall)


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
                   => FilePath            -- ^ Path to the unpacked GHC bindist (where the configure script resides)
                   -> InstallDirResolved  -- ^ Path to install to
                   -> Version             -- ^ The GHC version
                   -> Bool                -- ^ Force install
                   -> Excepts '[ProcessError] m ()
installUnpackedGHC path inst ver forceInstall
  | isWindows = do
      lift $ logInfo "Installing GHC (this may take a while)"
      -- Windows bindists are relocatable and don't need
      -- to run configure.
      -- We also must make sure to preserve mtime to not confuse ghc-pkg.
      fs <- lift $ withRunInIO $ \_ -> mergeFileTreeAll path (fromInstallDir inst) $ \source dest -> do
        mtime <- getModificationTime source
        moveFilePortable source dest
        setModificationTime dest mtime
      case inst of
        IsolateDirResolved _ -> pure ()
        _ -> recordInstalledFiles fs GHC (mkTVer ver)
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
                        : alpineArgs
                       )
                       (Just path)
                       "ghc-configure"
                       Nothing
      tmpInstallDest <- lift withGHCupTmpDir
      lEM $ make ["DESTDIR=" <> tmpInstallDest, "install"] (Just path)
      lift $ logInfo $ "Merging file tree from \"" <> T.pack tmpInstallDest <> "\" to \"" <> T.pack (fromInstallDir inst) <> "\""
      fs <- mergeFileTreeAll (tmpInstallDest </> dropDrive (fromInstallDir inst))
        (fromInstallDir inst)
        (\f t -> liftIO $ install f t (not forceInstall))
      case inst of
        IsolateDirResolved _ -> pure ()
        _ -> recordInstalledFiles fs GHC (mkTVer ver)
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
                    ]
                   m
                   ()
installGHCBin ver installDir forceInstall = do
  dlinfo <- liftE $ getDownloadInfo GHC ver
  liftE $ installGHCBindist dlinfo ver installDir forceInstall


-- | Like 'installCabalBin', except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installCabalBindist :: ( MonadMask m
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
                    -> InstallDir
                    -> Bool           -- ^ Force install
                    -> Excepts
                         '[ AlreadyInstalled
                          , CopyError
                          , DigestError
                          , GPGError
                          , DownloadFailed
                          , NoDownload
                          , NotInstalled
                          , UnknownArchive
                          , TarDirDoesNotExist
                          , ArchiveResult
                          , FileAlreadyExistsError
                          ]
                         m
                         ()
installCabalBindist dlinfo ver installDir forceInstall = do
  lift $ logDebug $ "Requested to install cabal version " <> prettyVer ver

  PlatformRequest {..} <- lift getPlatformReq
  Dirs {..} <- lift getDirs

  -- check if we already have a regular cabal already installed
  regularCabalInstalled <- lift $ checkIfToolInstalled Cabal ver

  if
    | not forceInstall
    , regularCabalInstalled
    , GHCupInternal <- installDir -> do
        throwE $ AlreadyInstalled Cabal ver

    | forceInstall
    , regularCabalInstalled
    , GHCupInternal <- installDir -> do
        lift $ logInfo "Removing the currently installed version first!"
        liftE $ rmCabalVer ver

    | otherwise -> pure ()


  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack <- lift withGHCupTmpDir
  liftE $ cleanUpOnError tmpUnpack (unpackToDir tmpUnpack dl)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)

  case installDir of
    IsolateDir isoDir -> do             -- isolated install
      lift $ logInfo $ "isolated installing Cabal to " <> T.pack isoDir
      liftE $ installCabalUnpacked workdir (IsolateDirResolved isoDir) ver forceInstall

    GHCupInternal -> do                 -- regular install
      liftE $ installCabalUnpacked workdir (GHCupDir binDir) ver forceInstall


-- | Install an unpacked cabal distribution.Symbol
installCabalUnpacked :: (MonadCatch m, HasLog env, MonadIO m, MonadReader env m)
              => FilePath      -- ^ Path to the unpacked cabal bindist (where the executable resides)
              -> InstallDirResolved      -- ^ Path to install to
              -> Version
              -> Bool          -- ^ Force Install
              -> Excepts '[CopyError, FileAlreadyExistsError] m ()
installCabalUnpacked path inst ver forceInstall = do
  lift $ logInfo "Installing cabal"
  let cabalFile = "cabal"
  liftIO $ createDirRecursive' (fromInstallDir inst)
  let destFileName = cabalFile
        <> (case inst of
              IsolateDirResolved _ -> ""
              GHCupDir _ -> ("-" <>) . T.unpack . prettyVer $ ver
           )
        <> exeExt
  let destPath = fromInstallDir inst </> destFileName

  copyFileE
    (path </> cabalFile <> exeExt)
    destPath
    (not forceInstall)
  lift $ chmod_755 destPath

-- | Installs cabal into @~\/.ghcup\/bin/cabal-\<ver\>@ and
-- creates a default @cabal -> cabal-x.y.z.q@ symlink for
-- the latest installed version.
installCabalBin :: ( MonadMask m
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
                -> Bool           -- force install
                -> Excepts
                     '[ AlreadyInstalled
                      , CopyError
                      , DigestError
                      , GPGError
                      , DownloadFailed
                      , NoDownload
                      , NotInstalled
                      , UnknownArchive
                      , TarDirDoesNotExist
                      , ArchiveResult
                      , FileAlreadyExistsError
                      ]
                     m
                     ()
installCabalBin ver installDir forceInstall = do
  dlinfo <- liftE $ getDownloadInfo Cabal ver
  installCabalBindist dlinfo ver installDir forceInstall


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
                        ]
                       m
                       ()
installHLSBindist dlinfo ver installDir forceInstall = do
  lift $ logDebug $ "Requested to install hls version " <> prettyVer ver

  PlatformRequest {..} <- lift getPlatformReq
  Dirs {..} <- lift getDirs

  regularHLSInstalled <- lift $ checkIfToolInstalled HLS ver

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
  liftE $ cleanUpOnError tmpUnpack (unpackToDir tmpUnpack dl)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)
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
      then liftE $ installHLSUnpackedLegacy workdir (GHCupDir binDir) ver forceInstall
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
                      )
                   => FilePath      -- ^ Path to the unpacked hls bindist (where the executable resides)
                   -> InstallDirResolved      -- ^ Path to install to
                   -> Version
                   -> Bool
                   -> Excepts '[ProcessError, CopyError, FileAlreadyExistsError, NotInstalled] m ()
installHLSUnpacked path inst ver forceInstall = do
  lift $ logInfo "Installing HLS"
  tmpInstallDest <- lift withGHCupTmpDir
  lEM $ make ["DESTDIR=" <> tmpInstallDest, "PREFIX=" <> fromInstallDir inst, "install"] (Just path)
  fs <- mergeFileTreeAll (tmpInstallDest </> dropDrive (fromInstallDir inst))
                   (fromInstallDir inst)
                   (\f t -> liftIO $ install f t (not forceInstall))
  case inst of
    IsolateDirResolved _ -> pure ()
    _ -> recordInstalledFiles fs HLS (mkTVer ver)

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
                   GHCupDir _ -> ("~" <>) . T.unpack . prettyVer $ ver
                 )
              <> exeExt

    let srcPath = path </> f
    let destPath = fromInstallDir installDir </> toF

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
                 GHCupDir _ -> ("-" <>) . T.unpack . prettyVer $ ver
               )
            <> exeExt
      srcWrapperPath = path </> wrapper <> exeExt
      destWrapperPath = fromInstallDir installDir </> toF

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
           => Either Version GitBranch
           -> [Version]
           -> Maybe Int
           -> Maybe Version
           -> InstallDir
           -> Maybe (Either FilePath URI)
           -> Maybe URI
           -> Maybe (Either FilePath [URI])  -- ^ patches
           -> [Text]                   -- ^ additional args to cabal install
           -> Excepts '[ NoDownload
                       , GPGError
                       , DownloadFailed
                       , DigestError
                       , UnknownArchive
                       , TarDirDoesNotExist
                       , ArchiveResult
                       , BuildFailed
                       , NotInstalled
                       ] m Version
compileHLS targetHLS ghcs jobs ov installDir cabalProject cabalProjectLocal patches cabalArgs = do
  PlatformRequest { .. } <- lift getPlatformReq
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  Dirs { .. } <- lift getDirs


  (workdir, tver) <- case targetHLS of
    -- unpack from version tarball
    Left tver -> do
      lift $ logDebug $ "Requested to compile: " <> prettyVer tver

      -- download source tarball
      dlInfo <-
        preview (ix HLS % ix tver % viSourceDL % _Just) dls
          ?? NoDownload
      dl <- liftE $ downloadCached dlInfo Nothing

      -- unpack
      tmpUnpack <- lift mkGhcupTmpDir
      liftE $ cleanUpOnError tmpUnpack (unpackToDir tmpUnpack dl)
      liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

      workdir <- maybe (pure tmpUnpack)
                       (liftE . intoSubdir tmpUnpack)
                       (view dlSubdir dlInfo)

      pure (workdir, tver)

    -- clone from git
    Right GitBranch{..} -> do
      tmpUnpack <- lift mkGhcupTmpDir
      let git args = execLogged "git" ("--no-pager":args) (Just tmpUnpack) "git" Nothing
      tver <- reThrowAll @_ @'[ProcessError] DownloadFailed $ do
        let rep = fromMaybe "https://github.com/haskell/haskell-language-server.git" repo
        lift $ logInfo $ "Fetching git repo " <> T.pack rep <> " at ref " <> T.pack ref <> " (this may take a while)"
        lEM $ git [ "init" ]
        lEM $ git [ "remote"
                  , "add"
                  , "origin"
                  , fromString rep ]

        let fetch_args =
                  [ "fetch"
                  , "--depth"
                  , "1"
                  , "--quiet"
                  , "origin"
                  , fromString ref ]
        lEM $ git fetch_args

        lEM $ git [ "checkout", "FETCH_HEAD" ]
        (Just gpd) <- parseGenericPackageDescriptionMaybe <$> liftIO (B.readFile (tmpUnpack </> "haskell-language-server.cabal"))
        pure . (\c -> Version Nothing c [] Nothing)
          . NE.fromList . fmap (NE.fromList . (:[]) . digits . fromIntegral)
          . versionNumbers
          . pkgVersion
          . package
          . packageDescription
          $ gpd

      liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack
      lift $ logInfo $ "Git version " <> T.pack ref <> " corresponds to HLS version " <> prettyVer tver

      pure (tmpUnpack, tver)

  -- the version that's installed may differ from the
  -- compiled version, so the user can overwrite it
  let installVer = fromMaybe tver ov

  liftE $ runBuildAction
    workdir
    (reThrowAll @_ @'[GPGError, DownloadFailed, DigestError, PatchFailed, ProcessError, FileAlreadyExistsError, CopyError] @'[BuildFailed] (BuildFailed workdir) $ do
      let tmpInstallDir = workdir </> "out"
      liftIO $ createDirRecursive' tmpInstallDir

      -- apply patches
      liftE $ applyAnyPatch patches workdir

      -- set up project files
      cp <- case cabalProject of
        Just (Left cp)
          | isAbsolute cp -> do
              copyFileE cp (workdir </> "cabal.project") False
              pure "cabal.project"
          | otherwise -> pure (takeFileName cp)
        Just (Right uri) -> do
          tmpUnpack <- lift withGHCupTmpDir
          cp <- liftE $ download uri Nothing Nothing tmpUnpack (Just "cabal.project") False
          copyFileE cp (workdir </> "cabal.project") False
          pure "cabal.project"
        Nothing -> pure "cabal.project"
      forM_ cabalProjectLocal $ \uri -> do
        tmpUnpack <- lift withGHCupTmpDir
        cpl <- liftE $ download uri Nothing Nothing tmpUnpack (Just (cp <.> "local")) False
        copyFileE cpl (workdir </> cp <.> "local") False
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
          (Just workdir) "cabal" Nothing
        pure ghcInstallDir

      forM_ artifacts $ \artifact -> do
        liftIO $ renameFile (artifact </> "haskell-language-server" <.> exeExt)
          (tmpInstallDir </> "haskell-language-server-" <> takeFileName artifact <.> exeExt)
        liftIO $ renameFile (artifact </> "haskell-language-server-wrapper" <.> exeExt)
          (tmpInstallDir </> "haskell-language-server-wrapper" <.> exeExt)
        liftIO $ rmPathForcibly artifact

      case installDir of
        IsolateDir isoDir -> do
          lift $ logInfo $ "isolated installing HLS to " <> T.pack isoDir
          liftE $ installHLSUnpackedLegacy tmpInstallDir (IsolateDirResolved isoDir) installVer True
        GHCupInternal -> do
          liftE $ installHLSUnpackedLegacy tmpInstallDir (GHCupDir binDir) installVer True
    )

  pure installVer



-- | Installs stack into @~\/.ghcup\/bin/stack-\<ver\>@ and
-- creates a default @stack -> stack-x.y.z.q@ symlink for
-- the latest installed version.
installStackBin :: ( MonadMask m
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
                => Version
                -> InstallDir
                -> Bool            -- ^ Force install
                -> Excepts
                     '[ AlreadyInstalled
                      , CopyError
                      , DigestError
                      , GPGError
                      , DownloadFailed
                      , NoDownload
                      , NotInstalled
                      , UnknownArchive
                      , TarDirDoesNotExist
                      , ArchiveResult
                      , FileAlreadyExistsError
                      ]
                     m
                     ()
installStackBin ver installDir forceInstall = do
  dlinfo <- liftE $ getDownloadInfo Stack ver
  installStackBindist dlinfo ver installDir forceInstall


-- | Like 'installStackBin', except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installStackBindist :: ( MonadMask m
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
                    -> InstallDir
                    -> Bool           -- ^ Force install
                    -> Excepts
                         '[ AlreadyInstalled
                          , CopyError
                          , DigestError
                          , GPGError
                          , DownloadFailed
                          , NoDownload
                          , NotInstalled
                          , UnknownArchive
                          , TarDirDoesNotExist
                          , ArchiveResult
                          , FileAlreadyExistsError
                          ]
                         m
                         ()
installStackBindist dlinfo ver installDir forceInstall = do
  lift $ logDebug $ "Requested to install stack version " <> prettyVer ver

  PlatformRequest {..} <- lift getPlatformReq
  Dirs {..} <- lift getDirs

  regularStackInstalled <- lift $ checkIfToolInstalled Stack ver

  if
    | not forceInstall
    , regularStackInstalled
    , GHCupInternal <- installDir -> do
        throwE $ AlreadyInstalled Stack ver

    | forceInstall
    , regularStackInstalled
    , GHCupInternal <- installDir -> do
        lift $ logInfo "Removing the currently installed version of Stack first!"
        liftE $ rmStackVer ver

    | otherwise -> pure ()

  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack <- lift withGHCupTmpDir
  liftE $ cleanUpOnError tmpUnpack (unpackToDir tmpUnpack dl)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)

  case installDir of
    IsolateDir isoDir -> do                 -- isolated install
      lift $ logInfo $ "isolated installing Stack to " <> T.pack isoDir
      liftE $ installStackUnpacked workdir (IsolateDirResolved isoDir) ver forceInstall
    GHCupInternal -> do                     -- regular install
      liftE $ installStackUnpacked workdir (GHCupDir binDir) ver forceInstall


-- | Install an unpacked stack distribution.
installStackUnpacked :: (MonadReader env m, HasLog env, MonadCatch m, MonadIO m)
              => FilePath      -- ^ Path to the unpacked stack bindist (where the executable resides)
              -> InstallDirResolved
              -> Version
              -> Bool          -- ^ Force install
              -> Excepts '[CopyError, FileAlreadyExistsError] m ()
installStackUnpacked path installDir ver forceInstall = do
  lift $ logInfo "Installing stack"
  let stackFile = "stack"
  liftIO $ createDirRecursive' (fromInstallDir installDir)
  let destFileName = stackFile
                     <> (case installDir of
                          IsolateDirResolved _ -> ""
                          GHCupDir _ -> ("-" <>) .  T.unpack . prettyVer $ ver
                        )
                     <> exeExt
      destPath = fromInstallDir installDir </> destFileName

  copyFileE
    (path </> stackFile <> exeExt)
    destPath
    (not forceInstall)
  lift $ chmod_755 destPath


    ---------------------
    --[ Set GHC/cabal ]--
    ---------------------



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

  when (isNothing mBinDir) $ do
    -- create symlink for share dir
    when (isNothing . _tvTarget $ ver) $ lift $ symlinkShareDir ghcdir verS

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
    let destdir = baseDir
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


-- | Set the @~\/.ghcup\/bin\/cabal@ symlink.
setCabal :: ( MonadMask m
            , MonadReader env m
            , HasDirs env
            , HasLog env
            , MonadFail m
            , MonadIO m
            , MonadUnliftIO m)
         => Version
         -> Excepts '[NotInstalled] m ()
setCabal ver = do
  let targetFile = "cabal-" <> T.unpack (prettyVer ver) <> exeExt

  -- symlink destination
  Dirs {..} <- lift getDirs

  whenM (liftIO $ not <$> doesFileExist (binDir </> targetFile))
    $ throwE
    $ NotInstalled Cabal (GHCTargetVersion Nothing ver)

  let cabalbin = binDir </> "cabal" <> exeExt

  -- create link
  let destL = targetFile
  lift $ createLink destL cabalbin

  pure ()

unsetCabal :: ( MonadMask m
              , MonadReader env m
              , HasDirs env
              , MonadIO m)
           => m ()
unsetCabal = do
  Dirs {..} <- getDirs
  let cabalbin = binDir </> "cabal" <> exeExt
  hideError doesNotExistErrorType $ rmLink cabalbin


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


-- | Set the @~\/.ghcup\/bin\/stack@ symlink.
setStack :: ( MonadMask m
            , MonadReader env m
            , HasDirs env
            , HasLog env
            , MonadThrow m
            , MonadFail m
            , MonadIO m
            , MonadUnliftIO m
            )
         => Version
         -> Excepts '[NotInstalled] m ()
setStack ver = do
  let targetFile = "stack-" <> T.unpack (prettyVer ver) <> exeExt

  -- symlink destination
  Dirs {..} <- lift getDirs

  whenM (liftIO $ not <$> doesFileExist (binDir </> targetFile))
    $ throwE
    $ NotInstalled Stack (GHCTargetVersion Nothing ver)

  let stackbin = binDir </> "stack" <> exeExt

  lift $ createLink targetFile stackbin

  pure ()


unsetStack :: ( MonadMask m
              , MonadReader env m
              , HasDirs env
              , MonadIO m)
           => m ()
unsetStack = do
  Dirs {..} <- getDirs
  let stackbin = binDir </> "stack" <> exeExt
  hideError doesNotExistErrorType $ rmLink stackbin


-- | Warn if the installed and set HLS is not compatible with the installed and
-- set GHC version.
warnAboutHlsCompatibility :: ( MonadReader env m
                             , HasDirs env
                             , HasLog env
                             , MonadThrow m
                             , MonadCatch m
                             , MonadIO m
                             )
                          => m ()
warnAboutHlsCompatibility = do
  supportedGHC <- hlsGHCVersions
  currentGHC   <- fmap _tvVersion <$> ghcSet Nothing
  currentHLS   <- hlsSet

  case (currentGHC, currentHLS) of
    (Just gv, Just hv) | gv `notElem` supportedGHC -> do
      logWarn $
        "GHC " <> T.pack (prettyShow gv) <> " is not compatible with " <>
        "Haskell Language Server " <> T.pack (prettyShow hv) <> "." <> "\n" <>
        "Haskell IDE support may not work until this is fixed." <> "\n" <>
        "Install a different HLS version, or install and set one of the following GHCs:" <> "\n" <>
        T.pack (prettyShow supportedGHC)

    _ -> return ()

    ------------------
    --[ List tools ]--
    ------------------


-- | Filter data type for 'listVersions'.
data ListCriteria = ListInstalled
                  | ListSet
                  | ListAvailable
                  deriving Show

-- | A list result describes a single tool version
-- and various of its properties.
data ListResult = ListResult
  { lTool      :: Tool
  , lVer       :: Version
  , lCross     :: Maybe Text -- ^ currently only for GHC
  , lTag       :: [Tag]
  , lInstalled :: Bool
  , lSet       :: Bool -- ^ currently active version
  , fromSrc    :: Bool -- ^ compiled from source
  , lStray     :: Bool -- ^ not in download info
  , lNoBindist :: Bool -- ^ whether the version is available for this platform/arch
  , hlsPowered :: Bool
  }
  deriving (Eq, Ord, Show)


-- | Extract all available tool versions and their tags.
availableToolVersions :: GHCupDownloads -> Tool -> Map.Map Version VersionInfo
availableToolVersions av tool = view
  (at tool % non Map.empty)
  av


-- | List all versions from the download info, as well as stray
-- versions.
listVersions :: ( MonadCatch m
                , HasLog env
                , MonadThrow m
                , HasLog env
                , MonadIO m
                , MonadReader env m
                , HasDirs env
                , HasPlatformReq env
                , HasGHCupInfo env
                )
             => Maybe Tool
             -> Maybe ListCriteria
             -> m [ListResult]
listVersions lt' criteria = do
  -- some annoying work to avoid too much repeated IO
  cSet <- cabalSet
  cabals <- getInstalledCabals
  hlsSet' <- hlsSet
  hlses <- getInstalledHLSs
  sSet <- stackSet
  stacks <- getInstalledStacks

  go lt' cSet cabals hlsSet' hlses sSet stacks
 where
  go lt cSet cabals hlsSet' hlses sSet stacks = do
    case lt of
      Just t -> do
        GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
        -- get versions from GHCupDownloads
        let avTools = availableToolVersions dls t
        lr <- filter' <$> forM (Map.toList avTools) (toListResult t cSet cabals hlsSet' hlses sSet stacks)

        case t of
          GHC -> do
            slr <- strayGHCs avTools
            pure (sort (slr ++ lr))
          Cabal -> do
            slr <- strayCabals avTools cSet cabals
            pure (sort (slr ++ lr))
          HLS -> do
            slr <- strayHLS avTools hlsSet' hlses
            pure (sort (slr ++ lr))
          Stack -> do
            slr <- strayStacks avTools sSet stacks
            pure (sort (slr ++ lr))
          GHCup -> do
            let cg = maybeToList $ currentGHCup avTools
            pure (sort (cg ++ lr))
      Nothing -> do
        ghcvers   <- go (Just GHC) cSet cabals hlsSet' hlses sSet stacks
        cabalvers <- go (Just Cabal) cSet cabals hlsSet' hlses sSet stacks
        hlsvers   <- go (Just HLS) cSet cabals hlsSet' hlses sSet stacks
        ghcupvers <- go (Just GHCup) cSet cabals hlsSet' hlses sSet stacks
        stackvers <- go (Just Stack) cSet cabals hlsSet' hlses sSet stacks
        pure (ghcvers <> cabalvers <> hlsvers <> stackvers <> ghcupvers)
  strayGHCs :: ( MonadCatch m
               , MonadReader env m
               , HasDirs env
               , MonadThrow m
               , HasLog env
               , MonadIO m
               )
            => Map.Map Version VersionInfo
            -> m [ListResult]
  strayGHCs avTools = do
    ghcs <- getInstalledGHCs
    fmap catMaybes $ forM ghcs $ \case
      Right tver@GHCTargetVersion{ _tvTarget = Nothing, .. } -> do
        case Map.lookup _tvVersion avTools of
          Just _  -> pure Nothing
          Nothing -> do
            lSet    <- fmap (maybe False (\(GHCTargetVersion _ v ) -> v == _tvVersion)) $ ghcSet Nothing
            fromSrc <- ghcSrcInstalled tver
            hlsPowered <- fmap (elem _tvVersion) hlsGHCVersions
            pure $ Just $ ListResult
              { lTool      = GHC
              , lVer       = _tvVersion
              , lCross     = Nothing
              , lTag       = []
              , lInstalled = True
              , lStray     = isNothing (Map.lookup _tvVersion avTools)
              , lNoBindist = False
              , ..
              }
      Right tver@GHCTargetVersion{ .. } -> do
        lSet    <- fmap (maybe False (\(GHCTargetVersion _ v ) -> v == _tvVersion)) $ ghcSet _tvTarget
        fromSrc <- ghcSrcInstalled tver
        hlsPowered <- fmap (elem _tvVersion) hlsGHCVersions
        pure $ Just $ ListResult
          { lTool      = GHC
          , lVer       = _tvVersion
          , lCross     = _tvTarget
          , lTag       = []
          , lInstalled = True
          , lStray     = True -- NOTE: cross currently cannot be installed via bindist
          , lNoBindist = False
          , ..
          }
      Left e -> do
        logWarn
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  strayCabals :: ( MonadReader env m
                 , HasDirs env
                 , MonadCatch m
                 , MonadThrow m
                 , HasLog env
                 , MonadIO m
                 )
            => Map.Map Version VersionInfo
            -> Maybe Version
            -> [Either FilePath Version]
            -> m [ListResult]
  strayCabals avTools cSet cabals = do
    fmap catMaybes $ forM cabals $ \case
      Right ver ->
        case Map.lookup ver avTools of
          Just _  -> pure Nothing
          Nothing -> do
            let lSet = cSet == Just ver
            pure $ Just $ ListResult
              { lTool      = Cabal
              , lVer       = ver
              , lCross     = Nothing
              , lTag       = []
              , lInstalled = True
              , lStray     = isNothing (Map.lookup ver avTools)
              , lNoBindist = False
              , fromSrc    = False -- actually, we don't know :>
              , hlsPowered = False
              , ..
              }
      Left e -> do
        logWarn
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  strayHLS :: ( MonadReader env m
              , HasDirs env
              , MonadCatch m
              , MonadThrow m
              , HasLog env
              , MonadIO m)
           => Map.Map Version VersionInfo
           -> Maybe Version
           -> [Either FilePath Version]
           -> m [ListResult]
  strayHLS avTools hlsSet' hlss = do
    fmap catMaybes $ forM hlss $ \case
      Right ver ->
        case Map.lookup ver avTools of
          Just _  -> pure Nothing
          Nothing -> do
            let lSet = hlsSet' == Just ver
            pure $ Just $ ListResult
              { lTool      = HLS
              , lVer       = ver
              , lCross     = Nothing
              , lTag       = []
              , lInstalled = True
              , lStray     = isNothing (Map.lookup ver avTools)
              , lNoBindist = False
              , fromSrc    = False -- actually, we don't know :>
              , hlsPowered = False
              , ..
              }
      Left e -> do
        logWarn
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  strayStacks :: ( MonadReader env m
                 , HasDirs env
                 , MonadCatch m
                 , MonadThrow m
                 , HasLog env
                 , MonadIO m
                 )
              => Map.Map Version VersionInfo
              -> Maybe Version
              -> [Either FilePath Version]
              -> m [ListResult]
  strayStacks avTools stackSet' stacks = do
    fmap catMaybes $ forM stacks $ \case
      Right ver ->
        case Map.lookup ver avTools of
          Just _  -> pure Nothing
          Nothing -> do
            let lSet = stackSet' == Just ver
            pure $ Just $ ListResult
              { lTool      = Stack
              , lVer       = ver
              , lCross     = Nothing
              , lTag       = []
              , lInstalled = True
              , lStray     = isNothing (Map.lookup ver avTools)
              , lNoBindist = False
              , fromSrc    = False -- actually, we don't know :>
              , hlsPowered = False
              , ..
              }
      Left e -> do
        logWarn
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  currentGHCup :: Map.Map Version VersionInfo -> Maybe ListResult
  currentGHCup av =
    let currentVer = fromJust $ pvpToVersion ghcUpVer ""
        listVer    = Map.lookup currentVer av
        latestVer  = fst <$> headOf (getTagged Latest) av
        recommendedVer = fst <$> headOf (getTagged Latest) av
        isOld  = maybe True (> currentVer) latestVer && maybe True (> currentVer) recommendedVer
    in if | Map.member currentVer av -> Nothing
          | otherwise -> Just $ ListResult { lVer    = currentVer
                                           , lTag    = maybe (if isOld then [Old] else []) _viTags listVer
                                           , lCross  = Nothing
                                           , lTool   = GHCup
                                           , fromSrc = False
                                           , lStray  = isNothing listVer
                                           , lSet    = True
                                           , lInstalled = True
                                           , lNoBindist = False
                                           , hlsPowered = False
                                           }

  -- NOTE: this are not cross ones, because no bindists
  toListResult :: ( HasLog env
                  , MonadReader env m
                  , HasDirs env
                  , HasGHCupInfo env
                  , HasPlatformReq env
                  , MonadIO m
                  , MonadCatch m
                  )
               => Tool
               -> Maybe Version
               -> [Either FilePath Version]
               -> Maybe Version
               -> [Either FilePath Version]
               -> Maybe Version
               -> [Either FilePath Version]
               -> (Version, VersionInfo)
               -> m ListResult
  toListResult t cSet cabals hlsSet' hlses stackSet' stacks (v, _viTags -> tags) = do
    case t of
      GHC -> do
        lNoBindist <- fmap (isLeft . veitherToEither) $ runE @'[NoDownload] $ getDownloadInfo GHC v
        let tver = mkTVer v
        lSet       <- fmap (maybe False (\(GHCTargetVersion _ v') -> v' == v)) $ ghcSet Nothing
        lInstalled <- ghcInstalled tver
        fromSrc    <- ghcSrcInstalled tver
        hlsPowered <- fmap (elem v) hlsGHCVersions
        pure ListResult { lVer = v, lCross = Nothing , lTag = tags, lTool = t, lStray = False, .. }
      Cabal -> do
        lNoBindist <- fmap (isLeft . veitherToEither) $ runE @'[NoDownload] $ getDownloadInfo Cabal v
        let lSet = cSet == Just v
        let lInstalled = elem v $ rights cabals
        pure ListResult { lVer    = v
                        , lCross  = Nothing
                        , lTag    = tags
                        , lTool   = t
                        , fromSrc = False
                        , lStray  = False
                        , hlsPowered = False
                        , ..
                        }
      GHCup -> do
        let lSet       = prettyPVP ghcUpVer == prettyVer v
        let lInstalled = lSet
        pure ListResult { lVer    = v
                        , lTag    = tags
                        , lCross  = Nothing
                        , lTool   = t
                        , fromSrc = False
                        , lStray  = False
                        , lNoBindist = False
                        , hlsPowered = False
                        , ..
                        }
      HLS -> do
        lNoBindist <- fmap (isLeft . veitherToEither) $ runE @'[NoDownload] $ getDownloadInfo HLS v
        let lSet = hlsSet' == Just v
        let lInstalled = elem v $ rights hlses
        pure ListResult { lVer    = v
                        , lCross  = Nothing
                        , lTag    = tags
                        , lTool   = t
                        , fromSrc = False
                        , lStray  = False
                        , hlsPowered = False
                        , ..
                        }
      Stack -> do
        lNoBindist <- fmap (isLeft . veitherToEither) $ runE @'[NoDownload] $ getDownloadInfo Stack v
        let lSet = stackSet' == Just v
        let lInstalled = elem v $ rights stacks
        pure ListResult { lVer    = v
                        , lCross  = Nothing
                        , lTag    = tags
                        , lTool   = t
                        , fromSrc = False
                        , lStray  = False
                        , hlsPowered = False
                        , ..
                        }


  filter' :: [ListResult] -> [ListResult]
  filter' lr = case criteria of
    Nothing            -> lr
    Just ListInstalled -> filter (\ListResult {..} -> lInstalled) lr
    Just ListSet       -> filter (\ListResult {..} -> lSet) lr
    Just ListAvailable -> filter (\ListResult {..} -> not lNoBindist) lr



    --------------------
    --[ GHC/cabal rm ]--
    --------------------


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

  dir <- lift $ ghcupGHCDir ver
  lift (getInstalledFiles GHC ver) >>= \case
    Just files -> do
      lift $ logInfo $ "Removing files safely from: " <> T.pack dir
      forM_ files (liftIO . deleteFile . (\f -> dir </> dropDrive f))
      f <- recordedInstallationFile GHC ver
      liftIO $ deleteFile f
      removeEmptyDirsRecursive dir
      survivors <- liftIO $ hideErrorDef [doesNotExistErrorType] [] $ listDirectory dir
      when (not (null survivors)) $ throwE $ UninstallFailed dir survivors
    Nothing -> do
      lift $ logInfo $ "Removing legacy directory recursively: " <> T.pack dir
      lift $ recyclePathForcibly dir

  v' <-
    handle
      (\(e :: ParseError) -> lift $ logWarn (T.pack $ displayException e) >> pure Nothing)
    $ fmap Just
    $ getMajorMinorV (_tvVersion ver)
  forM_ v' $ \(mj, mi) -> lift (getGHCForPVP (PVP (fromIntegral mj :| [fromIntegral mi])) (_tvTarget ver))
    >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY Nothing)

  Dirs {..} <- lift getDirs

  lift $ hideError doesNotExistErrorType $ rmDirectoryLink (baseDir </> "share")


-- | Delete a cabal version. Will try to fix the @cabal@ symlink
-- after removal (e.g. setting it to an older version).
rmCabalVer :: ( MonadMask m
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
           -> Excepts '[NotInstalled] m ()
rmCabalVer ver = do
  whenM (lift $ fmap not $ cabalInstalled ver) $ throwE (NotInstalled Cabal (GHCTargetVersion Nothing ver))

  cSet      <- lift cabalSet

  Dirs {..} <- lift getDirs

  let cabalFile = "cabal-" <> T.unpack (prettyVer ver) <> exeExt
  lift $ hideError doesNotExistErrorType $ recycleFile (binDir </> cabalFile)

  when (Just ver == cSet) $ do
    cVers <- lift $ fmap rights getInstalledCabals
    case headMay . reverse . sort $ cVers of
      Just latestver -> setCabal latestver
      Nothing        -> lift $ rmLink (binDir </> "cabal" <> exeExt)


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

  hlsDir <- ghcupHLSDir ver
  lift (getInstalledFiles HLS (mkTVer ver)) >>= \case
    Just files -> do
      lift $ logInfo $ "Removing files safely from: " <> T.pack hlsDir
      forM_ files (liftIO . deleteFile . (\f -> hlsDir </> dropDrive f))
      f <- recordedInstallationFile HLS (mkTVer ver)
      liftIO $ deleteFile f
      removeEmptyDirsRecursive hlsDir
      survivors <- liftIO $ hideErrorDef [doesNotExistErrorType] [] $ listDirectory hlsDir
      when (not (null survivors)) $ throwE $ UninstallFailed hlsDir survivors
    Nothing -> do
      lift $ logInfo $ "Removing legacy directory recursively: " <> T.pack hlsDir
      recyclePathForcibly hlsDir

  when (Just ver == isHlsSet) $ do
    -- set latest hls
    hlsVers <- lift $ fmap rights getInstalledHLSs
    case headMay . reverse . sort $ hlsVers of
      Just latestver -> liftE $ setHLS latestver SetHLSOnly Nothing
      Nothing        -> pure ()


-- | Delete a stack version. Will try to fix the @stack@ symlink
-- after removal (e.g. setting it to an older version).
rmStackVer :: ( MonadMask m
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
           -> Excepts '[NotInstalled] m ()
rmStackVer ver = do
  whenM (lift $ fmap not $ stackInstalled ver) $ throwE (NotInstalled Stack (GHCTargetVersion Nothing ver))

  sSet      <- lift stackSet

  Dirs {..} <- lift getDirs

  let stackFile = "stack-" <> T.unpack (prettyVer ver) <> exeExt
  lift $ hideError doesNotExistErrorType $ recycleFile (binDir </> stackFile)

  when (Just ver == sSet) $ do
    sVers <- lift $ fmap rights getInstalledStacks
    case headMay . reverse . sort $ sVers of
      Just latestver -> setStack latestver
      Nothing        -> lift $ rmLink (binDir </> "stack" <> exeExt)


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
    -- we move it to temp dir, to be deleted at next reboot
    tempFilepath <- mkGhcupTmpDir
    hideError UnsupportedOperation $
              liftIO $ hideError NoSuchThing $
              moveFile ghcupFilepath (tempFilepath </> "ghcup")
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

rmTool :: ( MonadReader env m
          , HasDirs env
          , HasLog env
          , MonadFail m
          , MonadMask m
          , MonadUnliftIO m)
          => ListResult
          -> Excepts '[NotInstalled, UninstallFailed] m ()
rmTool ListResult {lVer, lTool, lCross} = do
  case lTool of
    GHC ->
      let ghcTargetVersion = GHCTargetVersion lCross lVer
      in rmGHCVer ghcTargetVersion
    HLS -> rmHLSVer lVer
    Cabal -> liftE $ rmCabalVer lVer
    Stack -> liftE $ rmStackVer lVer
    GHCup -> lift rmGhcup


rmGhcupDirs :: ( MonadReader env m
               , HasDirs env
               , MonadIO m
               , HasLog env
               , MonadCatch m
               , MonadMask m )
            => m [FilePath]
rmGhcupDirs = do
  Dirs
    { baseDir
    , binDir
    , logsDir
    , cacheDir
    , recycleDir
    } <- getDirs

  let envFilePath = baseDir </> "env"

  confFilePath <- getConfigFilePath

  handleRm $ rmEnvFile  envFilePath
  handleRm $ rmConfFile confFilePath

  -- for xdg dirs, the order matters here
  handleRm $ rmDir logsDir
  handleRm $ rmDir cacheDir

  handleRm $ rmBinDir binDir
  handleRm $ rmDir recycleDir
  when isWindows $ do
    logInfo $ "removing " <> T.pack (baseDir </> "msys64")
    handleRm $ rmPathForcibly (baseDir </> "msys64")

  handleRm $ removeEmptyDirsRecursive baseDir

  -- report files in baseDir that are left-over after
  -- the standard location deletions above
  hideErrorDef [doesNotExistErrorType] [] $ reportRemainingFiles baseDir

  where
    handleRm :: (MonadReader env m, MonadCatch m, HasLog env, MonadIO m)  => m () -> m ()
    handleRm = handleIO (\e -> logDebug $ "Part of the cleanup action failed with error: " <> T.pack (displayException e) <> "\n"
                                <> "continuing regardless...")

    rmEnvFile :: (HasLog env, MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmEnvFile enFilePath = do
      logInfo "Removing Ghcup Environment File"
      hideErrorDef [permissionErrorType] () $ deleteFile' enFilePath

    rmConfFile :: (HasLog env, MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmConfFile confFilePath = do
      logInfo "removing Ghcup Config File"
      hideErrorDef [permissionErrorType] () $ deleteFile' confFilePath

    rmDir :: (HasLog env, MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmDir dir =
      -- 'getDirectoryContentsRecursive' is lazy IO. In case
      -- an error leaks through, we catch it here as well,
      -- althought 'deleteFile' should already handle it.
      hideErrorDef [doesNotExistErrorType] () $ do
        logInfo $ "removing " <> T.pack dir
        contents <- liftIO $ getDirectoryContentsRecursive dir
        forM_ contents (deleteFile' . (dir </>))

    rmBinDir :: (MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmBinDir binDir
      | isWindows = removeDirIfEmptyOrIsSymlink binDir
      | otherwise = do
          isXDGStyle <- liftIO useXDG
          if not isXDGStyle
            then removeDirIfEmptyOrIsSymlink binDir
            else pure ()

    reportRemainingFiles :: MonadIO m => FilePath -> m [FilePath]
    reportRemainingFiles dir = do
      -- force the files so the errors don't leak
      (force -> !remainingFiles) <- liftIO
        (getDirectoryContentsRecursive dir >>= evaluate)
      let normalizedFilePaths = fmap normalise remainingFiles
      let sortedByDepthRemainingFiles = sortBy (flip compareFn) normalizedFilePaths
      let remainingFilesAbsolute = fmap (dir </>) sortedByDepthRemainingFiles

      pure remainingFilesAbsolute

      where
        calcDepth :: FilePath -> Int
        calcDepth = length . filter isPathSeparator

        compareFn :: FilePath -> FilePath -> Ordering
        compareFn fp1 fp2 = compare (calcDepth fp1) (calcDepth fp2)

-- we expect only files inside cache/log dir
-- we report remaining files/dirs later,
-- hence the force/quiet mode in these delete functions below.

deleteFile' :: (MonadReader env m, HasDirs env, MonadMask m, MonadIO m) => FilePath -> m ()
deleteFile' filepath = do
  hideError doesNotExistErrorType
    $ hideError InappropriateType $ rmFile filepath

removeDirIfEmptyOrIsSymlink :: (MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
removeDirIfEmptyOrIsSymlink filepath =
  hideError UnsatisfiedConstraints $
  handleIO' InappropriateType
        (handleIfSym filepath)
        (liftIO $ rmDirectory filepath)
  where
    handleIfSym fp e = do
      isSym <- liftIO $ pathIsSymbolicLink fp
      if isSym
      then deleteFile' fp
      else liftIO $ ioError e

removeEmptyDirsRecursive :: (MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
removeEmptyDirsRecursive fp = do
  cs <- liftIO $ listDirectory fp >>= filterM doesDirectoryExist . fmap (fp </>)
  forM_ cs removeEmptyDirsRecursive
  hideError InappropriateType $ removeDirIfEmptyOrIsSymlink fp


    ------------------
    --[ Debug info ]--
    ------------------


getDebugInfo :: ( Alternative m
                , MonadFail m
                , MonadReader env m
                , HasDirs env
                , HasLog env
                , MonadCatch m
                , MonadIO m
                )
             => Excepts
                  '[NoCompatiblePlatform , NoCompatibleArch , DistroNotFound]
                  m
                  DebugInfo
getDebugInfo = do
  Dirs {..} <- lift getDirs
  let diBaseDir  = baseDir
  let diBinDir   = binDir
  diGHCDir       <- lift ghcupGHCBaseDir
  let diCacheDir = cacheDir
  diArch         <- lE getArchitecture
  diPlatform     <- liftE getPlatform
  pure $ DebugInfo { .. }




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
        liftE $ cleanUpOnError tmpUnpack (unpackToDir tmpUnpack dl)
        liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

        workdir <- maybe (pure tmpUnpack)
                         (liftE . intoSubdir tmpUnpack)
                         (view dlSubdir dlInfo)
        liftE $ applyAnyPatch patches workdir

        pure (workdir, tmpUnpack, tver)

      -- clone from git
      Right GitBranch{..} -> do
        tmpUnpack <- lift mkGhcupTmpDir
        let git args = execLogged "git" ("--no-pager":args) (Just tmpUnpack) "git" Nothing
        tver <- reThrowAll @_ @'[PatchFailed, ProcessError, NotFoundInPATH, DigestError, DownloadFailed, GPGError] DownloadFailed $ do
          let rep = fromMaybe "https://gitlab.haskell.org/ghc/ghc.git" repo
          lift $ logInfo $ "Fetching git repo " <> T.pack rep <> " at ref " <> T.pack ref <> " (this may take a while)"
          lEM $ git [ "init" ]
          lEM $ git [ "remote"
                    , "add"
                    , "origin"
                    , fromString rep ]

          let fetch_args =
                    [ "fetch"
                    , "--depth"
                    , "1"
                    , "--quiet"
                    , "origin"
                    , fromString ref ]
          lEM $ git fetch_args

          lEM $ git [ "checkout", "FETCH_HEAD" ]
          lEM $ git [ "submodule", "update", "--init", "--depth", "1" ]
          liftE $ applyAnyPatch patches tmpUnpack
          lEM $ execWithGhcEnv "python3" ["./boot"] (Just tmpUnpack) "ghc-bootstrap"
          lEM $ execWithGhcEnv "sh" ["./configure"] (Just tmpUnpack) "ghc-bootstrap"
          CapturedProcess {..} <- lift $ makeOut
            ["show!", "--quiet", "VALUE=ProjectVersion" ] (Just tmpUnpack)
          case _exitCode of
            ExitSuccess -> throwEither . MP.parse ghcProjectVersion "" . decUTF8Safe' $ _stdOut
            ExitFailure c -> fail ("Could not figure out GHC project version. Exit code was: " <> show c <> ". Error was: " <> T.unpack (decUTF8Safe' _stdErr))

        liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack
        lift $ logInfo $ "Git version " <> T.pack ref <> " corresponds to GHC version " <> prettyVer tver

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
             then compileHadrianBindist tver workdir ghcdir
             else compileMakeBindist tver workdir ghcdir
        bmk <- liftIO $ handleIO (\_ -> pure "") $ B.readFile (build_mk workdir)
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
    let tarPath = cacheDir </> tarName
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






    ---------------------
    --[ Upgrade GHCup ]--
    ---------------------


-- | Upgrade ghcup and place it in @~\/.ghcup\/bin\/ghcup@,
-- if no path is provided.
upgradeGHCup :: ( MonadMask m
                , MonadReader env m
                , HasDirs env
                , HasPlatformReq env
                , HasGHCupInfo env
                , HasSettings env
                , MonadCatch m
                , HasLog env
                , MonadThrow m
                , MonadFail m
                , MonadResource m
                , MonadIO m
                , MonadUnliftIO m
                )
             => Maybe FilePath    -- ^ full file destination to write ghcup into
             -> Bool              -- ^ whether to force update regardless
                                  --   of currently installed version
             -> Bool              -- ^ whether to throw an error if ghcup is shadowed
             -> Excepts
                  '[ CopyError
                   , DigestError
                   , GPGError
                   , GPGError
                   , DownloadFailed
                   , NoDownload
                   , NoUpdate
                   , GHCupShadowed
                   ]
                  m
                  Version
upgradeGHCup mtarget force' fatal = do
  Dirs {..} <- lift getDirs
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo

  lift $ logInfo "Upgrading GHCup..."
  let latestVer = fst (fromJust (getLatest dls GHCup))
  (Just ghcupPVPVer) <- pure $ pvpToVersion ghcUpVer ""
  when (not force' && (latestVer <= ghcupPVPVer)) $ throwE NoUpdate
  dli   <- liftE $ getDownloadInfo GHCup latestVer
  tmp   <- lift withGHCupTmpDir
  let fn = "ghcup" <> exeExt
  p <- liftE $ download (_dlUri dli) Nothing (Just (_dlHash dli)) tmp (Just fn) False
  let destDir = takeDirectory destFile
      destFile = fromMaybe (binDir </> fn) mtarget
  lift $ logDebug $ "mkdir -p " <> T.pack destDir
  liftIO $ createDirRecursive' destDir
  lift $ logDebug $ "rm -f " <> T.pack destFile
  lift $ hideError NoSuchThing $ recycleFile destFile
  lift $ logDebug $ "cp " <> T.pack p <> " " <> T.pack destFile
  copyFileE p destFile False
  lift $ chmod_755 destFile

  liftIO (isInPath destFile) >>= \b -> unless b $
    lift $ logWarn $ T.pack (takeFileName destFile) <> " is not in PATH! You have to add it in order to use ghcup."
  liftIO (isShadowed destFile) >>= \case
    Nothing -> pure ()
    Just pa
      | fatal -> throwE (GHCupShadowed pa destFile latestVer)
      | otherwise ->
        lift $ logWarn $ "ghcup is shadowed by "
          <> T.pack pa
          <> ". The upgrade will not be in effect, unless you remove "
          <> T.pack pa
          <> " or make sure "
          <> T.pack destDir
          <> " comes before "
          <> T.pack (takeDirectory pa)
          <> " in PATH."

  pure latestVer



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


-- | Reports the binary location of a given tool:
--
--   * for GHC, this reports: @~\/.ghcup\/ghc\/\<ver\>\/bin\/ghc@
--   * for cabal, this reports @~\/.ghcup\/bin\/cabal-\<ver\>@
--   * for hls, this reports @~\/.ghcup\/bin\/haskell-language-server-wrapper-\<ver\>@
--   * for stack, this reports @~\/.ghcup\/bin\/stack-\<ver\>@
--   * for ghcup, this reports the location of the currently running executable
whereIsTool :: ( MonadReader env m
               , HasDirs env
               , HasLog env
               , MonadThrow m
               , MonadFail m
               , MonadIO m
               , MonadCatch m
               , MonadMask m
               , MonadUnliftIO m
               )
            => Tool
            -> GHCTargetVersion
            -> Excepts '[NotInstalled] m FilePath
whereIsTool tool ver@GHCTargetVersion {..} = do
  dirs <- lift getDirs

  case tool of
    GHC -> do
      whenM (lift $ fmap not $ ghcInstalled ver)
        $ throwE (NotInstalled GHC ver)
      bdir <- lift $ ghcupGHCDir ver
      pure (bdir </> "bin" </> ghcBinaryName ver)
    Cabal -> do
      whenM (lift $ fmap not $ cabalInstalled _tvVersion)
        $ throwE (NotInstalled Cabal (GHCTargetVersion Nothing _tvVersion))
      pure (binDir dirs </> "cabal-" <> T.unpack (prettyVer _tvVersion) <> exeExt)
    HLS -> do
      whenM (lift $ fmap not $ hlsInstalled _tvVersion)
        $ throwE (NotInstalled HLS (GHCTargetVersion Nothing _tvVersion))
      ifM (lift $ isLegacyHLS _tvVersion)
        (pure (binDir dirs </> "haskell-language-server-wrapper-" <> T.unpack (prettyVer _tvVersion) <> exeExt))
        $ do
          bdir <- lift $ ghcupHLSDir _tvVersion
          pure (bdir </> "bin" </> "haskell-language-server-wrapper" <> exeExt)

    Stack -> do
      whenM (lift $ fmap not $ stackInstalled _tvVersion)
        $ throwE (NotInstalled Stack (GHCTargetVersion Nothing _tvVersion))
      pure (binDir dirs </> "stack-" <> T.unpack (prettyVer _tvVersion) <> exeExt)
    GHCup -> do
      currentRunningExecPath <- liftIO getExecutablePath
      liftIO $ canonicalizePath currentRunningExecPath

-- | Doesn't work for cross GHC.
checkIfToolInstalled :: ( MonadIO m
                        , MonadReader env m
                        , HasDirs env
                        , MonadCatch m) =>
                        Tool ->
                        Version ->
                        m Bool
checkIfToolInstalled tool ver = checkIfToolInstalled' tool (mkTVer ver)

checkIfToolInstalled' :: ( MonadIO m
                         , MonadReader env m
                         , HasDirs env
                         , MonadCatch m) =>
                        Tool ->
                        GHCTargetVersion ->
                        m Bool
checkIfToolInstalled' tool ver =
  case tool of
    Cabal -> cabalInstalled (_tvVersion ver)
    HLS   -> hlsInstalled (_tvVersion ver)
    Stack -> stackInstalled (_tvVersion ver)
    GHC   -> ghcInstalled ver
    _     -> pure False

throwIfFileAlreadyExists :: ( MonadIO m ) =>
                            FilePath ->
                            Excepts '[FileAlreadyExistsError] m ()

throwIfFileAlreadyExists fp = whenM (checkFileAlreadyExists fp)
                                (throwE $ FileAlreadyExistsError fp)



    --------------------------
    --[ Garbage collection ]--
    --------------------------


rmOldGHC :: ( MonadReader env m
            , HasGHCupInfo env
            , HasDirs env
            , HasLog env
            , MonadIO m
            , MonadFail m
            , MonadMask m
            , MonadUnliftIO m
            )
         => Excepts '[NotInstalled, UninstallFailed] m ()
rmOldGHC = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  let oldGHCs = mkTVer <$> toListOf (ix GHC % getTagged Old % to fst) dls
  ghcs <- lift $ fmap rights getInstalledGHCs
  forM_ ghcs $ \ghc -> when (ghc `elem` oldGHCs) $ rmGHCVer ghc



rmProfilingLibs :: ( MonadReader env m
                   , HasDirs env
                   , HasLog env
                   , MonadIO m
                   , MonadFail m
                   , MonadMask m
                   , MonadUnliftIO m
                   )
                => m ()
rmProfilingLibs = do
  ghcs <- fmap rights getInstalledGHCs

  let regexes :: [ByteString]
      regexes = [[s|.*_p\.a$|], [s|.*\.p_hi$|]]

  forM_ regexes $ \regex ->
    forM_ ghcs $ \ghc -> do
      d <- ghcupGHCDir ghc
      matches <- liftIO $ handleIO (\_ -> pure []) $ findFilesDeep
        d
        (makeRegexOpts compExtended
                       execBlank
                       regex
        )
      forM_ matches $ \m -> do
        let p = d </> m
        logDebug $ "rm " <> T.pack p
        rmFile p



rmShareDir :: ( MonadReader env m
              , HasDirs env
              , HasLog env
              , MonadIO m
              , MonadFail m
              , MonadMask m
              , MonadUnliftIO m
              )
           => m ()
rmShareDir = do
  ghcs <- fmap rights getInstalledGHCs
  forM_ ghcs $ \ghc -> do
    d <- ghcupGHCDir ghc
    let p = d </> "share"
    logDebug $ "rm -rf " <> T.pack p
    rmPathForcibly p


rmHLSNoGHC :: ( MonadReader env m
              , HasDirs env
              , HasLog env
              , MonadIO m
              , MonadMask m
              , MonadFail m
              , MonadUnliftIO m
              )
           => Excepts '[NotInstalled, UninstallFailed] m ()
rmHLSNoGHC = do
  Dirs {..} <- getDirs
  ghcs <- fmap rights getInstalledGHCs
  hlses <- fmap rights getInstalledHLSs
  forM_ hlses $ \hls -> do
    hlsGHCs <- fmap mkTVer <$> hlsGHCVersions' hls
    let candidates = filter (`notElem` ghcs) hlsGHCs
    if (length hlsGHCs - length candidates) <= 0
    then rmHLSVer hls
    else
      forM_ candidates $ \ghc -> do
        bins1 <- fmap (binDir </>) <$> hlsServerBinaries hls (Just $ _tvVersion ghc)
        bins2 <- ifM (isLegacyHLS hls) (pure []) $ do
          shs <- hlsInternalServerScripts hls (Just $ _tvVersion ghc)
          bins <- hlsInternalServerBinaries hls (Just $ _tvVersion ghc)
          libs <- hlsInternalServerLibs hls (_tvVersion ghc)
          pure (shs ++ bins ++ libs)
        forM_ (bins1 ++ bins2) $ \f -> do
          logDebug $ "rm " <> T.pack f
          rmFile f
    pure ()


rmCache :: ( MonadReader env m
           , HasDirs env
           , HasLog env
           , MonadIO m
           , MonadMask m
           )
        => m ()
rmCache = do
  Dirs {..} <- getDirs
  contents <- liftIO $ listDirectory cacheDir
  forM_ contents $ \f -> do
    let p = cacheDir </> f
    logDebug $ "rm " <> T.pack p
    rmFile p


rmTmp :: ( MonadReader env m
         , HasDirs env
         , HasLog env
         , MonadIO m
         , MonadMask m
         )
      => m ()
rmTmp = do
  tmpdir <- liftIO getCanonicalTemporaryDirectory
  ghcup_dirs <- liftIO $ handleIO (\_ -> pure []) $ findFiles
    tmpdir
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^ghcup-.*$|] :: ByteString)
    )
  forM_ ghcup_dirs $ \f -> do
    let p = tmpdir </> f
    logDebug $ "rm -rf " <> T.pack p
    rmPathForcibly p


applyAnyPatch :: ( MonadReader env m
                 , HasDirs env
                 , HasLog env
                 , HasSettings env
                 , MonadUnliftIO m
                 , MonadCatch m
                 , MonadResource m
                 , MonadThrow m
                 , MonadMask m
                 , MonadIO m)
              => Maybe (Either FilePath [URI])
              -> FilePath
              -> Excepts '[PatchFailed, DownloadFailed, DigestError, GPGError] m ()
applyAnyPatch Nothing _                   = pure ()
applyAnyPatch (Just (Left pdir)) workdir  = liftE $ applyPatches pdir workdir
applyAnyPatch (Just (Right uris)) workdir = do
  tmpUnpack <- lift withGHCupTmpDir
  forM_ uris $ \uri -> do
    patch <- liftE $ download uri Nothing Nothing tmpUnpack Nothing False
    liftE $ applyPatch patch workdir
