{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}

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
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
#if defined(IS_WINDOWS)
import           Control.Monad.IO.Unlift        ( MonadUnliftIO( withRunInIO ) )
#endif
import           Data.ByteString                ( ByteString )
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import           Data.Time.Clock
import           Data.Time.Format.ISO8601
import           Data.Versions
import           GHC.IO.Exception
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           Safe                    hiding ( at )
import           System.Directory        hiding ( findFiles )
import           System.Environment
import           System.FilePath
import           System.IO.Error
#if defined(IS_WINDOWS)
import           System.IO.Temp
#endif
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           Text.Regex.Posix

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as E
#if defined(IS_WINDOWS)
import qualified System.Win32.File             as Win32
#endif
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
                    , MonadLogger m
                    , MonadResource m
                    , MonadIO m
                    , MonadUnliftIO m
                    )
                 => Version
                 -> Tool
                 -> Maybe FilePath
                 -> Excepts
                      '[ DigestError
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
               , MonadLogger m
               , MonadResource m
               , MonadIO m
               , MonadUnliftIO m
               )
            => Version
            -> Maybe FilePath
            -> Excepts
                 '[ DigestError
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
                     , MonadLogger m
                     , MonadResource m
                     , MonadIO m
                     , MonadUnliftIO m
                     )
                  => DownloadInfo    -- ^ where/how to download
                  -> Version         -- ^ the version to install
                  -> Maybe FilePath  -- ^ isolated filepath if user passed any
                  -> Excepts
                       '[ AlreadyInstalled
                        , BuildFailed
                        , DigestError
                        , DownloadFailed
                        , NoDownload
                        , NotInstalled
                        , UnknownArchive
                        , TarDirDoesNotExist
                        , DirNotEmpty
                        , ArchiveResult
                        ]
                       m
                       ()
installGHCBindist dlinfo ver isoFilepath = do
  let tver = mkTVer ver

  lift $ $(logDebug) $ "Requested to install GHC with " <> prettyVer ver

  case isoFilepath of
    -- we only care for already installed errors in regular (non-isolated) installs
    Nothing -> whenM (lift $ ghcInstalled tver) (throwE $ AlreadyInstalled GHC ver)
    _ -> pure ()

  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing

  -- prepare paths
  ghcdir <- lift $ ghcupGHCDir tver

  toolchainSanityChecks
  
  case isoFilepath of
    Just isoDir -> do                        -- isolated install
      lift $ $(logInfo) $ "isolated installing GHC to " <> T.pack isoDir
      liftE $ installPackedGHC dl (view dlSubdir dlinfo) isoDir ver
    Nothing -> do                            -- regular install
      liftE $ installPackedGHC dl (view dlSubdir dlinfo) ghcdir ver

      -- make symlinks & stuff when regular install,
      liftE $ postGHCInstall tver

 where
  toolchainSanityChecks = do
    r <- forM ["CC", "LD"] (liftIO . lookupEnv)
    case catMaybes r of
      [] -> pure ()
      _ -> do
        lift $ $(logWarn) "CC/LD environment variable is set. This will change the compiler/linker"
        lift $ $(logWarn) "GHC uses internally and can cause defunct GHC in some cases (e.g. in Anaconda"
        lift $ $(logWarn) "environments). If you encounter problems, unset CC and LD and reinstall."


-- | Install a packed GHC distribution. This only deals with unpacking and the GHC
-- build system and nothing else.
installPackedGHC :: ( MonadMask m
                    , MonadCatch m
                    , MonadReader env m
                    , HasDirs env
                    , HasPlatformReq env
                    , HasSettings env
                    , MonadThrow m
                    , MonadLogger m
                    , MonadIO m
                    , MonadUnliftIO m
                    )
                 => FilePath          -- ^ Path to the packed GHC bindist
                 -> Maybe TarDir      -- ^ Subdir of the archive
                 -> FilePath          -- ^ Path to install to
                 -> Version           -- ^ The GHC version
                 -> Excepts
                      '[ BuildFailed
                       , UnknownArchive
                       , TarDirDoesNotExist
                       , DirNotEmpty
                       , ArchiveResult
                       ] m ()
installPackedGHC dl msubdir inst ver = do
  PlatformRequest {..} <- lift getPlatformReq

  liftE $ installDestSanityCheck inst

  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack)
                   (liftE . intoSubdir tmpUnpack)
                   msubdir

  liftE $ runBuildAction tmpUnpack
                         (Just inst)
                         (installUnpackedGHC workdir inst ver)
 where
  -- | Does basic checks for isolated installs
  -- Isolated Directory:
  --   1. if it doesn't exist -> proceed
  --   2. if it exists and is empty -> proceed
  --   3. if it exists and is non-empty -> panic and leave the house
  installDestSanityCheck :: ( MonadIO m
                            , MonadCatch m
                            ) =>
                            FilePath ->
                            Excepts '[DirNotEmpty] m ()
  installDestSanityCheck isoDir = do
    hideErrorDef [doesNotExistErrorType] () $ do
      contents <- liftIO $ getDirectoryContentsRecursive isoDir
      unless (null contents) (throwE $ DirNotEmpty isoDir)



-- | Install an unpacked GHC distribution. This only deals with the GHC
-- build system and nothing else.
installUnpackedGHC :: ( MonadReader env m
                      , HasPlatformReq env
                      , HasDirs env
                      , HasSettings env
                      , MonadThrow m
                      , MonadLogger m
                      , MonadIO m
                      , MonadUnliftIO m
                      , MonadMask m
                      )
                   => FilePath      -- ^ Path to the unpacked GHC bindist (where the configure script resides)
                   -> FilePath      -- ^ Path to install to
                   -> Version       -- ^ The GHC version
                   -> Excepts '[ProcessError] m ()
installUnpackedGHC path inst ver = do
#if defined(IS_WINDOWS)
  lift $ $(logInfo) "Installing GHC (this may take a while)"
  -- Windows bindists are relocatable and don't need
  -- to run configure.
  -- We also must make sure to preserve mtime to not confuse ghc-pkg.
  lift $ withRunInIO $ \run -> flip onException (run $ recyclePathForcibly inst) $ copyDirectoryRecursive path inst $ \source dest -> do
    mtime <- getModificationTime source
    Win32.moveFile source dest
    setModificationTime dest mtime
#else
  PlatformRequest {..} <- lift getPlatformReq

  let alpineArgs
       | ver >= [vver|8.2.2|], Linux Alpine <- _rPlatform
       = ["--disable-ld-override"]
       | otherwise
       = []

  lift $ $(logInfo) "Installing GHC (this may take a while)"
  lEM $ execLogged "sh"
                   ("./configure" : ("--prefix=" <> inst) 
                    : alpineArgs
                   )
                   (Just path)
                   "ghc-configure"
                   Nothing
  lEM $ make ["install"] (Just path)
  pure ()
#endif


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
                 , MonadLogger m
                 , MonadResource m
                 , MonadIO m
                 , MonadUnliftIO m
                 )
              => Version         -- ^ the version to install
              -> Maybe FilePath  -- ^ isolated install filepath, if user passed any
              -> Excepts
                   '[ AlreadyInstalled
                    , BuildFailed
                    , DigestError
                    , DownloadFailed
                    , NoDownload
                    , NotInstalled
                    , UnknownArchive
                    , TarDirDoesNotExist
                    , DirNotEmpty
                    , ArchiveResult
                    ]
                   m
                   ()
installGHCBin ver isoFilepath = do
  dlinfo <- liftE $ getDownloadInfo GHC ver
  installGHCBindist dlinfo ver isoFilepath


-- | Like 'installCabalBin', except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installCabalBindist :: ( MonadMask m
                       , MonadCatch m
                       , MonadReader env m
                       , HasPlatformReq env
                       , HasDirs env
                       , HasSettings env
                       , MonadLogger m
                       , MonadResource m
                       , MonadIO m
                       , MonadUnliftIO m
                       , MonadFail m
                       )
                    => DownloadInfo
                    -> Version
                    -> Maybe FilePath -- ^ isolated install filepath, if user provides any.
                    -> Excepts
                         '[ AlreadyInstalled
                          , CopyError
                          , DigestError
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
installCabalBindist dlinfo ver isoFilepath = do
  lift $ $(logDebug) $ "Requested to install cabal version " <> prettyVer ver

  PlatformRequest {..} <- lift getPlatformReq
  Dirs {..} <- lift getDirs

  case isoFilepath of
    Nothing -> -- for regular install check if any previous versions installed
      whenM
        (lift (cabalInstalled ver) >>= \a -> liftIO $
          handleIO (\_ -> pure False)
            $ fmap (\x -> a && x)
            -- ignore when the installation is a legacy cabal (binary, not symlink)
            $ pathIsLink (binDir </> "cabal" <> exeExt)
        )
        (throwE $ AlreadyInstalled Cabal ver)

    _ -> pure () -- check isn't required in isolated installs

  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack <- lift withGHCupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)

  case isoFilepath of
    Just isoDir -> do             -- isolated install
      lift $ $(logInfo) $ "isolated installing Cabal to " <> T.pack isoDir
      liftE $ installCabalUnpacked workdir isoDir Nothing

    Nothing -> do                 -- regular install
      liftE $ installCabalUnpacked workdir binDir (Just ver)

      -- create symlink if this is the latest version for regular installs
      cVers <- lift $ fmap rights getInstalledCabals
      let lInstCabal = headMay . reverse . sort $ cVers
      when (maybe True (ver >=) lInstCabal) $ liftE $ setCabal ver

-- | Install an unpacked cabal distribution.
installCabalUnpacked :: (MonadLogger m, MonadCatch m, MonadIO m)
              => FilePath      -- ^ Path to the unpacked cabal bindist (where the executable resides)
              -> FilePath      -- ^ Path to install to
              -> Maybe Version -- ^ Nothing for isolated install
              -> Excepts '[CopyError, FileAlreadyExistsError] m ()
installCabalUnpacked path inst mver' = do
  lift $ $(logInfo) "Installing cabal"
  let cabalFile = "cabal"
  liftIO $ createDirRecursive' inst
  let destFileName = cabalFile
        <> maybe "" (("-" <>) . T.unpack . prettyVer) mver'
        <> exeExt
  let destPath = inst </> destFileName

  liftE $ throwIfFileAlreadyExists destPath
    
  handleIO (throwE . CopyError . show) $ liftIO $ copyFile
    (path </> cabalFile <> exeExt)
    destPath
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
                   , MonadLogger m
                   , MonadResource m
                   , MonadIO m
                   , MonadUnliftIO m
                   , MonadFail m
                   )
                => Version
                -> Maybe FilePath -- isolated install Path, if user provided any
                -> Excepts
                     '[ AlreadyInstalled
                      , CopyError
                      , DigestError
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
installCabalBin ver isoFilepath = do
  dlinfo <- liftE $ getDownloadInfo Cabal ver
  installCabalBindist dlinfo ver isoFilepath


-- | Like 'installHLSBin, except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installHLSBindist :: ( MonadMask m
                     , MonadCatch m
                     , MonadReader env m
                     , HasPlatformReq env
                     , HasDirs env
                     , HasSettings env
                     , MonadLogger m
                     , MonadResource m
                     , MonadIO m
                     , MonadUnliftIO m
                     , MonadFail m
                     )
                  => DownloadInfo
                  -> Version
                  -> Maybe FilePath -- ^ isolated install path, if user passed any
                  -> Excepts
                       '[ AlreadyInstalled
                        , CopyError
                        , DigestError
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
installHLSBindist dlinfo ver isoFilepath = do
  lift $ $(logDebug) $ "Requested to install hls version " <> prettyVer ver

  PlatformRequest {..} <- lift getPlatformReq
  Dirs {..} <- lift getDirs

  case isoFilepath of
    Nothing ->
      -- we only check for already installed in regular (non-isolated) installs
      whenM (lift (hlsInstalled ver))
      (throwE $ AlreadyInstalled HLS ver)

    _ -> pure ()

  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack <- lift withGHCupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)

  case isoFilepath of
    Just isoDir -> do
      lift $ $(logInfo) $ "isolated installing HLS to " <> T.pack isoDir
      liftE $ installHLSUnpacked workdir isoDir Nothing

    Nothing -> do
      liftE $ installHLSUnpacked workdir binDir (Just ver)

      -- create symlink if this is the latest version in a regular install
      hlsVers <- lift $ fmap rights getInstalledHLSs
      let lInstHLS = headMay . reverse . sort $ hlsVers
      when (maybe True (ver >=) lInstHLS) $ liftE $ setHLS ver


-- | Install an unpacked hls distribution.
installHLSUnpacked :: (MonadFail m, MonadLogger m, MonadCatch m, MonadIO m)
              => FilePath      -- ^ Path to the unpacked hls bindist (where the executable resides)
              -> FilePath      -- ^ Path to install to
              -> Maybe Version -- ^ Nothing for isolated install
              -> Excepts '[CopyError, FileAlreadyExistsError] m ()
installHLSUnpacked path inst mver' = do
  lift $ $(logInfo) "Installing HLS"
  liftIO $ createDirRecursive' inst

  -- install haskell-language-server-<ghcver>
  bins@(_:_) <- liftIO $ findFiles
    path
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^haskell-language-server-[0-9].*$|] :: ByteString)
    )
  forM_ bins $ \f -> do
    let toF = dropSuffix exeExt f
              <> maybe "" (("~" <>) . T.unpack . prettyVer) mver'
              <> exeExt

    let srcPath = path </> f
    let destPath = inst </> toF

    liftE $ throwIfFileAlreadyExists destPath
      
    handleIO (throwE . CopyError . show) $ liftIO $ copyFile
      srcPath
      destPath
    lift $ chmod_755 destPath

  -- install haskell-language-server-wrapper
  let wrapper = "haskell-language-server-wrapper"
      toF = wrapper
            <> maybe "" (("-" <>) . T.unpack . prettyVer) mver'
            <> exeExt
      srcWrapperPath = path </> wrapper <> exeExt
      destWrapperPath = inst </> toF

  liftE $ throwIfFileAlreadyExists destWrapperPath
      
  handleIO (throwE . CopyError . show) $ liftIO $ copyFile
    srcWrapperPath
    destWrapperPath
    
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
                 , MonadLogger m
                 , MonadResource m
                 , MonadIO m
                 , MonadUnliftIO m
                 , MonadFail m
                 )
              => Version
              -> Maybe FilePath
              -> Excepts
                   '[ AlreadyInstalled
                    , CopyError
                    , DigestError
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
installHLSBin ver isoFilepath = do
  dlinfo <- liftE $ getDownloadInfo HLS ver
  installHLSBindist dlinfo ver isoFilepath


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
                   , MonadLogger m
                   , MonadResource m
                   , MonadIO m
                   , MonadUnliftIO m
                   , MonadFail m
                   )
                => Version
                -> Maybe FilePath
                -> Excepts
                     '[ AlreadyInstalled
                      , CopyError
                      , DigestError
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
installStackBin ver isoFilepath = do
  dlinfo <- liftE $ getDownloadInfo Stack ver
  installStackBindist dlinfo ver isoFilepath


-- | Like 'installStackBin', except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installStackBindist :: ( MonadMask m
                       , MonadCatch m
                       , MonadReader env m
                       , HasPlatformReq env
                       , HasDirs env
                       , HasSettings env
                       , MonadLogger m
                       , MonadResource m
                       , MonadIO m
                       , MonadUnliftIO m
                       , MonadFail m
                       )
                    => DownloadInfo
                    -> Version
                    -> Maybe FilePath
                    -> Excepts
                         '[ AlreadyInstalled
                          , CopyError
                          , DigestError
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
installStackBindist dlinfo ver isoFilepath = do
  lift $ $(logDebug) $ "Requested to install stack version " <> prettyVer ver

  PlatformRequest {..} <- lift getPlatformReq
  Dirs {..} <- lift getDirs

  case isoFilepath of
    Nothing ->             -- check previous versions in case of regular installs
      whenM (lift (stackInstalled ver))
      (throwE $ AlreadyInstalled Stack ver)

    _ -> pure ()           -- don't do shit for isolates

  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack <- lift withGHCupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)

  case isoFilepath of
    Just isoDir -> do                 -- isolated install
      lift $ $(logInfo) $ "isolated installing Stack to " <> T.pack isoDir
      liftE $ installStackUnpacked workdir isoDir Nothing
    Nothing -> do                     -- regular install
      liftE $ installStackUnpacked workdir binDir (Just ver)

      -- create symlink if this is the latest version and a regular install
      sVers <- lift $ fmap rights getInstalledStacks
      let lInstStack = headMay . reverse . sort $ sVers
      when (maybe True (ver >=) lInstStack) $ liftE $ setStack ver


-- | Install an unpacked stack distribution.
installStackUnpacked :: (MonadLogger m, MonadCatch m, MonadIO m)
              => FilePath      -- ^ Path to the unpacked stack bindist (where the executable resides)
              -> FilePath      -- ^ Path to install to
              -> Maybe Version -- ^ Nothing for isolated installs
              -> Excepts '[CopyError, FileAlreadyExistsError] m ()
installStackUnpacked path inst mver' = do
  lift $ $(logInfo) "Installing stack"
  let stackFile = "stack"
  liftIO $ createDirRecursive' inst
  let destFileName = stackFile
                     <> maybe "" (("-" <>) .  T.unpack . prettyVer) mver'
                     <> exeExt
      destPath = inst </> destFileName

  liftE $ throwIfFileAlreadyExists destPath
      
  handleIO (throwE . CopyError . show) $ liftIO $ copyFile
    (path </> stackFile <> exeExt)
    destPath
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
          , MonadLogger m
          , MonadThrow m
          , MonadFail m
          , MonadIO m
          , MonadCatch m
          , MonadMask m
          , MonadUnliftIO m
          )
       => GHCTargetVersion
       -> SetGHC
       -> Excepts '[NotInstalled] m GHCTargetVersion
setGHC ver sghc = do
  let verS = T.unpack $ prettyVer (_tvVersion ver)
  ghcdir                        <- lift $ ghcupGHCDir ver

  whenM (lift $ not <$> ghcInstalled ver) (throwE (NotInstalled GHC ver))

  -- symlink destination
  Dirs {..} <- lift getDirs

  -- first delete the old symlinks (this fixes compatibility issues
  -- with old ghcup)
  case sghc of
    SetGHCOnly -> liftE $ rmPlain (_tvTarget ver)
    SetGHC_XY  -> liftE $ rmMajorSymlinks ver
    SetGHC_XYZ -> liftE $ rmMinorSymlinks ver

  -- for ghc tools (ghc, ghci, haddock, ...)
  verfiles <- ghcToolFiles ver
  forM_ verfiles $ \file -> do
    mTargetFile <- case sghc of
      SetGHCOnly -> pure $ Just file
      SetGHC_XY  -> do
        handle
            (\(e :: ParseError) -> lift $ $(logWarn) (T.pack $ displayException e) >> pure Nothing)
          $ do 
            (mj, mi) <- getMajorMinorV (_tvVersion ver)
            let major' = intToText mj <> "." <> intToText mi
            pure $ Just (file <> "-" <> T.unpack major')
      SetGHC_XYZ ->
        pure $ Just (file <> "-" <> verS)

    -- create symlink
    forM mTargetFile $ \targetFile -> do
      let fullF = binDir </> targetFile  <> exeExt
          fileWithExt = file <> exeExt
      destL <- lift $ ghcLinkDestination fileWithExt ver
      lift $ createLink destL fullF

  -- create symlink for share dir
  when (isNothing . _tvTarget $ ver) $ lift $ symlinkShareDir ghcdir verS

  pure ver

 where

  symlinkShareDir :: ( MonadReader env m
                     , HasDirs env
                     , MonadIO m
                     , MonadLogger m
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
        whenM (liftIO $ doesDirectoryExist fullsharedir) $ do
          let fullF   = destdir </> sharedir
          let targetF = "." </> "ghc" </> ver' </> sharedir
          $(logDebug) $ "rm -f " <> T.pack fullF
          hideError doesNotExistErrorType $ rmDirectoryLink fullF
          $(logDebug) $ "ln -s " <> T.pack targetF <> " " <> T.pack fullF
          liftIO
#if defined(IS_WINDOWS)
            -- On windows we need to be more permissive
            -- in case symlinks can't be created, be just
            -- give up here. This symlink isn't strictly necessary.
            $ hideError permissionErrorType
            $ hideError illegalOperationErrorType
#endif
            $ createDirectoryLink targetF fullF
      _ -> pure ()



-- | Set the @~\/.ghcup\/bin\/cabal@ symlink.
setCabal :: ( MonadMask m
            , MonadReader env m
            , HasDirs env
            , MonadLogger m
            , MonadThrow m
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




-- | Set the haskell-language-server symlinks.
setHLS :: ( MonadCatch m
          , MonadReader env m
          , HasDirs env
          , MonadLogger m
          , MonadThrow m
          , MonadFail m
          , MonadIO m
          , MonadMask m
          , MonadUnliftIO m
          )
       => Version
       -> Excepts '[NotInstalled] m ()
setHLS ver = do
  Dirs {..} <- lift getDirs

  -- Delete old symlinks, since these might have different ghc versions than the
  -- selected version, so we could end up with stray or incorrect symlinks.
  oldSyms <- lift hlsSymlinks
  forM_ oldSyms $ \f -> do
    lift $ $(logDebug) $ "rm " <> T.pack (binDir </> f)
    lift $ rmLink (binDir </> f)

  -- set haskell-language-server-<ghcver> symlinks
  bins <- lift $ hlsServerBinaries ver
  when (null bins) $ throwE $ NotInstalled HLS (GHCTargetVersion Nothing ver)

  forM_ bins $ \f -> do
    let destL = f
    let target = (<> exeExt) . head . splitOn "~" $ f
    lift $ createLink destL (binDir </> target)

  -- set haskell-language-server-wrapper symlink
  let destL = "haskell-language-server-wrapper-" <> T.unpack (prettyVer ver) <> exeExt
  let wrapper = binDir </> "haskell-language-server-wrapper" <> exeExt

  lift $ createLink destL wrapper

  pure ()


-- | Set the @~\/.ghcup\/bin\/stack@ symlink.
setStack :: ( MonadMask m
            , MonadReader env m
            , HasDirs env
            , MonadLogger m
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



    ------------------
    --[ List tools ]--
    ------------------


-- | Filter data type for 'listVersions'.
data ListCriteria = ListInstalled
                  | ListSet
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
                , MonadLogger m
                , MonadThrow m
                , MonadLogger m
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
               , MonadLogger m
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
        $(logWarn)
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  strayCabals :: ( MonadReader env m
                 , HasDirs env
                 , MonadCatch m
                 , MonadThrow m
                 , MonadLogger m
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
        $(logWarn)
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  strayHLS :: ( MonadReader env m
              , HasDirs env
              , MonadCatch m
              , MonadThrow m
              , MonadLogger m
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
        $(logWarn)
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  strayStacks :: ( MonadReader env m
                 , HasDirs env
                 , MonadCatch m
                 , MonadThrow m
                 , MonadLogger m
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
        $(logWarn)
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  currentGHCup :: Map.Map Version VersionInfo -> Maybe ListResult
  currentGHCup av =
    let currentVer = pvpToVersion ghcUpVer
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
  toListResult :: ( MonadLogger m
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
            , MonadLogger m
            , MonadIO m
            , MonadFail m
            , MonadCatch m
            , MonadMask m
            , MonadUnliftIO m
            )
         => GHCTargetVersion
         -> Excepts '[NotInstalled] m ()
rmGHCVer ver = do
  isSetGHC <- lift $ fmap (== Just ver) $ ghcSet (_tvTarget ver)

  whenM (lift $ fmap not $ ghcInstalled ver) (throwE (NotInstalled GHC ver))
  dir <- lift $ ghcupGHCDir ver

  -- this isn't atomic, order matters
  when isSetGHC $ do
    lift $ $(logInfo) "Removing ghc symlinks"
    liftE $ rmPlain (_tvTarget ver)

  lift $ $(logInfo) "Removing ghc-x.y.z symlinks"
  liftE $ rmMinorSymlinks ver

  lift $ $(logInfo) "Removing/rewiring ghc-x.y symlinks"
  -- first remove
  handle (\(_ :: ParseError) -> pure ()) $ liftE $ rmMajorSymlinks ver
  -- then fix them (e.g. with an earlier version)

  lift $ $(logInfo) $ "Removing directory recursively: " <> T.pack dir
  lift $ recyclePathForcibly dir

  v' <-
    handle
      (\(e :: ParseError) -> lift $ $(logWarn) (T.pack $ displayException e) >> pure Nothing)
    $ fmap Just
    $ getMajorMinorV (_tvVersion ver)
  forM_ v' $ \(mj, mi) -> lift (getGHCForMajor mj mi (_tvTarget ver))
    >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY)

  Dirs {..} <- lift getDirs

  lift $ hideError doesNotExistErrorType $ rmDirectoryLink (baseDir </> "share")


-- | Delete a cabal version. Will try to fix the @cabal@ symlink
-- after removal (e.g. setting it to an older version).
rmCabalVer :: ( MonadMask m
              , MonadReader env m
              , HasDirs env
              , MonadThrow m
              , MonadLogger m
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
            , MonadLogger m
            , MonadIO m
            , MonadFail m
            , MonadCatch m
            , MonadUnliftIO m
            )
         => Version
         -> Excepts '[NotInstalled] m ()
rmHLSVer ver = do
  whenM (lift $ fmap not $ hlsInstalled ver) $ throwE (NotInstalled HLS (GHCTargetVersion Nothing ver))

  isHlsSet      <- lift hlsSet

  Dirs {..} <- lift getDirs

  bins <- lift $ hlsAllBinaries ver
  forM_ bins $ \f -> lift $ recycleFile (binDir </> f)

  when (Just ver == isHlsSet) $ do
    -- delete all set symlinks
    oldSyms <- lift hlsSymlinks
    forM_ oldSyms $ \f -> do
      let fullF = binDir </> f
      lift $ $(logDebug) $ "rm " <> T.pack fullF
      lift $ rmLink fullF
    -- set latest hls
    hlsVers <- lift $ fmap rights getInstalledHLSs
    case headMay . reverse . sort $ hlsVers of
      Just latestver -> setHLS latestver
      Nothing        -> pure ()


-- | Delete a stack version. Will try to fix the @stack@ symlink
-- after removal (e.g. setting it to an older version).
rmStackVer :: ( MonadMask m
              , MonadReader env m
              , HasDirs env
              , MonadThrow m
              , MonadLogger m
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
           , MonadLogger m
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

  unless areEqualPaths $ $logWarn $ nonStandardInstallLocationMsg currentRunningExecPath

#if defined(IS_WINDOWS)
  -- since it doesn't seem possible to delete a running exe on windows
  -- we move it to temp dir, to be deleted at next reboot
  tempFilepath <- mkGhcupTmpDir
  hideError UnsupportedOperation $
            liftIO $ hideError NoSuchThing $
            Win32.moveFileEx ghcupFilepath (Just (tempFilepath </> "ghcup")) 0
#else
  -- delete it.
  hideError doesNotExistErrorType $ rmFile ghcupFilepath
#endif

  where
    handlePathNotPresent fp _err = do
      $logDebug $ "Error: The path does not exist, " <> T.pack fp
      pure fp

    nonStandardInstallLocationMsg path = T.pack $
      "current ghcup is invoked from a non-standard location: \n"
      <> path <>
      "\n you may have to uninstall it manually."

rmTool :: ( MonadReader env m
          , HasDirs env
          , MonadLogger m
          , MonadFail m
          , MonadMask m
          , MonadUnliftIO m)
          => ListResult
          -> Excepts '[NotInstalled ] m ()
rmTool ListResult {lVer, lTool, lCross} = do
  case lTool of
    GHC ->
      let ghcTargetVersion = GHCTargetVersion lCross lVer
      in rmGHCVer ghcTargetVersion
    HLS -> rmHLSVer lVer
    Cabal -> rmCabalVer lVer
    Stack -> rmStackVer lVer
    GHCup -> lift rmGhcup


rmGhcupDirs :: ( MonadReader env m
               , HasDirs env
               , MonadIO m
               , MonadLogger m
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
#if defined(IS_WINDOWS)
  $logInfo $ "removing " <> T.pack (baseDir </> "msys64")
  handleRm $ rmPathForcibly (baseDir </> "msys64")
#endif

  handleRm $ removeEmptyDirsRecursive baseDir

  -- report files in baseDir that are left-over after
  -- the standard location deletions above
  hideErrorDef [doesNotExistErrorType] [] $ reportRemainingFiles baseDir

  where
    handleRm :: (MonadCatch m, MonadLogger m)  => m () -> m ()
    handleRm = handleIO (\e -> $logDebug $ "Part of the cleanup action failed with error: " <> T.pack (displayException e) <> "\n"
                                <> "continuing regardless...")

    rmEnvFile :: (MonadLogger m, MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmEnvFile enFilePath = do
      $logInfo "Removing Ghcup Environment File"
      hideErrorDef [permissionErrorType] () $ deleteFile enFilePath

    rmConfFile :: (MonadLogger m, MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmConfFile confFilePath = do
      $logInfo "removing Ghcup Config File"
      hideErrorDef [permissionErrorType] () $ deleteFile confFilePath

    rmDir :: (MonadLogger m, MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmDir dir =
      -- 'getDirectoryContentsRecursive' is lazy IO. In case
      -- an error leaks through, we catch it here as well,
      -- althought 'deleteFile' should already handle it.
      hideErrorDef [doesNotExistErrorType] () $ do
        $logInfo $ "removing " <> T.pack dir
        contents <- liftIO $ getDirectoryContentsRecursive dir
        forM_ contents (deleteFile . (dir </>))

    rmBinDir :: (MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmBinDir binDir = do
#if !defined(IS_WINDOWS)
      isXDGStyle <- liftIO useXDG
      if not isXDGStyle
        then removeDirIfEmptyOrIsSymlink binDir
        else pure ()
#else
      removeDirIfEmptyOrIsSymlink binDir
#endif

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

    removeEmptyDirsRecursive :: (MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    removeEmptyDirsRecursive fp = do
      cs <- liftIO $ listDirectory fp >>= filterM doesDirectoryExist . fmap (fp </>)
      forM_ cs removeEmptyDirsRecursive
      hideError InappropriateType $ removeDirIfEmptyOrIsSymlink fp
        

    -- we expect only files inside cache/log dir
    -- we report remaining files/dirs later,
    -- hence the force/quiet mode in these delete functions below.

    deleteFile :: (MonadReader env m, HasDirs env, MonadMask m, MonadIO m) => FilePath -> m ()
    deleteFile filepath = do
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
          then deleteFile fp
          else liftIO $ ioError e



    ------------------
    --[ Debug info ]--
    ------------------


getDebugInfo :: ( Alternative m
                , MonadFail m
                , MonadReader env m
                , HasDirs env
                , MonadLogger m
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
              , MonadLogger m
              , MonadIO m
              , MonadUnliftIO m
              , MonadFail m
              )
           => Either GHCTargetVersion GitBranch          -- ^ version to install
           -> Maybe Version            -- ^ overwrite version
           -> Either Version FilePath  -- ^ version to bootstrap with
           -> Maybe Int                -- ^ jobs
           -> Maybe FilePath           -- ^ build config
           -> Maybe FilePath           -- ^ patch directory
           -> [Text]                   -- ^ additional args to ./configure
           -> Maybe String             -- ^ build flavour
           -> Bool
           -> Maybe FilePath           -- ^ isolate dir
           -> Excepts
                '[ AlreadyInstalled
                 , BuildFailed
                 , DigestError
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
                 ]
                m
                GHCTargetVersion
compileGHC targetGhc ov bstrap jobs mbuildConfig patchdir aargs buildFlavour hadrian isolateDir
  = do
    PlatformRequest { .. } <- lift getPlatformReq
    GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo

    (workdir, tmpUnpack, tver) <- case targetGhc of
      -- unpack from version tarball
      Left tver -> do
        lift $ $(logDebug) $ "Requested to compile: " <> tVerToText tver <> " with " <> either prettyVer T.pack bstrap

        -- download source tarball
        dlInfo <-
          preview (ix GHC % ix (tver ^. tvVersion) % viSourceDL % _Just) dls
            ?? NoDownload
        dl <- liftE $ downloadCached dlInfo Nothing

        -- unpack
        tmpUnpack <- lift mkGhcupTmpDir
        liftE $ unpackToDir tmpUnpack dl
        liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack

        workdir <- maybe (pure tmpUnpack)
                         (liftE . intoSubdir tmpUnpack)
                         (view dlSubdir dlInfo)

        pure (workdir, tmpUnpack, tver)

      -- clone from git
      Right GitBranch{..} -> do
        tmpUnpack <- lift mkGhcupTmpDir
        let git args = execLogged "git" ("--no-pager":args) (Just tmpUnpack) "git" Nothing
        tver <- reThrowAll @_ @'[ProcessError] DownloadFailed $ do
          let rep = fromMaybe "https://gitlab.haskell.org/ghc/ghc.git" repo
          lift $ $(logInfo) $ "Fetching git repo " <> T.pack rep <> " at ref " <> T.pack ref <> " (this may take a while)"
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
          lEM $ execLogged "python3" ["./boot"] (Just tmpUnpack) "ghc-bootstrap" Nothing
          lEM $ execLogged "sh" ["./configure"] (Just tmpUnpack) "ghc-bootstrap" Nothing
          CapturedProcess {..} <- lift $ makeOut
            ["show!", "--quiet", "VALUE=ProjectVersion" ] (Just tmpUnpack)
          case _exitCode of
            ExitSuccess -> throwEither . MP.parse ghcProjectVersion "" . decUTF8Safe' $ _stdOut
            ExitFailure c -> fail ("Could not figure out GHC project version. Exit code was: " <> show c <> ". Error was: " <> T.unpack (decUTF8Safe' _stdErr))

        liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform tmpUnpack
        lift $ $(logInfo) $ "Git version " <> T.pack ref <> " corresponds to GHC version " <> prettyVer tver

        pure (tmpUnpack, tmpUnpack, GHCTargetVersion Nothing tver)
    -- the version that's installed may differ from the
    -- compiled version, so the user can overwrite it
    let installVer = maybe tver (\ov' -> tver { _tvVersion = ov' }) ov

    alreadyInstalled <- lift $ ghcInstalled installVer
    alreadySet <- fmap (== Just tver) $ lift $ ghcSet (_tvTarget tver)
    when alreadyInstalled $ do
      case isolateDir of
        Just isoDir ->
          lift $ $(logWarn) $ "GHC " <> T.pack (prettyShow tver) <> " already installed. Isolate installing to " <> T.pack isoDir
        Nothing ->
          lift $ $(logWarn) $ "GHC " <> T.pack (prettyShow tver) <> " already installed. Will overwrite existing version."
      lift $ $(logWarn)
        "...waiting for 10 seconds before continuing, you can still abort..."
      liftIO $ threadDelay 10000000 -- give the user a sec to intervene

    ghcdir <- case isolateDir of
      Just isoDir -> pure isoDir
      Nothing -> lift $ ghcupGHCDir installVer

    bghc <- case bstrap of
      Right g    -> pure $ Right g
      Left  bver -> pure $ Left ("ghc-" <> (T.unpack . prettyVer $ bver) <> exeExt)

    (mBindist, bmk) <- liftE $ runBuildAction
      tmpUnpack
      Nothing
      (do
        b <- if hadrian
             then compileHadrianBindist bghc tver workdir ghcdir
             else compileMakeBindist bghc tver workdir ghcdir
        bmk <- liftIO $ handleIO (\_ -> pure "") $ B.readFile (build_mk workdir)
        pure (b, bmk)
      )

    case isolateDir of
      Nothing ->
        -- only remove old ghc in regular installs
        when alreadyInstalled $ do
          lift $ $(logInfo) "Deleting existing installation"
          liftE $ rmGHCVer tver
          
      _ -> pure ()

    forM_ mBindist $ \bindist -> do
      liftE $ installPackedGHC bindist
                               (Just $ RegexDir "ghc-.*")
                               ghcdir
                               (tver ^. tvVersion)

    liftIO $ B.writeFile (ghcdir </> ghcUpSrcBuiltFile) bmk
    
    case isolateDir of
      -- set and make symlinks for regular (non-isolated) installs
      Nothing -> do
        reThrowAll GHCupSetError $ postGHCInstall tver
        -- restore
        when alreadySet $ liftE $ void $ setGHC tver SetGHCOnly
        
      _ -> pure ()

    pure tver

 where
  defaultConf = case targetGhc of
    Left (GHCTargetVersion (Just _) _) -> [s|
V=0
BUILD_MAN = NO
BUILD_SPHINX_HTML = NO
BUILD_SPHINX_PDF = NO
HADDOCK_DOCS = NO
ifneq "$(BuildFlavour)" ""
include mk/flavours/$(BuildFlavour).mk
endif
Stage1Only = YES|]
    _ -> [s|
V=0
BUILD_MAN = NO
BUILD_SPHINX_HTML = NO
BUILD_SPHINX_PDF = NO
HADDOCK_DOCS = YES
ifneq "$(BuildFlavour)" ""
include mk/flavours/$(BuildFlavour).mk
endif|]

  compileHadrianBindist :: ( MonadReader env m
                           , HasDirs env
                           , HasSettings env
                           , HasPlatformReq env
                           , MonadThrow m
                           , MonadCatch m
                           , MonadLogger m
                           , MonadIO m
                           , MonadFail m
                           )
                        => Either FilePath FilePath
                        -> GHCTargetVersion
                        -> FilePath
                        -> FilePath
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
  compileHadrianBindist bghc tver workdir ghcdir = do
    lEM $ execLogged "python3" ["./boot"] (Just workdir) "ghc-bootstrap" Nothing

    liftE $ configureBindist bghc tver workdir ghcdir

    lift $ $(logInfo) "Building (this may take a while)..."
    hadrian_build <- liftE $ findHadrianFile workdir
    lEM $ execLogged hadrian_build
                          ( maybe [] (\j  -> ["-j" <> show j]         ) jobs
                         ++ maybe [] (\bf -> ["--flavour=" <> bf]) buildFlavour
                         ++ ["binary-dist"]
                          )
                          (Just workdir) "ghc-make" Nothing
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
#if defined(IS_WINDOWS)
    let possible_files = ((workdir </> "hadrian") </>) <$> ["build.bat"]
#else   
    let possible_files = ((workdir </> "hadrian") </>) <$> ["build", "build.sh"]
#endif
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
                        , MonadLogger m
                        , MonadIO m
                        , MonadFail m
                        )
                     => Either FilePath FilePath
                     -> GHCTargetVersion
                     -> FilePath
                     -> FilePath
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
  compileMakeBindist bghc tver workdir ghcdir = do
    liftE $ configureBindist bghc tver workdir ghcdir

    case mbuildConfig of
      Just bc -> liftIOException
        doesNotExistErrorType
        (FileDoesNotExistError bc)
        (liftIO $ copyFile bc (build_mk workdir))
      Nothing ->
        liftIO $ T.writeFile (build_mk workdir) (addBuildFlavourToConf defaultConf)

    liftE $ checkBuildConfig (build_mk workdir)

    lift $ $(logInfo) "Building (this may take a while)..."
    lEM $ make (maybe [] (\j -> ["-j" <> fS (show j)]) jobs) (Just workdir)

    if | isCross tver -> do
          lift $ $(logInfo) "Installing cross toolchain..."
          lEM $ make ["install"] (Just workdir)
          pure Nothing
       | otherwise -> do
          lift $ $(logInfo) "Creating bindist..."
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
                 , MonadLogger m
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
    handleIO (throwE . CopyError . show) $ liftIO $ copyFile (workdir </> tar)
                                                             tarPath
    lift $ $(logInfo) $ "Copied bindist to " <> T.pack tarPath
    pure tarPath

  checkBuildConfig :: (MonadCatch m, MonadIO m, MonadLogger m)
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
        lift $ $(logWarn) $ "Customly specified build config overwrites --flavour=" <> T.pack bf <> " switch! Waiting 5 seconds..."
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
                      , MonadLogger m
                      , MonadIO m
                      , MonadFail m
                      )
                   => Either FilePath FilePath
                   -> GHCTargetVersion
                   -> FilePath
                   -> FilePath
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
  configureBindist bghc tver workdir ghcdir = do
    lift $ $(logInfo) [s|configuring build|]
    
    forM_ patchdir (\dir -> liftE $ applyPatches dir workdir)

    cEnv <- liftIO getEnvironment

    if | _tvVersion tver >= [vver|8.8.0|] -> do
          bghcPath <- case bghc of
            Right ghc' -> pure ghc'
            Left  bver -> do
              spaths <- liftIO getSearchPath
              liftIO (searchPath spaths bver) !? NotFoundInPATH bver
          lEM $ execLogged
            "sh"
            ("./configure" :  maybe mempty
                      (\x -> ["--target=" <> T.unpack x])
                      (_tvTarget tver)
            ++ ["--prefix=" <> ghcdir]
#if defined(IS_WINDOWS)
            ++ ["--enable-tarballs-autodownload"]
#endif
            ++ fmap T.unpack aargs
            )
            (Just workdir)
            "ghc-conf"
            (Just (("GHC", bghcPath) : cEnv))
       | otherwise -> do
        lEM $ execLogged
          "sh"
          (  [ "./configure", "--with-ghc=" <> either id id bghc
             ]
          ++ maybe mempty
                   (\x -> ["--target=" <> T.unpack x])
                   (_tvTarget tver)
          ++ ["--prefix=" <> ghcdir]
#if defined(IS_WINDOWS)
          ++ ["--enable-tarballs-autodownload"]
#endif
          ++ fmap T.unpack aargs
          )
          (Just workdir)
          "ghc-conf"
          (Just cEnv)
    pure ()






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
                , MonadLogger m
                , MonadThrow m
                , MonadResource m
                , MonadIO m
                , MonadUnliftIO m
                )
             => Maybe FilePath    -- ^ full file destination to write ghcup into
             -> Bool              -- ^ whether to force update regardless
                                  --   of currently installed version
             -> Excepts
                  '[ CopyError
                   , DigestError
                   , DownloadFailed
                   , NoDownload
                   , NoUpdate
                   ]
                  m
                  Version
upgradeGHCup mtarget force' = do
  Dirs {..} <- lift getDirs
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo

  lift $ $(logInfo) "Upgrading GHCup..."
  let latestVer = fromJust $ fst <$> getLatest dls GHCup
  when (not force' && (latestVer <= pvpToVersion ghcUpVer)) $ throwE NoUpdate
  dli   <- liftE $ getDownloadInfo GHCup latestVer
  tmp   <- lift withGHCupTmpDir
  let fn = "ghcup" <> exeExt
  p <- liftE $ download (_dlUri dli) (Just (_dlHash dli)) tmp (Just fn) False
  let destDir = takeDirectory destFile
      destFile = fromMaybe (binDir </> fn) mtarget
  lift $ $(logDebug) $ "mkdir -p " <> T.pack destDir
  liftIO $ createDirRecursive' destDir
  lift $ $(logDebug) $ "rm -f " <> T.pack destFile
  lift $ hideError NoSuchThing $ recycleFile destFile
  lift $ $(logDebug) $ "cp " <> T.pack p <> " " <> T.pack destFile
  handleIO (throwE . CopyError . show) $ liftIO $ copyFile p
                                                           destFile
  lift $ chmod_755 destFile

  liftIO (isInPath destFile) >>= \b -> unless b $
    lift $ $(logWarn) $ T.pack (takeFileName destFile) <> " is not in PATH! You have to add it in order to use ghcup."
  liftIO (isShadowed destFile) >>= \case
    Nothing -> pure ()
    Just pa -> lift $ $(logWarn) $ "ghcup is shadowed by "
      <> T.pack pa
      <> ". The upgrade will not be in effect, unless you remove "
      <> T.pack pa
      <> " or make sure "
      <> T.pack destDir
      <> " comes before "
      <> T.pack (takeFileName pa)
      <> " in PATH."

  pure latestVer



    -------------
    --[ Other ]--
    -------------



-- | Creates @ghc-x.y.z@ and @ghc-x.y@ symlinks. This is used for
-- both installing from source and bindist.
postGHCInstall :: ( MonadReader env m
                  , HasDirs env
                  , MonadLogger m
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
  void $ liftE $ setGHC ver SetGHC_XYZ

  -- Create ghc-x.y symlinks. This may not be the current
  -- version, create it regardless.
  v' <-
    handle (\(e :: ParseError) -> lift $ $(logWarn) (T.pack $ displayException e) >> pure Nothing)
    $ fmap Just
    $ getMajorMinorV _tvVersion
  forM_ v' $ \(mj, mi) -> lift (getGHCForMajor mj mi _tvTarget)
    >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY)


-- | Reports the binary location of a given tool:
--
--   * for GHC, this reports: @~\/.ghcup\/ghc\/\<ver\>\/bin\/ghc@
--   * for cabal, this reports @~\/.ghcup\/bin\/cabal-\<ver\>@
--   * for hls, this reports @~\/.ghcup\/bin\/haskell-language-server-wrapper-\<ver\>@
--   * for stack, this reports @~\/.ghcup\/bin\/stack-\<ver\>@
--   * for ghcup, this reports the location of the currently running executable
whereIsTool :: ( MonadReader env m
               , HasDirs env
               , MonadLogger m
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
      pure (binDir dirs </> "haskell-language-server-wrapper-" <> T.unpack (prettyVer _tvVersion) <> exeExt)

    Stack -> do
      whenM (lift $ fmap not $ stackInstalled _tvVersion)
        $ throwE (NotInstalled Stack (GHCTargetVersion Nothing _tvVersion))
      pure (binDir dirs </> "stack-" <> T.unpack (prettyVer _tvVersion) <> exeExt)
    GHCup -> do
      currentRunningExecPath <- liftIO getExecutablePath
      liftIO $ canonicalizePath currentRunningExecPath


throwIfFileAlreadyExists :: ( MonadIO m ) =>
                            FilePath ->
                            Excepts '[FileAlreadyExistsError] m ()

throwIfFileAlreadyExists fp = whenM (checkFileAlreadyExists fp)
                                (throwE $ FileAlreadyExistsError fp)

