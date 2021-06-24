{-# LANGUAGE CPP                   #-}
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

#if !defined(TAR)
import           Codec.Archive                  ( ArchiveResult )
#endif
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Data.ByteString                ( ByteString )
import           Data.Either
import           Data.List
import           Data.List.Extra
import           Data.Maybe
import           Data.String                    ( fromString )
import           Data.String.Interpolate
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
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           Text.Regex.Posix

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
#if defined(IS_WINDOWS)
import qualified System.Win32.File             as Win32
#endif
import qualified Text.Megaparsec               as MP
import GHCup.Utils.MegaParsec
import Control.Concurrent (threadDelay)



    -------------------------
    --[ Tool installation ]--
    -------------------------


-- | Like 'installGHCBin', except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installGHCBindist :: ( MonadFail m
                     , MonadMask m
                     , MonadCatch m
                     , MonadReader AppState m
                     , MonadLogger m
                     , MonadResource m
                     , MonadIO m
                     , MonadUnliftIO m
                     )
                  => DownloadInfo    -- ^ where/how to download
                  -> Version         -- ^ the version to install
                  -> Excepts
                       '[ AlreadyInstalled
                        , BuildFailed
                        , DigestError
                        , DownloadFailed
                        , NoDownload
                        , NotInstalled
                        , UnknownArchive
                        , TarDirDoesNotExist
#if !defined(TAR)
                        , ArchiveResult
#endif
                        ]
                       m
                       ()
installGHCBindist dlinfo ver = do
  AppState { dirs , settings } <- lift ask

  let tver = mkTVer ver
  lift $ $(logDebug) [i|Requested to install GHC with #{ver}|]
  whenM (lift $ ghcInstalled tver) (throwE $ AlreadyInstalled GHC ver)

  -- download (or use cached version)
  dl                           <- liftE $ downloadCached settings dirs dlinfo Nothing

  -- prepare paths
  ghcdir <- lift $ ghcupGHCDir tver

  toolchainSanityChecks
  
  liftE $ installPackedGHC dl (view dlSubdir dlinfo) ghcdir ver

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
                    , MonadReader AppState m
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
#if !defined(TAR)
                       , ArchiveResult
#endif
                       ] m ()
installPackedGHC dl msubdir inst ver = do
  AppState { pfreq = PlatformRequest {..} } <- lift ask

  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ lift $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack)
                   (liftE . intoSubdir tmpUnpack)
                   msubdir

  liftE $ runBuildAction tmpUnpack
                         (Just inst)
                         (installUnpackedGHC workdir inst ver)


-- | Install an unpacked GHC distribution. This only deals with the GHC
-- build system and nothing else.
installUnpackedGHC :: ( MonadReader AppState m
                      , MonadThrow m
                      , MonadLogger m
                      , MonadIO m
                      )
                   => FilePath      -- ^ Path to the unpacked GHC bindist (where the configure script resides)
                   -> FilePath      -- ^ Path to install to
                   -> Version       -- ^ The GHC version
                   -> Excepts '[ProcessError] m ()
#if defined(IS_WINDOWS)
installUnpackedGHC path inst _ = do
  lift $ $(logInfo) "Installing GHC (this may take a while)"
  -- windows bindists are relocatable and don't need
  -- to run configure
  liftIO $ copyDirectoryRecursive path inst
#else
installUnpackedGHC path inst ver = do
  AppState { pfreq = PlatformRequest {..} } <- lift ask

  let alpineArgs
       | ver >= [vver|8.2.2|], Linux Alpine <- _rPlatform
       = ["--disable-ld-override"]
       | otherwise
       = []

  lift $ $(logInfo) "Installing GHC (this may take a while)"
  lEM $ execLogged "sh"
                   ("./configure" : ("--prefix=" <> inst) 
#if defined(IS_WINDOWS)
                    : "--enable-tarballs-autodownload"
#endif
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
                 , MonadReader AppState m
                 , MonadLogger m
                 , MonadResource m
                 , MonadIO m
                 , MonadUnliftIO m
                 )
              => Version         -- ^ the version to install
              -> Excepts
                   '[ AlreadyInstalled
                    , BuildFailed
                    , DigestError
                    , DownloadFailed
                    , NoDownload
                    , NotInstalled
                    , UnknownArchive
                    , TarDirDoesNotExist
#if !defined(TAR)
                    , ArchiveResult
#endif
                    ]
                   m
                   ()
installGHCBin ver = do
  AppState { pfreq
           , ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- lift ask
  dlinfo <- lE $ getDownloadInfo GHC ver pfreq dls
  installGHCBindist dlinfo ver


-- | Like 'installCabalBin', except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installCabalBindist :: ( MonadMask m
                       , MonadCatch m
                       , MonadReader AppState m
                       , MonadLogger m
                       , MonadResource m
                       , MonadIO m
                       , MonadUnliftIO m
                       , MonadFail m
                       )
                    => DownloadInfo
                    -> Version
                    -> Excepts
                         '[ AlreadyInstalled
                          , CopyError
                          , DigestError
                          , DownloadFailed
                          , NoDownload
                          , NotInstalled
                          , UnknownArchive
                          , TarDirDoesNotExist
#if !defined(TAR)
                          , ArchiveResult
#endif
                          ]
                         m
                         ()
installCabalBindist dlinfo ver = do
  lift $ $(logDebug) [i|Requested to install cabal version #{ver}|]

  AppState { dirs = dirs@Dirs {..}
           , pfreq = PlatformRequest {..}
           , settings } <- lift ask

  whenM
      (lift (cabalInstalled ver) >>= \a -> liftIO $
        handleIO (\_ -> pure False)
          $ fmap (\x -> a && x)
          -- ignore when the installation is a legacy cabal (binary, not symlink)
          $ pathIsLink (binDir </> "cabal" <> exeExt)
      )
      (throwE $ AlreadyInstalled Cabal ver)

  -- download (or use cached version)
  dl                           <- liftE $ downloadCached settings dirs dlinfo Nothing

  -- unpack
  tmpUnpack                    <- lift withGHCupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ lift $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)

  liftE $ installCabal' workdir binDir

  -- create symlink if this is the latest version
  cVers <- lift $ fmap rights getInstalledCabals
  let lInstCabal = headMay . reverse . sort $ cVers
  when (maybe True (ver >=) lInstCabal) $ liftE $ setCabal ver

 where
  -- | Install an unpacked cabal distribution.
  installCabal' :: (MonadLogger m, MonadCatch m, MonadIO m)
                => FilePath      -- ^ Path to the unpacked cabal bindist (where the executable resides)
                -> FilePath      -- ^ Path to install to
                -> Excepts '[CopyError] m ()
  installCabal' path inst = do
    lift $ $(logInfo) "Installing cabal"
    let cabalFile = "cabal"
    liftIO $ createDirRecursive' inst
    let destFileName = cabalFile <> "-" <> T.unpack (prettyVer ver) <> exeExt
    let destPath = inst </> destFileName
    handleIO (throwE . CopyError . show) $ liftIO $ copyFile
      (path </> cabalFile <> exeExt)
      destPath
    lift $ chmod_755 destPath


-- | Installs cabal into @~\/.ghcup\/bin/cabal-\<ver\>@ and
-- creates a default @cabal -> cabal-x.y.z.q@ symlink for
-- the latest installed version.
installCabalBin :: ( MonadMask m
                   , MonadCatch m
                   , MonadReader AppState m
                   , MonadLogger m
                   , MonadResource m
                   , MonadIO m
                   , MonadUnliftIO m
                   , MonadFail m
                   )
                => Version
                -> Excepts
                     '[ AlreadyInstalled
                      , CopyError
                      , DigestError
                      , DownloadFailed
                      , NoDownload
                      , NotInstalled
                      , UnknownArchive
                      , TarDirDoesNotExist
#if !defined(TAR)
                      , ArchiveResult
#endif
                      ]
                     m
                     ()
installCabalBin ver = do
  AppState { pfreq
           , ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- lift ask
  dlinfo <- lE $ getDownloadInfo Cabal ver pfreq dls
  installCabalBindist dlinfo ver


-- | Like 'installHLSBin, except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installHLSBindist :: ( MonadMask m
                     , MonadCatch m
                     , MonadReader AppState m
                     , MonadLogger m
                     , MonadResource m
                     , MonadIO m
                     , MonadUnliftIO m
                     , MonadFail m
                     )
                  => DownloadInfo
                  -> Version
                  -> Excepts
                       '[ AlreadyInstalled
                        , CopyError
                        , DigestError
                        , DownloadFailed
                        , NoDownload
                        , NotInstalled
                        , UnknownArchive
                        , TarDirDoesNotExist
#if !defined(TAR)
                        , ArchiveResult
#endif
                        ]
                       m
                       ()
installHLSBindist dlinfo ver = do
  lift $ $(logDebug) [i|Requested to install hls version #{ver}|]

  AppState { dirs = dirs@Dirs {..}
           , pfreq = PlatformRequest {..}
           , settings } <- lift ask

  whenM (lift (hlsInstalled ver))
    (throwE $ AlreadyInstalled HLS ver)

  -- download (or use cached version)
  dl                           <- liftE $ downloadCached settings dirs dlinfo Nothing

  -- unpack
  tmpUnpack                    <- lift withGHCupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ lift $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)

  liftE $ installHLS' workdir binDir

  -- create symlink if this is the latest version
  hlsVers <- lift $ fmap rights getInstalledHLSs
  let lInstHLS = headMay . reverse . sort $ hlsVers
  when (maybe True (ver >=) lInstHLS) $ liftE $ setHLS ver

 where
  -- | Install an unpacked hls distribution.
  installHLS' :: (MonadFail m, MonadLogger m, MonadCatch m, MonadIO m)
                => FilePath      -- ^ Path to the unpacked hls bindist (where the executable resides)
                -> FilePath      -- ^ Path to install to
                -> Excepts '[CopyError] m ()
  installHLS' path inst = do
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
                <> "~" <> T.unpack (prettyVer ver) <> exeExt
      handleIO (throwE . CopyError . show) $ liftIO $ copyFile
        (path </> f)
        (inst </> toF)
      lift $ chmod_755 (inst </> toF)

    -- install haskell-language-server-wrapper
    let wrapper = "haskell-language-server-wrapper"
        toF = wrapper <> "-" <> T.unpack (prettyVer ver) <> exeExt
    handleIO (throwE . CopyError . show) $ liftIO $ copyFile
      (path </> wrapper <> exeExt)
      (inst </> toF)
    lift $ chmod_755 (inst </> toF)


-- | Installs hls binaries @haskell-language-server-\<ghcver\>@
-- into @~\/.ghcup\/bin/@, as well as @haskell-languager-server-wrapper@.
installHLSBin :: ( MonadMask m
                 , MonadCatch m
                 , MonadReader AppState m
                 , MonadLogger m
                 , MonadResource m
                 , MonadIO m
                 , MonadUnliftIO m
                 , MonadFail m
                 )
              => Version
              -> Excepts
                   '[ AlreadyInstalled
                    , CopyError
                    , DigestError
                    , DownloadFailed
                    , NoDownload
                    , NotInstalled
                    , UnknownArchive
                    , TarDirDoesNotExist
#if !defined(TAR)
                    , ArchiveResult
#endif
                    ]
                   m
                   ()
installHLSBin ver = do
  AppState { pfreq
           , ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- lift ask
  dlinfo <- lE $ getDownloadInfo HLS ver pfreq dls
  installHLSBindist dlinfo ver


-- | Installs stack into @~\/.ghcup\/bin/stack-\<ver\>@ and
-- creates a default @stack -> stack-x.y.z.q@ symlink for
-- the latest installed version.
installStackBin :: ( MonadMask m
                   , MonadCatch m
                   , MonadReader AppState m
                   , MonadLogger m
                   , MonadResource m
                   , MonadIO m
                   , MonadUnliftIO m
                   , MonadFail m
                   )
                => Version
                -> Excepts
                     '[ AlreadyInstalled
                      , CopyError
                      , DigestError
                      , DownloadFailed
                      , NoDownload
                      , NotInstalled
                      , UnknownArchive
                      , TarDirDoesNotExist
#if !defined(TAR)
                      , ArchiveResult
#endif
                      ]
                     m
                     ()
installStackBin ver = do
  AppState { pfreq, ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- lift ask
  dlinfo <- lE $ getDownloadInfo Stack ver pfreq dls
  installStackBindist dlinfo ver


-- | Like 'installStackBin', except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installStackBindist :: ( MonadMask m
                       , MonadCatch m
                       , MonadReader AppState m
                       , MonadLogger m
                       , MonadResource m
                       , MonadIO m
                       , MonadUnliftIO m
                       , MonadFail m
                       )
                    => DownloadInfo
                    -> Version
                    -> Excepts
                         '[ AlreadyInstalled
                          , CopyError
                          , DigestError
                          , DownloadFailed
                          , NoDownload
                          , NotInstalled
                          , UnknownArchive
                          , TarDirDoesNotExist
#if !defined(TAR)
                          , ArchiveResult
#endif
                          ]
                         m
                         ()
installStackBindist dlinfo ver = do
  lift $ $(logDebug) [i|Requested to install stack version #{ver}|]

  AppState { dirs = dirs@Dirs {..}
           , pfreq = PlatformRequest {..}
           , settings
           } <- lift ask

  whenM (lift (hlsInstalled ver))
    (throwE $ AlreadyInstalled Stack ver)

  -- download (or use cached version)
  dl                           <- liftE $ downloadCached settings dirs dlinfo Nothing

  -- unpack
  tmpUnpack                    <- lift withGHCupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ lift $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)

  liftE $ installStack' workdir binDir

  -- create symlink if this is the latest version
  sVers <- lift $ fmap rights getInstalledStacks
  let lInstStack = headMay . reverse . sort $ sVers
  when (maybe True (ver >=) lInstStack) $ liftE $ setStack ver

 where
  -- | Install an unpacked stack distribution.
  installStack' :: (MonadLogger m, MonadCatch m, MonadIO m)
                => FilePath      -- ^ Path to the unpacked stack bindist (where the executable resides)
                -> FilePath      -- ^ Path to install to
                -> Excepts '[CopyError] m ()
  installStack' path inst = do
    lift $ $(logInfo) "Installing stack"
    let stackFile = "stack"
    liftIO $ createDirRecursive' inst
    let destFileName = stackFile <> "-" <> T.unpack (prettyVer ver) <> exeExt
    let destPath = inst </> destFileName
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
setGHC :: ( MonadReader AppState m
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
  AppState { dirs = Dirs {..} } <- lift ask

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
            (\(e :: ParseError) -> lift $ $(logWarn) [i|#{e}|] >> pure Nothing)
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

  symlinkShareDir :: (MonadReader AppState m, MonadIO m, MonadLogger m)
                  => FilePath
                  -> String
                  -> m ()
  symlinkShareDir ghcdir ver' = do
    AppState { dirs = Dirs {..} } <- ask
    let destdir = baseDir
    case sghc of
      SetGHCOnly -> do
        let sharedir     = "share"
        let fullsharedir = ghcdir </> sharedir
        whenM (liftIO $ doesDirectoryExist fullsharedir) $ do
          let fullF   = destdir </> sharedir
          let targetF = "." </> "ghc" </> ver' </> sharedir
          $(logDebug) [i|rm -f #{fullF}|]
          liftIO $ hideError doesNotExistErrorType $ removeDirectoryLink fullF
          $(logDebug) [i|ln -s #{targetF} #{fullF}|]
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
            , MonadReader AppState m
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
  AppState {dirs = Dirs {..}} <- lift ask

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
          , MonadReader AppState m
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
  AppState { dirs = Dirs {..} } <- lift ask

  -- Delete old symlinks, since these might have different ghc versions than the
  -- selected version, so we could end up with stray or incorrect symlinks.
  oldSyms <- lift hlsSymlinks
  forM_ oldSyms $ \f -> do
    lift $ $(logDebug) [i|rm #{binDir </> f}|]
    liftIO $ rmLink (binDir </> f)

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
            , MonadReader AppState m
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
  AppState {dirs = Dirs {..}} <- lift ask

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
availableToolVersions :: GHCupDownloads -> Tool -> Map.Map Version [Tag]
availableToolVersions av tool = view
  (at tool % non Map.empty % to (fmap _viTags))
  av


-- | List all versions from the download info, as well as stray
-- versions.
listVersions :: ( MonadCatch m
                , MonadLogger m
                , MonadThrow m
                , MonadLogger m
                , MonadIO m
                , MonadReader AppState m
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
        AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask
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
            slr <- strayHLS avTools
            pure (sort (slr ++ lr))
          Stack -> do
            slr <- strayStacks avTools
            pure (sort (slr ++ lr))
          GHCup -> pure lr
      Nothing -> do
        ghcvers   <- go (Just GHC) cSet cabals hlsSet' hlses sSet stacks
        cabalvers <- go (Just Cabal) cSet cabals hlsSet' hlses sSet stacks
        hlsvers   <- go (Just HLS) cSet cabals hlsSet' hlses sSet stacks
        ghcupvers <- go (Just GHCup) cSet cabals hlsSet' hlses sSet stacks
        stackvers <- go (Just Stack) cSet cabals hlsSet' hlses sSet stacks
        pure (ghcvers <> cabalvers <> hlsvers <> stackvers <> ghcupvers)
  strayGHCs :: (MonadCatch m, MonadReader AppState m, MonadThrow m, MonadLogger m, MonadIO m)
            => Map.Map Version [Tag]
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
          [i|Could not parse version of stray directory #{e}|]
        pure Nothing

  strayCabals :: (MonadReader AppState m, MonadCatch m, MonadThrow m, MonadLogger m, MonadIO m)
            => Map.Map Version [Tag]
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
          [i|Could not parse version of stray directory #{e}|]
        pure Nothing

  strayHLS :: (MonadReader AppState m, MonadCatch m, MonadThrow m, MonadLogger m, MonadIO m)
           => Map.Map Version [Tag]
           -> m [ListResult]
  strayHLS avTools = do
    hlss <- getInstalledHLSs
    fmap catMaybes $ forM hlss $ \case
      Right ver ->
        case Map.lookup ver avTools of
          Just _  -> pure Nothing
          Nothing -> do
            lSet    <- fmap (== Just ver) hlsSet
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
          [i|Could not parse version of stray directory #{e}|]
        pure Nothing

  strayStacks :: (MonadReader AppState m, MonadCatch m, MonadThrow m, MonadLogger m, MonadIO m)
              => Map.Map Version [Tag]
              -> m [ListResult]
  strayStacks avTools = do
    stacks <- getInstalledStacks
    fmap catMaybes $ forM stacks $ \case
      Right ver ->
        case Map.lookup ver avTools of
          Just _  -> pure Nothing
          Nothing -> do
            lSet    <- fmap (== Just ver) hlsSet
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
          [i|Could not parse version of stray directory #{e}|]
        pure Nothing

  -- NOTE: this are not cross ones, because no bindists
  toListResult :: (MonadLogger m, MonadReader AppState m, MonadIO m, MonadCatch m)
               => Tool
               -> Maybe Version
               -> [Either FilePath Version]
               -> Maybe Version
               -> [Either FilePath Version]
               -> Maybe Version
               -> [Either FilePath Version]
               -> (Version, [Tag])
               -> m ListResult
  toListResult t cSet cabals hlsSet' hlses stackSet' stacks (v, tags) = do
    AppState { pfreq
             , ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask

    case t of
      GHC -> do
        let lNoBindist = isLeft $ getDownloadInfo GHC v pfreq dls
        let tver = mkTVer v
        lSet       <- fmap (maybe False (\(GHCTargetVersion _ v') -> v' == v)) $ ghcSet Nothing
        lInstalled <- ghcInstalled tver
        fromSrc    <- ghcSrcInstalled tver
        hlsPowered <- fmap (elem v) hlsGHCVersions
        pure ListResult { lVer = v, lCross = Nothing , lTag = tags, lTool = t, lStray = False, .. }
      Cabal -> do
        let lNoBindist = isLeft $ getDownloadInfo Cabal v pfreq dls
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
        let lNoBindist = isLeft $ getDownloadInfo HLS v pfreq dls
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
        let lNoBindist = isLeft $ getDownloadInfo Stack v pfreq dls
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
rmGHCVer :: ( MonadReader AppState m
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
    lift $ $(logInfo) [i|Removing ghc symlinks|]
    liftE $ rmPlain (_tvTarget ver)

  lift $ $(logInfo) [i|Removing ghc-x.y.z symlinks|]
  liftE $ rmMinorSymlinks ver

  lift $ $(logInfo) [i|Removing/rewiring ghc-x.y symlinks|]
  -- first remove
  handle (\(_ :: ParseError) -> pure ()) $ liftE $ rmMajorSymlinks ver
  -- then fix them (e.g. with an earlier version)

  lift $ $(logInfo) [i|Removing directory recursively: #{dir}|]
  liftIO $ rmPath dir

  v' <-
    handle
      (\(e :: ParseError) -> lift $ $(logWarn) [i|#{e}|] >> pure Nothing)
    $ fmap Just
    $ getMajorMinorV (_tvVersion ver)
  forM_ v' $ \(mj, mi) -> lift (getGHCForMajor mj mi (_tvTarget ver))
    >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY)

  AppState { dirs = Dirs {..} } <- lift ask

  liftIO
    $ hideError doesNotExistErrorType
    $ rmFile (baseDir </> "share")


-- | Delete a cabal version. Will try to fix the @cabal@ symlink
-- after removal (e.g. setting it to an older version).
rmCabalVer :: ( MonadMask m
              , MonadReader AppState m
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

  AppState {dirs = Dirs {..}} <- lift ask

  let cabalFile = "cabal-" <> T.unpack (prettyVer ver) <> exeExt
  liftIO $ hideError doesNotExistErrorType $ rmFile (binDir </> cabalFile)

  when (Just ver == cSet) $ do
    cVers <- lift $ fmap rights getInstalledCabals
    case headMay . reverse . sort $ cVers of
      Just latestver -> setCabal latestver
      Nothing        -> liftIO $ rmLink (binDir </> "cabal" <> exeExt)


-- | Delete a hls version. Will try to fix the hls symlinks
-- after removal (e.g. setting it to an older version).
rmHLSVer :: ( MonadMask m
            , MonadReader AppState m
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

  AppState {dirs = Dirs {..}} <- lift ask

  bins <- lift $ hlsAllBinaries ver
  forM_ bins $ \f -> liftIO $ rmFile (binDir </> f)

  when (Just ver == isHlsSet) $ do
    -- delete all set symlinks
    oldSyms <- lift hlsSymlinks
    forM_ oldSyms $ \f -> do
      let fullF = binDir </> f
      lift $ $(logDebug) [i|rm #{fullF}|]
      liftIO $ rmLink fullF
    -- set latest hls
    hlsVers <- lift $ fmap rights getInstalledHLSs
    case headMay . reverse . sort $ hlsVers of
      Just latestver -> setHLS latestver
      Nothing        -> pure ()


-- | Delete a stack version. Will try to fix the @stack@ symlink
-- after removal (e.g. setting it to an older version).
rmStackVer :: ( MonadMask m
              , MonadReader AppState m
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

  AppState {dirs = Dirs {..}} <- lift ask

  let stackFile = "stack-" <> T.unpack (prettyVer ver) <> exeExt
  liftIO $ hideError doesNotExistErrorType $ rmFile (binDir </> stackFile)

  when (Just ver == sSet) $ do
    sVers <- lift $ fmap rights getInstalledStacks
    case headMay . reverse . sort $ sVers of
      Just latestver -> setStack latestver
      Nothing        -> liftIO $ rmLink (binDir </> "stack" <> exeExt)


rmTool :: ( MonadReader AppState m
           , MonadLogger m
           , MonadFail m
           , MonadMask m
           , MonadUnliftIO m)
           => ListResult
           -> Excepts '[NotInstalled ] m ()

rmTool ListResult {lVer, lTool, lCross} = do
  -- appstate <- ask

  case lTool of
    GHC -> do
      let ghcTargetVersion = GHCTargetVersion lCross lVer
      rmGHCVer ghcTargetVersion

    HLS -> do
      rmHLSVer lVer

    Cabal -> do
      rmCabalVer lVer

    Stack -> do
      rmStackVer lVer

    GHCup -> do
      -- leaving this unimplemented for now.
      pure ()

rmGhcupDirs :: ( MonadReader AppState m
               , MonadIO m
               , MonadLogger m
               , MonadCatch m
               , MonadMask m )
                => m ()
rmGhcupDirs = do
  dirs@Dirs
    { baseDir
    , binDir
    , logsDir
    , cacheDir
    , confDir } <- asks dirs

  let envFilePath = baseDir </> "env"

  confFilePath <- getConfigFilePath

  -- remove env File
  rmEnvFile envFilePath

-- remove the configFile file
  rmConfFile confFilePath

  -- remove entire cache Dir
  rmCacheDir cacheDir

  -- remove entire logs Dir
  rmLogsDir logsDir

  -- remove bin directory conditionally
  rmBinDir binDir

  liftIO $ print dirs

  where

    rmEnvFile enFilePath = do
      $logInfo "Removing Ghcup Environment File"
      hideError doesNotExistErrorType $ liftIO $ removeFile enFilePath

    rmConfFile confFilePath = do
      $logInfo "removing Ghcup Config File"
      hideError doesNotExistErrorType $ liftIO $ removeFile confFilePath

    rmCacheDir cacheDir = do
      $logInfo "removing ghcup cache Dir"
      contents <- liftIO $ listDirectory cacheDir
      forM_ contents deleteFile
      removeDirIfEmpty cacheDir

    rmLogsDir logsDir = do
      $logInfo "removing ghcup logs Dir"
      contents <- liftIO $ listDirectory logsDir
      forM_ contents deleteFile
      removeDirIfEmpty logsDir

    rmBinDir binDir = do
#if !defined(IS_WINDOWS)
      isXDGStyle <- useXDG
      if not isXDGStyle
        then removeDirIfEmpty binDir
        else pure ()
#else
      removeDirIfEmpty binDir
#endif

    deleteFile filepath = do
      hideError InappropriateType $ rmFile filepath

    removeDirIfEmpty filepath =
      hideError UnsatisfiedConstraints $ liftIO $ removeDirectory filepath

    ------------------
    --[ Debug info ]--
    ------------------


getDebugInfo :: (Alternative m, MonadFail m, MonadReader AppState m, MonadLogger m, MonadCatch m, MonadIO m)
             => Excepts
                  '[NoCompatiblePlatform , NoCompatibleArch , DistroNotFound]
                  m
                  DebugInfo
getDebugInfo = do
  AppState {dirs = Dirs {..}} <- lift ask
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
              , MonadReader AppState m
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
           -> Maybe Int                  -- ^ jobs
           -> Maybe FilePath           -- ^ build config
           -> Maybe FilePath           -- ^ patch directory
           -> [Text]                     -- ^ additional args to ./configure
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
#if !defined(TAR)
                 , ArchiveResult
#endif
                 ]
                m
                GHCTargetVersion
compileGHC targetGhc ov bstrap jobs mbuildConfig patchdir aargs
  = do
    AppState { pfreq = PlatformRequest {..}
             , ghcupInfo = GHCupInfo { _ghcupDownloads = dls }
             , settings
             , dirs } <- lift ask
    (workdir, tmpUnpack, tver) <- case targetGhc of
      -- unpack from version tarball
      Left tver -> do
        lift $ $(logDebug) [i|Requested to compile: #{tver} with #{bstrap}|]

        -- download source tarball
        dlInfo <-
          preview (ix GHC % ix (tver ^. tvVersion) % viSourceDL % _Just) dls
            ?? NoDownload
        dl        <- liftE $ downloadCached settings dirs dlInfo Nothing

        -- unpack
        tmpUnpack <- lift mkGhcupTmpDir
        liftE $ unpackToDir tmpUnpack dl
        void $ lift $ darwinNotarization _rPlatform tmpUnpack

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
          lift $ $(logInfo) [i|Fetching git repo #{rep} at ref #{ref} (this may take a while)|]
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

        void $ lift $ darwinNotarization _rPlatform tmpUnpack
        lift $ $(logInfo) [i|Git version #{ref} corresponds to GHC version #{prettyVer tver}|]

        pure (tmpUnpack, tmpUnpack, GHCTargetVersion Nothing tver)
    -- the version that's installed may differ from the
    -- compiled version, so the user can overwrite it
    let installVer = maybe tver (\ov' -> tver { _tvVersion = ov' }) ov

    alreadyInstalled <- lift $ ghcInstalled installVer
    alreadySet <- fmap (== Just tver) $ lift $ ghcSet (_tvTarget tver)
    when alreadyInstalled $ do
      lift $ $(logWarn) [i|GHC #{prettyShow tver} already installed. Will overwrite existing version.|]
      lift $ $(logWarn)
        "...waiting for 10 seconds before continuing, you can still abort..."
      liftIO $ threadDelay 10000000 -- give the user a sec to intervene

    ghcdir         <- lift $ ghcupGHCDir installVer

    bghc <- case bstrap of
      Right g    -> pure $ Right g
      Left  bver -> pure $ Left ("ghc-" <> (T.unpack . prettyVer $ bver) <> exeExt)

    (bindist, bmk) <- liftE $ runBuildAction
      tmpUnpack
      Nothing
      (do
        b <- compileBindist bghc tver workdir
        bmk <- liftIO $ B.readFile (build_mk workdir)
        pure (b, bmk)
      )

    when alreadyInstalled $ do
      lift $ $(logInfo) [i|Deleting existing installation|]
      liftE $ rmGHCVer tver
    liftE $ installPackedGHC bindist
                             (Just $ RegexDir "ghc-.*")
                             ghcdir
                             (tver ^. tvVersion)

    liftIO $ B.writeFile (ghcdir </> ghcUpSrcBuiltFile) bmk

    reThrowAll GHCupSetError $ postGHCInstall tver

    -- restore
    when alreadySet $ liftE $ void $ setGHC tver SetGHCOnly

    pure tver

 where
  defaultConf = case targetGhc of
    Left (GHCTargetVersion (Just _) _) -> [s|
V=0
BUILD_MAN = NO
BUILD_SPHINX_HTML = NO
BUILD_SPHINX_PDF = NO
HADDOCK_DOCS = NO
Stage1Only = YES|]
    _ -> [s|
V=0
BUILD_MAN = NO
BUILD_SPHINX_HTML = NO
BUILD_SPHINX_PDF = NO
HADDOCK_DOCS = YES|]

  compileBindist :: ( MonadReader AppState m
                    , MonadThrow m
                    , MonadCatch m
                    , MonadLogger m
                    , MonadIO m
                    , MonadFail m
                    )
                 => Either FilePath FilePath
                 -> GHCTargetVersion
                 -> FilePath
                 -> Excepts
                      '[FileDoesNotExistError, InvalidBuildConfig, PatchFailed, ProcessError, NotFoundInPATH, CopyError]
                      m
                      FilePath  -- ^ output path of bindist
  compileBindist bghc tver workdir = do
    lift $ $(logInfo) [i|configuring build|]
    liftE checkBuildConfig

    AppState { dirs = Dirs {..}, pfreq } <- lift ask

    forM_ patchdir $ \dir -> liftE $ applyPatches dir workdir

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
#if defined(IS_WINDOWS)
            ++ ["--enable-tarballs-autodownload"]
#endif
          ++ fmap T.unpack aargs
          )
          (Just workdir)
          "ghc-conf"
          (Just cEnv)

    case mbuildConfig of
      Just bc -> liftIOException
        doesNotExistErrorType
        (FileDoesNotExistError bc)
        (liftIO $ copyFile bc (build_mk workdir))
      Nothing ->
        liftIO $ B.writeFile (build_mk workdir) defaultConf

    lift $ $(logInfo) [i|Building (this may take a while)...|]
    lEM $ make (maybe [] (\j -> ["-j" <> fS (show j)]) jobs) (Just workdir)

    lift $ $(logInfo) [i|Creating bindist...|]
    lEM $ make ["binary-dist"] (Just workdir)
    [tar] <- liftIO $ findFiles
      workdir
      (makeRegexOpts compExtended
                     execBlank
                     ([s|^ghc-.*\.tar\..*$|] :: ByteString)
      )
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
    let tarName = makeValid [i|ghc-#{tVerToText tver}-#{pfReqToString pfreq}-#{iso8601Show cTime}-#{cDigest}.tar#{takeExtension tar}|]
    let tarPath = cacheDir </> tarName
    handleIO (throwE . CopyError . show) $ liftIO $ copyFile (workdir </> tar)
                                                             tarPath
    lift $ $(logInfo) [i|Copied bindist to #{tarPath}|]
    pure tarPath

  build_mk workdir = workdir </> "mk" </> "build.mk"

  checkBuildConfig :: (MonadCatch m, MonadIO m)
                   => Excepts
                        '[FileDoesNotExistError, InvalidBuildConfig]
                        m
                        ()
  checkBuildConfig = do
    c <- case mbuildConfig of
      Just bc -> do
        liftIOException
          doesNotExistErrorType
          (FileDoesNotExistError bc)
          (liftIO $ B.readFile bc)
      Nothing -> pure defaultConf
    let lines' = fmap T.strip . T.lines $ decUTF8Safe c

   -- for cross, we need Stage1Only
    case targetGhc of
      Left (GHCTargetVersion (Just _) _) -> when ("Stage1Only = YES" `notElem` lines') $ throwE
        (InvalidBuildConfig
          [s|Cross compiling needs to be a Stage1 build, add "Stage1Only = YES" to your config!|]
        )
      _ -> pure ()




    ---------------------
    --[ Upgrade GHCup ]--
    ---------------------


-- | Upgrade ghcup and place it in @~\/.ghcup\/bin\/ghcup@,
-- if no path is provided.
upgradeGHCup :: ( MonadMask m
                , MonadReader AppState m
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
upgradeGHCup mtarget force = do
  AppState { dirs = Dirs {..}
           , pfreq
           , ghcupInfo = GHCupInfo { _ghcupDownloads = dls }
           , settings } <- lift ask
  lift $ $(logInfo) [i|Upgrading GHCup...|]
  let latestVer = fromJust $ fst <$> getLatest dls GHCup
  when (not force && (latestVer <= pvpToVersion ghcUpVer)) $ throwE NoUpdate
  dli   <- lE $ getDownloadInfo GHCup latestVer pfreq dls
  tmp   <- lift withGHCupTmpDir
  let fn = "ghcup" <> exeExt
  p <- liftE $ download settings dli tmp (Just fn)
  let destDir = takeDirectory destFile
      destFile = fromMaybe (binDir </> fn <> exeExt) mtarget
  lift $ $(logDebug) [i|mkdir -p #{destDir}|]
  liftIO $ createDirRecursive' destDir
#if defined(IS_WINDOWS)
  let tempGhcup = cacheDir </> "ghcup.old"
  liftIO $ hideError NoSuchThing $ rmFile tempGhcup

  lift $ $(logDebug) [i|mv #{destFile} #{tempGhcup}|]
  -- NoSuchThing may be raised when we're updating ghcup from
  -- a non-standard location
  liftIO $ hideError NoSuchThing $ Win32.moveFileEx destFile (Just tempGhcup) 0
  lift $ $(logDebug) [i|cp #{p} #{destFile}|]
  handleIO (throwE . CopyError . show) $ liftIO $ copyFile p
                                                           destFile
#else
  lift $ $(logDebug) [i|rm -f #{destFile}|]
  liftIO $ hideError NoSuchThing $ rmFile destFile
  lift $ $(logDebug) [i|cp #{p} #{destFile}|]
  handleIO (throwE . CopyError . show) $ liftIO $ copyFile p
                                                           destFile
#endif
  lift $ chmod_755 destFile

  liftIO (isInPath destFile) >>= \b -> unless b $
    lift $ $(logWarn) [i|"#{takeFileName destFile}" is not in PATH! You have to add it in order to use ghcup.|]
  liftIO (isShadowed destFile) >>= \case
    Nothing -> pure ()
    Just pa -> lift $ $(logWarn) [i|ghcup is shadowed by "#{pa}". The upgrade will not be in effect, unless you remove "#{pa}" or make sure "#{destDir}" comes before "#{takeFileName pa}" in PATH.|]

  pure latestVer



    -------------
    --[ Other ]--
    -------------


-- | Creates @ghc-x.y.z@ and @ghc-x.y@ symlinks. This is used for
-- both installing from source and bindist.
postGHCInstall :: ( MonadReader AppState m
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
    handle (\(e :: ParseError) -> lift $ $(logWarn) [i|#{e}|] >> pure Nothing)
    $ fmap Just
    $ getMajorMinorV _tvVersion
  forM_ v' $ \(mj, mi) -> lift (getGHCForMajor mj mi _tvTarget)
    >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY)
