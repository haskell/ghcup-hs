{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

{-|
Module      : GHCup
Description : GHCup installation functions
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX

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
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text                      ( Text )
import           Data.Time.Clock
import           Data.Time.Format.ISO8601
import           Data.Versions
import           Data.Word8
import           GHC.IO.Exception
import           HPath
import           HPath.IO                hiding ( hideError )
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           Safe                    hiding ( at )
import           System.IO.Error
import           System.Posix.Env.ByteString    ( getEnvironment, getEnv )
import           System.Posix.FilePath          ( getSearchPath, takeExtension )
import           System.Posix.Files.ByteString
import           Text.Regex.Posix

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E



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
                  -> PlatformRequest -- ^ the platform to install on
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
installGHCBindist dlinfo ver pfreq = do
  let tver = mkTVer ver
  lift $ $(logDebug) [i|Requested to install GHC with #{ver}|]
  whenM (lift $ ghcInstalled tver) (throwE $ AlreadyInstalled GHC ver)

  -- download (or use cached version)
  dl                           <- liftE $ downloadCached dlinfo Nothing

  -- prepare paths
  ghcdir <- lift $ ghcupGHCDir tver

  toolchainSanityChecks
  
  liftE $ installPackedGHC dl (view dlSubdir dlinfo) ghcdir ver pfreq

  liftE $ postGHCInstall tver

 where
  toolchainSanityChecks = do
    r <- forM ["CC", "LD"] (liftIO . getEnv)
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
                 => Path Abs          -- ^ Path to the packed GHC bindist
                 -> Maybe TarDir      -- ^ Subdir of the archive
                 -> Path Abs          -- ^ Path to install to
                 -> Version           -- ^ The GHC version
                 -> PlatformRequest
                 -> Excepts
                      '[ BuildFailed
                       , UnknownArchive
                       , TarDirDoesNotExist
#if !defined(TAR)
                       , ArchiveResult
#endif
                       ] m ()
installPackedGHC dl msubdir inst ver pfreq@PlatformRequest{..} = do
  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ liftIO $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack)
                   (liftE . intoSubdir tmpUnpack)
                   msubdir

  liftE $ runBuildAction tmpUnpack
                         (Just inst)
                         (installUnpackedGHC workdir inst ver pfreq)


-- | Install an unpacked GHC distribution. This only deals with the GHC
-- build system and nothing else.
installUnpackedGHC :: ( MonadReader AppState m
                      , MonadThrow m
                      , MonadLogger m
                      , MonadIO m
                      )
                   => Path Abs      -- ^ Path to the unpacked GHC bindist (where the configure script resides)
                   -> Path Abs      -- ^ Path to install to
                   -> Version       -- ^ The GHC version
                   -> PlatformRequest
                   -> Excepts '[ProcessError] m ()
installUnpackedGHC path inst ver PlatformRequest{..} = do
  lift $ $(logInfo) "Installing GHC (this may take a while)"
  lEM $ execLogged "./configure"
                   False
                   (("--prefix=" <> toFilePath inst) : alpineArgs)
                   [rel|ghc-configure|]
                   (Just path)
                   Nothing
  lEM $ make ["install"] (Just path)
  pure ()
 where
  alpineArgs
    | ver >= [vver|8.2.2|], Linux Alpine <- _rPlatform
    = ["--disable-ld-override"]
    | otherwise
    = []


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
              => GHCupDownloads  -- ^ the download info to look up the tarball from
              -> Version         -- ^ the version to install
              -> PlatformRequest -- ^ the platform to install on
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
installGHCBin bDls ver pfreq = do
  dlinfo <- lE $ getDownloadInfo GHC ver pfreq bDls
  installGHCBindist dlinfo ver pfreq


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
                    -> PlatformRequest
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
installCabalBindist dlinfo ver PlatformRequest {..} = do
  lift $ $(logDebug) [i|Requested to install cabal version #{ver}|]

  AppState {dirs = Dirs {..}} <- lift ask

  whenM
      (lift (cabalInstalled ver) >>= \a -> liftIO $
        handleIO (\_ -> pure False)
          $ fmap (\x -> a && isSymbolicLink x)
          -- ignore when the installation is a legacy cabal (binary, not symlink)
          $ getSymbolicLinkStatus (toFilePath (binDir </> [rel|cabal|]))
      )
      (throwE $ AlreadyInstalled Cabal ver)

  -- download (or use cached version)
  dl                           <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack                    <- lift withGHCupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ liftIO $ darwinNotarization _rPlatform tmpUnpack

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
                => Path Abs      -- ^ Path to the unpacked cabal bindist (where the executable resides)
                -> Path Abs      -- ^ Path to install to
                -> Excepts '[CopyError] m ()
  installCabal' path inst = do
    lift $ $(logInfo) "Installing cabal"
    let cabalFile = [rel|cabal|]
    liftIO $ createDirRecursive' inst
    destFileName <- lift $ parseRel (toFilePath cabalFile <> "-" <> verToBS ver)
    let destPath = inst </> destFileName
    handleIO (throwE . CopyError . show) $ liftIO $ copyFile
      (path </> cabalFile)
      destPath
      Overwrite
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
                => GHCupDownloads
                -> Version
                -> PlatformRequest
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
installCabalBin bDls ver pfreq = do
  dlinfo <- lE $ getDownloadInfo Cabal ver pfreq bDls
  installCabalBindist dlinfo ver pfreq


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
                  -> PlatformRequest
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
installHLSBindist dlinfo ver PlatformRequest{..} = do
  lift $ $(logDebug) [i|Requested to install hls version #{ver}|]

  AppState {dirs = Dirs {..}} <- lift ask

  whenM (lift (hlsInstalled ver))
    (throwE $ AlreadyInstalled HLS ver)

  -- download (or use cached version)
  dl                           <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack                    <- lift withGHCupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ liftIO $ darwinNotarization _rPlatform tmpUnpack

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
                => Path Abs      -- ^ Path to the unpacked hls bindist (where the executable resides)
                -> Path Abs      -- ^ Path to install to
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
      toF <- parseRel (toFilePath f <> "~" <> verToBS ver)
      handleIO (throwE . CopyError . show) $ liftIO $ copyFile
        (path </> f)
        (inst </> toF)
        Overwrite
      lift $ chmod_755 (inst </> toF)

    -- install haskell-language-server-wrapper
    let wrapper = [rel|haskell-language-server-wrapper|]
    toF <- parseRel (toFilePath wrapper <> "-" <> verToBS ver)
    handleIO (throwE . CopyError . show) $ liftIO $ copyFile
      (path </> wrapper)
      (inst </> toF)
      Overwrite
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
              => GHCupDownloads
              -> Version
              -> PlatformRequest
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
installHLSBin bDls ver pfreq = do
  dlinfo <- lE $ getDownloadInfo HLS ver pfreq bDls
  installHLSBindist dlinfo ver pfreq




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
          )
       => GHCTargetVersion
       -> SetGHC
       -> Excepts '[NotInstalled] m GHCTargetVersion
setGHC ver sghc = do
  let verBS = verToBS (_tvVersion ver)
  ghcdir                        <- lift $ ghcupGHCDir ver

  whenM (lift $ not <$> ghcInstalled ver) (throwE (NotInstalled GHC ver))

  -- symlink destination
  AppState { dirs = Dirs {..} } <- lift ask
  liftIO $ createDirRecursive' binDir

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
        v' <-
          handle
            (\(e :: ParseError) -> lift $ $(logWarn) [i|#{e}|] >> pure Nothing)
          $ fmap Just
          $ getMajorMinorV (_tvVersion ver)
        forM v' $ \(mj, mi) ->
          let major' = E.encodeUtf8 $ intToText mj <> "." <> intToText mi
          in  parseRel (toFilePath file <> B.singleton _hyphen <> major')
      SetGHC_XYZ ->
        fmap Just $ parseRel (toFilePath file <> B.singleton _hyphen <> verBS)

    -- create symlink
    forM mTargetFile $ \targetFile -> do
      let fullF = binDir </> targetFile
      destL <- lift $ ghcLinkDestination (toFilePath file) ver
      lift $ $(logDebug) [i|ln -s #{destL} #{toFilePath fullF}|]
      liftIO $ createSymlink fullF destL

  -- create symlink for share dir
  when (isNothing . _tvTarget $ ver) $ lift $ symlinkShareDir ghcdir verBS

  pure ver

 where

  symlinkShareDir :: (MonadReader AppState m, MonadIO m, MonadLogger m)
                  => Path Abs
                  -> ByteString
                  -> m ()
  symlinkShareDir ghcdir verBS = do
    AppState { dirs = Dirs {..} } <- ask
    let destdir = baseDir
    case sghc of
      SetGHCOnly -> do
        let sharedir     = [rel|share|]
        let fullsharedir = ghcdir </> sharedir
        whenM (liftIO $ doesDirectoryExist fullsharedir) $ do
          let fullF   = destdir </> sharedir
          let targetF = "./ghc/" <> verBS <> "/" <> toFilePath sharedir
          $(logDebug) [i|rm -f #{fullF}|]
          liftIO $ hideError doesNotExistErrorType $ deleteFile fullF
          $(logDebug) [i|ln -s #{targetF} #{fullF}|]
          liftIO $ createSymlink fullF targetF
      _ -> pure ()



-- | Set the @~\/.ghcup\/bin\/cabal@ symlink.
setCabal :: (MonadReader AppState m, MonadLogger m, MonadThrow m, MonadFail m, MonadIO m)
         => Version
         -> Excepts '[NotInstalled] m ()
setCabal ver = do
  let verBS = verToBS ver
  targetFile <- parseRel ("cabal-" <> verBS)

  -- symlink destination
  AppState {dirs = Dirs {..}} <- lift ask
  liftIO $ createDirRecursive' binDir

  whenM (liftIO $ not <$> doesFileExist (binDir </> targetFile))
    $ throwE
    $ NotInstalled Cabal (GHCTargetVersion Nothing ver)

  let cabalbin = binDir </> [rel|cabal|]

  -- delete old file (may be binary or symlink)
  lift $ $(logDebug) [i|rm -f #{toFilePath cabalbin}|]
  liftIO $ hideError doesNotExistErrorType $ deleteFile
    cabalbin

  -- create symlink
  let destL = toFilePath targetFile
  lift $ $(logDebug) [i|ln -s #{destL} #{toFilePath cabalbin}|]
  liftIO $ createSymlink cabalbin destL

  pure ()




-- | Set the haskell-language-server symlinks.
setHLS :: ( MonadCatch m
          , MonadReader AppState m
          , MonadLogger m
          , MonadThrow m
          , MonadFail m
          , MonadIO m
          )
       => Version
       -> Excepts '[NotInstalled] m ()
setHLS ver = do
  AppState { dirs = Dirs {..} } <- lift ask
  liftIO $ createDirRecursive' binDir

  -- Delete old symlinks, since these might have different ghc versions than the
  -- selected version, so we could end up with stray or incorrect symlinks.
  oldSyms <- lift hlsSymlinks
  forM_ oldSyms $ \f -> do
    lift $ $(logDebug) [i|rm #{toFilePath (binDir </> f)}|]
    liftIO $ deleteFile (binDir </> f)

  -- set haskell-language-server-<ghcver> symlinks
  bins <- lift $ hlsServerBinaries ver
  when (null bins) $ throwE $ NotInstalled HLS (GHCTargetVersion Nothing ver)

  forM_ bins $ \f -> do
    let destL = toFilePath f
    target <- parseRel . head . B.split _tilde . toFilePath $ f

    lift $ $(logDebug) [i|rm -f #{toFilePath (binDir </> target)}|]
    liftIO $ hideError doesNotExistErrorType $ deleteFile (binDir </> target)

    lift $ $(logDebug) [i|ln -s #{destL} #{toFilePath (binDir </> target)}|]
    liftIO $ createSymlink (binDir </> target) destL

  -- set haskell-language-server-wrapper symlink
  let destL = "haskell-language-server-wrapper-" <> verToBS ver
  let wrapper = binDir </> [rel|haskell-language-server-wrapper|]

  lift $ $(logDebug) [i|rm -f #{toFilePath wrapper}|]
  liftIO $ hideError doesNotExistErrorType $ deleteFile wrapper

  lift $ $(logDebug) [i|ln -s #{destL} #{toFilePath wrapper}|]
  liftIO $ createSymlink wrapper destL

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
             => GHCupDownloads
             -> Maybe Tool
             -> Maybe ListCriteria
             -> PlatformRequest
             -> m [ListResult]
listVersions av lt' criteria pfreq = do
  -- some annoying work to avoid too much repeated IO
  cSet <- cabalSet
  cabals <- getInstalledCabals' cSet
  hlsSet' <- hlsSet
  hlses <- getInstalledHLSs

  go lt' cSet cabals hlsSet' hlses
 where
  go lt cSet cabals hlsSet' hlses = do
    case lt of
      Just t -> do
        -- get versions from GHCupDownloads
        let avTools = availableToolVersions av t
        lr <- filter' <$> forM (Map.toList avTools) (toListResult t cSet cabals hlsSet' hlses)

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
          GHCup -> pure lr
      Nothing -> do
        ghcvers   <- go (Just GHC) cSet cabals hlsSet' hlses
        cabalvers <- go (Just Cabal) cSet cabals hlsSet' hlses
        hlsvers   <- go (Just HLS) cSet cabals hlsSet' hlses
        ghcupvers <- go (Just GHCup) cSet cabals hlsSet' hlses
        pure (ghcvers <> cabalvers <> hlsvers <> ghcupvers)
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
          [i|Could not parse version of stray directory #{toFilePath e}|]
        pure Nothing

  strayCabals :: (MonadReader AppState m, MonadCatch m, MonadThrow m, MonadLogger m, MonadIO m)
            => Map.Map Version [Tag]
            -> Maybe Version
            -> [Either (Path Rel) Version]
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
          [i|Could not parse version of stray directory #{toFilePath e}|]
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
          [i|Could not parse version of stray directory #{toFilePath e}|]
        pure Nothing

  -- NOTE: this are not cross ones, because no bindists
  toListResult :: (MonadLogger m, MonadReader AppState m, MonadIO m, MonadCatch m)
               => Tool
               -> Maybe Version
               -> [Either (Path Rel) Version]
               -> Maybe Version
               -> [Either (Path Rel) Version]
               -> (Version, [Tag])
               -> m ListResult
  toListResult t cSet cabals hlsSet' hlses (v, tags) = case t of
    GHC -> do
      let lNoBindist = isLeft $ getDownloadInfo GHC v pfreq av
      let tver = mkTVer v
      lSet       <- fmap (maybe False (\(GHCTargetVersion _ v') -> v' == v)) $ ghcSet Nothing
      lInstalled <- ghcInstalled tver
      fromSrc    <- ghcSrcInstalled tver
      hlsPowered <- fmap (elem v) hlsGHCVersions
      pure ListResult { lVer = v, lCross = Nothing , lTag = tags, lTool = t, lStray = False, .. }
    Cabal -> do
      let lNoBindist = isLeft $ getDownloadInfo Cabal v pfreq av
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
      let lNoBindist = isLeft $ getDownloadInfo HLS v pfreq av
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

  lift $ $(logInfo) [i|Removing directory recursively: #{toFilePath dir}|]
  liftIO $ deleteDirRecursive dir

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
    $ deleteFile (baseDir </> [rel|share|])


-- | Delete a cabal version. Will try to fix the @cabal@ symlink
-- after removal (e.g. setting it to an older version).
rmCabalVer :: (MonadReader AppState m, MonadThrow m, MonadLogger m, MonadIO m, MonadFail m, MonadCatch m)
           => Version
           -> Excepts '[NotInstalled] m ()
rmCabalVer ver = do
  whenM (lift $ fmap not $ cabalInstalled ver) $ throwE (NotInstalled Cabal (GHCTargetVersion Nothing ver))

  cSet      <- lift cabalSet

  AppState {dirs = Dirs {..}} <- lift ask

  cabalFile <- lift $ parseRel ("cabal-" <> verToBS ver)
  liftIO $ hideError doesNotExistErrorType $ deleteFile (binDir </> cabalFile)

  when (Just ver == cSet) $ do
    cVers <- lift $ fmap rights getInstalledCabals
    case headMay . reverse . sort $ cVers of
      Just latestver -> setCabal latestver
      Nothing        -> liftIO $ hideError doesNotExistErrorType $ deleteFile
        (binDir </> [rel|cabal|])


-- | Delete a hls version. Will try to fix the hls symlinks
-- after removal (e.g. setting it to an older version).
rmHLSVer :: (MonadReader AppState m, MonadThrow m, MonadLogger m, MonadIO m, MonadFail m, MonadCatch m)
         => Version
         -> Excepts '[NotInstalled] m ()
rmHLSVer ver = do
  whenM (lift $ fmap not $ hlsInstalled ver) $ throwE (NotInstalled HLS (GHCTargetVersion Nothing ver))

  isHlsSet      <- lift hlsSet

  AppState {dirs = Dirs {..}} <- lift ask

  bins <- lift $ hlsAllBinaries ver
  forM_ bins $ \f -> liftIO $ deleteFile (binDir </> f)

  when (Just ver == isHlsSet) $ do
    -- delete all set symlinks
    oldSyms <- lift hlsSymlinks
    forM_ oldSyms $ \f -> do
      lift $ $(logDebug) [i|rm #{toFilePath (binDir </> f)}|]
      liftIO $ deleteFile (binDir </> f)
    -- set latest hls
    hlsVers <- lift $ fmap rights getInstalledHLSs
    case headMay . reverse . sort $ hlsVers of
      Just latestver -> setHLS latestver
      Nothing        -> pure ()




    ------------------
    --[ Debug info ]--
    ------------------


getDebugInfo :: (MonadReader AppState m, MonadLogger m, MonadCatch m, MonadIO m)
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
           => GHCupDownloads
           -> GHCTargetVersion           -- ^ version to install
           -> Either Version (Path Abs)  -- ^ version to bootstrap with
           -> Maybe Int                  -- ^ jobs
           -> Maybe (Path Abs)           -- ^ build config
           -> Maybe (Path Abs)           -- ^ patch directory
           -> [Text]                     -- ^ additional args to ./configure
           -> PlatformRequest
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
                ()
compileGHC dls tver bstrap jobs mbuildConfig patchdir aargs pfreq@PlatformRequest{..}
  = do
    lift $ $(logDebug) [i|Requested to compile: #{tver} with #{bstrap}|]

    alreadyInstalled <- lift $ ghcInstalled tver
    alreadySet <- fmap (== Just tver) $ lift $ ghcSet (_tvTarget tver)

    -- download source tarball
    dlInfo <-
      preview (ix GHC % ix (tver ^. tvVersion) % viSourceDL % _Just) dls
        ?? NoDownload
    dl        <- liftE $ downloadCached dlInfo Nothing

    -- unpack
    tmpUnpack <- lift mkGhcupTmpDir
    liftE $ unpackToDir tmpUnpack dl
    void $ liftIO $ darwinNotarization _rPlatform tmpUnpack

    bghc <- case bstrap of
      Right g    -> pure $ Right g
      Left  bver -> Left <$> parseRel ("ghc-" <> verToBS bver)
    workdir <- maybe (pure tmpUnpack)
                     (liftE . intoSubdir tmpUnpack)
                     (view dlSubdir dlInfo)
    ghcdir         <- lift $ ghcupGHCDir tver

    (bindist, bmk) <- liftE $ runBuildAction
      tmpUnpack
      Nothing
      (do
        b   <- compileBindist bghc ghcdir workdir
        bmk <- liftIO $ readFileStrict (build_mk workdir)
        pure (b, bmk)
      )

    when alreadyInstalled $ do
      lift $ $(logInfo) [i|Deleting existing installation|]
      liftE $ rmGHCVer tver
    liftE $ installPackedGHC bindist
                             (view dlSubdir dlInfo)
                             ghcdir
                             (tver ^. tvVersion)
                             pfreq

    liftIO $ writeFile (ghcdir </> ghcUpSrcBuiltFile) (Just newFilePerms) bmk

    reThrowAll GHCupSetError $ postGHCInstall tver

    -- restore
    when alreadySet $ liftE $ void $ setGHC tver SetGHCOnly

 where
  defaultConf = case _tvTarget tver of
    Nothing -> [s|
V=0
BUILD_MAN = NO
BUILD_SPHINX_HTML = NO
BUILD_SPHINX_PDF = NO
HADDOCK_DOCS = YES|]
    Just _ -> [s|
V=0
BUILD_MAN = NO
BUILD_SPHINX_HTML = NO
BUILD_SPHINX_PDF = NO
HADDOCK_DOCS = NO
Stage1Only = YES|]

  compileBindist :: ( MonadReader AppState m
                    , MonadThrow m
                    , MonadCatch m
                    , MonadLogger m
                    , MonadIO m
                    , MonadFail m
                    )
                 => Either (Path Rel) (Path Abs)
                 -> Path Abs
                 -> Path Abs
                 -> Excepts
                      '[FileDoesNotExistError, InvalidBuildConfig, PatchFailed, ProcessError, NotFoundInPATH, CopyError]
                      m
                      (Path Abs)  -- ^ output path of bindist
  compileBindist bghc ghcdir workdir = do
    lift $ $(logInfo) [i|configuring build|]
    liftE checkBuildConfig

    AppState { dirs = Dirs {..} } <- lift ask

    forM_ patchdir $ \dir -> liftE $ applyPatches dir workdir

    cEnv <- liftIO getEnvironment

    if
      | _tvVersion tver >= [vver|8.8.0|] -> do
        bghcPath <- case bghc of
          Right ghc' -> pure ghc'
          Left  bver -> do
            spaths <- catMaybes . fmap parseAbs <$> liftIO getSearchPath
            liftIO (searchPath spaths bver) !? NotFoundInPATH bver
        lEM $ execLogged
          "./configure"
          False
          (  ["--prefix=" <> toFilePath ghcdir]
          ++ maybe mempty
                    (\x -> ["--target=" <> E.encodeUtf8 x])
                    (_tvTarget tver)
          ++ fmap E.encodeUtf8 aargs
          )
          [rel|ghc-conf|]
          (Just workdir)
          (Just (("GHC", toFilePath bghcPath) : cEnv))
      | otherwise -> do
        lEM $ execLogged
          "./configure"
          False
          (  [ "--prefix=" <> toFilePath ghcdir
             , "--with-ghc=" <> either toFilePath toFilePath bghc
             ]
          ++ maybe mempty
                   (\x -> ["--target=" <> E.encodeUtf8 x])
                   (_tvTarget tver)
          ++ fmap E.encodeUtf8 aargs
          )
          [rel|ghc-conf|]
          (Just workdir)
          (Just cEnv)

    case mbuildConfig of
      Just bc -> liftIOException
        doesNotExistErrorType
        (FileDoesNotExistError $ toFilePath bc)
        (liftIO $ copyFile bc (build_mk workdir) Overwrite)
      Nothing ->
        liftIO $ writeFile (build_mk workdir) (Just newFilePerms) defaultConf

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
    c       <- liftIO $ readFile (workdir </> tar)
    cDigest <-
      fmap (T.take 8)
      . lift
      . throwEither
      . E.decodeUtf8'
      . B16.encode
      . SHA256.hashlazy
      $ c
    cTime <- liftIO getCurrentTime
    tarName <-
      parseRel
        [i|ghc-#{tVerToText tver}-#{pfReqToString pfreq}-#{iso8601Show cTime}-#{cDigest}.tar#{takeExtension (toFilePath tar)}|]
    let tarPath = cacheDir </> tarName
    handleIO (throwE . CopyError . show) $ liftIO $ copyFile (workdir </> tar)
                                                             tarPath
                                                             Strict
    lift $ $(logInfo) [i|Copied bindist to #{tarPath}|]
    pure tarPath

  build_mk workdir = workdir </> [rel|mk/build.mk|]

  checkBuildConfig :: (MonadCatch m, MonadIO m)
                   => Excepts
                        '[FileDoesNotExistError, InvalidBuildConfig]
                        m
                        ()
  checkBuildConfig = do
    c <- case mbuildConfig of
      Just bc -> do
        BL.toStrict <$> liftIOException
          doesNotExistErrorType
          (FileDoesNotExistError $ toFilePath bc)
          (liftIO $ readFile bc)
      Nothing -> pure defaultConf
    let lines' = fmap T.strip . T.lines $ decUTF8Safe c

   -- for cross, we need Stage1Only
    case _tvTarget tver of
      Just _ -> when ("Stage1Only = YES" `notElem` lines') $ throwE
        (InvalidBuildConfig
          [s|Cross compiling needs to be a Stage1 build, add "Stage1Only = YES" to your config!|]
        )
      Nothing -> pure ()




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
             => GHCupDownloads
             -> Maybe (Path Abs)  -- ^ full file destination to write ghcup into
             -> Bool              -- ^ whether to force update regardless
                                  --   of currently installed version
             -> PlatformRequest
             -> Excepts
                  '[ CopyError
                   , DigestError
                   , DownloadFailed
                   , NoDownload
                   , NoUpdate
                   ]
                  m
                  Version
upgradeGHCup dls mtarget force pfreq = do
  AppState {dirs = Dirs {..}} <- lift ask
  lift $ $(logInfo) [i|Upgrading GHCup...|]
  let latestVer = fromJust $ fst <$> getLatest dls GHCup
  when (not force && (latestVer <= pvpToVersion ghcUpVer)) $ throwE NoUpdate
  dli   <- lE $ getDownloadInfo GHCup latestVer pfreq dls
  tmp   <- lift withGHCupTmpDir
  let fn = [rel|ghcup|]
  p <- liftE $ download dli tmp (Just fn)
  let destDir = dirname destFile
      destFile = fromMaybe (binDir </> fn) mtarget
  lift $ $(logDebug) [i|mkdir -p #{toFilePath destDir}|]
  liftIO $ createDirRecursive' destDir
  lift $ $(logDebug) [i|rm -f #{toFilePath destFile}|]
  liftIO $ hideError NoSuchThing $ deleteFile destFile
  lift $ $(logDebug) [i|cp #{toFilePath p} #{toFilePath destFile}|]
  handleIO (throwE . CopyError . show) $ liftIO $ copyFile p
                                                           destFile
                                                           Overwrite
  lift $ chmod_755 destFile

  liftIO (isInPath destFile) >>= \b -> unless b $
    lift $ $(logWarn) [i|"#{toFilePath (dirname destFile)}" is not in PATH! You have to add it in order to use ghcup.|]
  liftIO (isShadowed destFile) >>= \case
    Nothing -> pure ()
    Just pa -> lift $ $(logWarn) [i|ghcup is shadowed by "#{toFilePath pa}". The upgrade will not be in effect, unless you remove "#{toFilePath pa}" or make sure "#{toFilePath destDir}" comes before "#{toFilePath (dirname pa)}" in PATH.|]

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

