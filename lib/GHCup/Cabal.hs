{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : GHCup.Cabal
Description : GHCup installation functions for Cabal
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Cabal where

import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Prelude
import           GHCup.Prelude.File
import           GHCup.Prelude.Logger

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
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Versions                hiding ( patch )
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , writeFile
                                                )
import           Safe                    hiding ( at )
import           System.FilePath
import           System.IO.Error

import qualified Data.Text                     as T
import Text.PrettyPrint.HughesPJClass (prettyShow)



    -------------------------
    --[ Tool installation ]--
    -------------------------


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
                          , ContentLengthError
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
  regularCabalInstalled <- lift $ cabalInstalled ver

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
  liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)

  -- the subdir of the archive where we do the work
  workdir <- fromGHCupPath <$> maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)

  case installDir of
    IsolateDir isoDir -> do             -- isolated install
      lift $ logInfo $ "isolated installing Cabal to " <> T.pack isoDir
      liftE $ installCabalUnpacked workdir (IsolateDirResolved isoDir) ver forceInstall

    GHCupInternal -> do                 -- regular install
      liftE $ installCabalUnpacked workdir (GHCupBinDir binDir) ver forceInstall


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
              _ -> ("-" <>) . T.unpack . prettyVer $ ver
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
                      , ContentLengthError
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


    -----------------
    --[ Set cabal ]--
    -----------------


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

  liftIO (isShadowed cabalbin) >>= \case
    Nothing -> pure ()
    Just pa -> lift $ logWarn $ T.pack $ prettyShow (ToolShadowed Cabal pa cabalbin ver)

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


    ----------------
    --[ Rm cabal ]--
    ----------------


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
