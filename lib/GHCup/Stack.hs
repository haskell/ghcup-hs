{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : GHCup.Stack
Description : GHCup installation functions for Stack
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Stack where

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



    --------------------
    --[ Installation ]--
    --------------------


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
installStackBindist dlinfo ver installDir forceInstall = do
  lift $ logDebug $ "Requested to install stack version " <> prettyVer ver

  PlatformRequest {..} <- lift getPlatformReq
  Dirs {..} <- lift getDirs

  regularStackInstalled <- lift $ stackInstalled ver

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
  liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack) (liftE . intoSubdir tmpUnpack) (view dlSubdir dlinfo)

  case installDir of
    IsolateDir isoDir -> do                 -- isolated install
      lift $ logInfo $ "isolated installing Stack to " <> T.pack isoDir
      liftE $ installStackUnpacked workdir (IsolateDirResolved isoDir) ver forceInstall
    GHCupInternal -> do                     -- regular install
      liftE $ installStackUnpacked workdir (GHCupBinDir binDir) ver forceInstall


-- | Install an unpacked stack distribution.
installStackUnpacked :: (MonadReader env m, HasLog env, MonadCatch m, MonadIO m)
              => GHCupPath      -- ^ Path to the unpacked stack bindist (where the executable resides)
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
                          _ -> ("-" <>) .  T.unpack . prettyVer $ ver
                        )
                     <> exeExt
      destPath = fromInstallDir installDir </> destFileName

  copyFileE
    (fromGHCupPath path </> stackFile <> exeExt)
    destPath
    (not forceInstall)
  lift $ chmod_755 destPath



    -----------------
    --[ Set stack ]--
    -----------------


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

  liftIO (isShadowed stackbin) >>= \case
    Nothing -> pure ()
    Just pa -> lift $ logWarn $ T.pack $ prettyShow (ToolShadowed Cabal pa stackbin ver)

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


    ----------------
    --[ Rm stack ]--
    ----------------

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
