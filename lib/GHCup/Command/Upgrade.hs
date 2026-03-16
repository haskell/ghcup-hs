{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GHCup.Command.Upgrade
Description : GHCup upgrade
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Command.Upgrade where


import GHCup.Download
import GHCup.Errors
import GHCup.Hardcoded.Version
import GHCup.Input.Parsers.URI
import GHCup.Prelude
import GHCup.Query.GHCupDirs
import GHCup.Query.Metadata
import GHCup.Types
import GHCup.Types.Optics

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Resource hiding ( throwM )
import Data.Maybe
import Data.Variant.Excepts
import Data.Versions                hiding ( patch )
import GHC.IO.Exception
import Prelude                      hiding ( abs, writeFile )
import System.FilePath

import qualified Data.Text as T





    -------------------------
    --[ GHCup upgrade etc ]--
    -------------------------

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
                   , ContentLengthError
                   , GPGError
                   , GPGError
                   , DownloadFailed
                   , NoDownload
                   , NoUpdate
                   , ToolShadowed
                   , URIParseError
                   ]
                  m
                  Version
upgradeGHCup mtarget force' fatal = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  let latestVer = _tvVersion $ fst (fromJust (getLatest dls ghcup))
  upgradeGHCup' mtarget force' fatal latestVer


-- | Upgrade ghcup and place it in @~\/.ghcup\/bin\/ghcup@,
-- if no path is provided.
upgradeGHCup' :: ( MonadMask m
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
              -> Version
              -> Excepts
                   '[ CopyError
                    , DigestError
                    , ContentLengthError
                    , GPGError
                    , GPGError
                    , DownloadFailed
                    , NoDownload
                    , NoUpdate
                    , ToolShadowed
                    , URIParseError
                    ]
                   m
                   Version
upgradeGHCup' mtarget force' fatal latestVer = do
  Dirs {..} <- lift getDirs
  lift $ logInfo "Upgrading GHCup..."
  (Just ghcupPVPVer) <- pure $ pvpToVersion ghcUpVer ""
  when (not force' && (latestVer <= ghcupPVPVer)) $ throwE NoUpdate
  dli   <- liftE $ getDownloadInfo ghcup latestVer
  tmp   <- fromGHCupPath <$> lift withGHCupTmpDir
  let fn = "ghcup" <> exeExt
  dlu <- lE $ parseURI' (_dlUri dli)
  p <- liftE $ download dlu Nothing (Just (_dlHash dli)) (_dlCSize dli) tmp (Just fn) False
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
      | fatal -> throwE (ToolShadowed ghcup pa destFile latestVer)
      | otherwise ->
        lift $ logWarn $ T.pack $ prettyHFError (ToolShadowed ghcup pa destFile latestVer)

  pure latestVer

