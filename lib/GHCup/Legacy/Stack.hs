{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GHCup.Legacy.Stack
Description : GHCup legacy installation functions for Stack
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Legacy.Stack where

import GHCup.Errors
import GHCup.Legacy.Utils
import GHCup.Prelude
import GHCup.System.Directory
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
import Data.Either
import Data.List
import Data.Maybe
import Data.Ord
import Data.Variant.Excepts
import Data.Versions                hiding ( patch )
import Prelude                      hiding ( abs, writeFile )
import Safe                         hiding ( at )
import System.FilePath
import System.IO.Error

import qualified Data.Text as T



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
         -> Maybe FilePath
         -> Excepts '[NotInstalled] m ()
setStack ver mTmpDir = do
  let targetFile = "stack-" <> T.unpack (prettyVer ver) <> exeExt

  -- symlink destination
  Dirs {..} <- lift getDirs

  whenM (liftIO $ not <$> doesFileExist (binDir </> targetFile))
    $ throwE
    $ NotInstalled stack (TargetVersion Nothing ver)

  let stackbin = fromMaybe binDir mTmpDir </> "stack" <> exeExt

  lift $ createLink targetFile stackbin

  liftIO (isShadowed stackbin) >>= \case
    Nothing -> pure ()
    Just pa -> lift $ logWarn $ T.pack $ prettyHFError (ToolShadowed stack ver [(pa, stackbin)])

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
  whenM (lift $ fmap not $ stackInstalled ver) $ throwE (NotInstalled stack (TargetVersion Nothing ver))

  sSet      <- lift stackSet

  Dirs {..} <- lift getDirs

  let stackFile = "stack-" <> T.unpack (prettyVer ver) <> exeExt
  lift $ hideError doesNotExistErrorType $ recycleFile (binDir </> stackFile)

  when (Just ver == sSet) $ do
    sVers <- lift $ fmap rights getInstalledStacks
    case headMay . sortBy (comparing Down) $ sVers of
      Just latestver -> setStack latestver Nothing
      Nothing        -> lift $ rmLink (binDir </> "stack" <> exeExt)
