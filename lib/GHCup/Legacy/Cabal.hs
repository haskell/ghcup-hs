{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GHCup.Legacy.Cabal
Description : GHCup legacy installation functions for Cabal
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Legacy.Cabal where

import GHCup.Errors
import GHCup.Legacy.Utils
import GHCup.Prelude
import GHCup.Types
import GHCup.System.Directory
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
         -> Maybe FilePath
         -> Excepts '[NotInstalled] m ()
setCabal ver mTmpDir = do
  let targetFile = "cabal-" <> T.unpack (prettyVer ver) <> exeExt

  -- symlink destination
  Dirs {..} <- lift getDirs

  whenM (liftIO $ not <$> doesFileExist (binDir </> targetFile))
    $ throwE
    $ NotInstalled cabal (GHCTargetVersion Nothing ver)

  let cabalbin = fromMaybe binDir mTmpDir </> "cabal" <> exeExt

  -- create link
  let destL = targetFile
  lift $ createLink destL cabalbin

  liftIO (isShadowed cabalbin) >>= \case
    Nothing -> pure ()
    Just pa -> lift $ logWarn $ T.pack $ prettyHFError (ToolShadowed cabal pa cabalbin ver)

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
  whenM (lift $ fmap not $ cabalInstalled ver) $ throwE (NotInstalled cabal (GHCTargetVersion Nothing ver))

  cSet      <- lift cabalSet

  Dirs {..} <- lift getDirs

  let cabalFile = "cabal-" <> T.unpack (prettyVer ver) <> exeExt
  lift $ hideError doesNotExistErrorType $ recycleFile (binDir </> cabalFile)

  when (Just ver == cSet) $ do
    cVers <- lift $ fmap rights getInstalledCabals
    case headMay . sortBy (comparing Down) $ cVers of
      Just latestver -> setCabal latestver Nothing
      Nothing        -> lift $ rmLink (binDir </> "cabal" <> exeExt)
