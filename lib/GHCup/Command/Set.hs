{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Command.Set where

import GHCup.Errors
import GHCup.Legacy.Cabal
import GHCup.Legacy.HLS
import GHCup.Legacy.Stack
import GHCup.Legacy.Utils (rmPlainGHC, binarySymLinkDestination, ghcInternalBinDir, ghcToolFiles)
import GHCup.Prelude
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.Query.Symlink
import GHCup.System.Directory
import GHCup.Types
import GHCup.Types.Optics
import GHCup.Warnings

import Control.Applicative
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Data.Maybe
import Data.Data (Proxy(..))
import Data.Variant.Excepts
import Data.Versions        hiding ( patch )
import Prelude              hiding ( abs )
import System.FilePath
import System.IO.Error

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import GHCup.Query.DB.HLS

setToolVersion ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , HasPlatformReq env
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
  -> Excepts '[ParseError, NotInstalled] m TargetVersion
setToolVersion tool ver = setToolVersion' tool ver Nothing

setToolVersion' ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , HasPlatformReq env
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
  -> Maybe FilePath        -- ^ if @Nothing@, then we operate in ~/.ghcup/bin,
                           --   otherwise in a tmp directory
  -> Excepts '[ParseError, NotInstalled] m TargetVersion
-- TODO: warn about hls compatibility
setToolVersion' tool tver mTmpDir = do
  hideExcept' @NoToolVersionSet Proxy $ when (isNothing mTmpDir) $ unsetTool tool (_tvTarget tver)

  let ver' = _tvVersion tver
  vspec <- lift $ runE $ getSymlinkSpec tool tver
  case vspec of
    -- legacy
    VLeft _
      | tool == ghc   -> liftE legacySetGHC
      | tool == cabal -> liftE $ setCabal ver' mTmpDir
      | tool == stack -> liftE $ setStack ver' mTmpDir
      | tool == hls   -> liftE $ setHLS ver' SetHLSOnly mTmpDir
      | otherwise -> throwE $ NotInstalled tool tver
    VRight spec -> do
      lift $ logDebug2 $ T.pack (show spec)
      toolDir <- lift $ fmap fromGHCupPath $ toolInstallDestination tool tver
      symls <- maybe
        (lift $ getUnqualifiedSymlinks spec toolDir)
        (\tmpDir -> lift $ getUnqualifiedSymlinks' tmpDir spec toolDir)
        mTmpDir
      shadows <- fmap catMaybes <$> forM symls $ \(target, bin) -> do
        lift $ createLink target bin
        liftIO (isShadowed bin) >>= \case
          Nothing -> pure Nothing
          Just pa -> pure $ Just (pa, bin)
      logWarn $ T.pack $ prettyHFError (ToolShadowed tool (_tvVersion tver) shadows)

  -- record in 'set' file
  when (isNothing mTmpDir) $ do
    setFile <- lift $ recordedSetVersionFile tool (_tvTarget tver)
    liftIO $ createDirRecursive' (takeDirectory setFile)
    liftIO $ T.writeFile setFile (prettyVer . _tvVersion $ tver)
    currentHLS <- liftE $ getSetVersion' hls Nothing
    currentGHC <- liftE $ getSetVersion' ghc Nothing
    supportedGHC <- lift $ maybe (pure []) (getHLSGHCs . fst) currentHLS
    lift $ warnAboutHlsCompatibility (fst <$> currentHLS) (fst <$> currentGHC) supportedGHC

  pure tver
 where
  legacySetGHC = do
    binDir' <- maybe (binDir <$> lift getDirs) pure mTmpDir
    ghcdir <- lift $ ghcupGHCDir tver
    -- for ghc tools (ghc, ghci, haddock, ...)
    verfiles <- ghcToolFiles tver
    forM_ verfiles $ \file -> do
      -- create symlink
      internalBinDir <- ghcInternalBinDir tver
      let fullF = binDir' </> file  <> exeExt
      destL <- binarySymLinkDestination binDir' (internalBinDir </> file <> exeExt)
      lift $ createLink destL fullF

    when ((isNothing . _tvTarget $ tver) && isNothing mTmpDir)
      $ lift $ symlinkShareDir (fromGHCupPath ghcdir) (T.unpack $ prettyVer (_tvVersion tver))

  symlinkShareDir :: ( MonadReader env m
                     , HasDirs env
                     , HasLog env
                     , MonadIOish m
                     )
                  => FilePath
                  -> String
                  -> m ()
  symlinkShareDir ghcdir ver' = do
    Dirs {..} <- getDirs
    let destdir = fromGHCupPath baseDir
    let sharedir     = "share"
    let fullsharedir = ghcdir </> sharedir
    logDebug $ "Checking for sharedir existence: " <> T.pack fullsharedir
    whenM (liftIO $ doesDirectoryExist fullsharedir) $ do
      let fullF   = destdir </> sharedir
      let targetF = "." </> "ghc" </> ver' </> sharedir
      hideError doesNotExistErrorType $ rmDirectoryLink fullF

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



unsetTool ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , HasPlatformReq env
  , MonadIOish m
  )
  => Tool
  -> Maybe T.Text
  -> Excepts '[ParseError, NotInstalled, NoToolVersionSet] m ()
unsetTool tool target = do
  unlessM (fmap isJust $ liftE $ getSetVersion' tool target) $ throwE (NoToolVersionSet tool target)

  mset <- liftE $ getSetVersion' tool target
  setFile <- lift $ recordedSetVersionFile tool target
  case mset of
    Just (ver, mSetFile) -> do
      let tver = TargetVersion target ver
      vspec <- lift $ runE $ getSymlinkSpec tool tver
      case vspec of
        VLeft _
          | tool == ghc -> liftE $ rmPlainGHC target
          | tool == cabal -> lift unsetCabal
          | tool == stack -> lift unsetStack
          | tool == hls -> lift unsetHLS
          | otherwise -> pure ()
        VRight spec -> do
          toolDir <- lift $ fmap fromGHCupPath $ toolInstallDestination tool tver
          unqualSyml <- lift $ getUnqualifiedSymlinks spec toolDir
          forM_ unqualSyml (rmLink . snd)
          forM_ mSetFile recycleFile
    Nothing -> pure ()
  when (tool == ghc) $ do
    Dirs {..} <- getDirs
    hideError doesNotExistErrorType $ rmDirectoryLink (fromGHCupPath baseDir </> "share")

  logDebug2 $ "rm -f " <> T.pack setFile
  hideError doesNotExistErrorType $ recycleFile setFile

