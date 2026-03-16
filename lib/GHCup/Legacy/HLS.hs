{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : GHCup.Legacy.HLS
Description : GHCup installation functions for legacy HLS
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Legacy.HLS where

import GHCup.Errors
import GHCup.Legacy.Utils
import GHCup.Prelude
import GHCup.Prelude.String.QQ
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
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
import Data.ByteString              ( ByteString )
import Data.Either
import Data.List
import Data.Maybe
import Data.Ord
import Data.Variant.Excepts
import Data.Versions                hiding ( patch )
import GHC.IO.Exception
import Prelude                      hiding ( abs, writeFile )
import Safe                         hiding ( at )
import System.FilePath
import System.IO.Error
import Text.Regex.Posix

import qualified Data.Text       as T
import qualified Text.Megaparsec as MP



    --------------------
    --[ Installation ]--
    --------------------



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

  lift $ logInfo $ "Install binaries from " <> T.pack path
  -- install haskell-language-server-<ghcver>
  bins@(_:_) <- liftIO $ findFiles
    path
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^haskell-language-server-[0-9].*$|] :: ByteString)
    )

  lift $ logInfo $ "Found binaries " <> T.pack (show bins)
  forM_ bins $ \f -> do
    let toF = dropSuffix exeExt f
              <> (case installDir of
                   IsolateDirResolved _ -> ""
                   _                    -> ("~" <>) . T.unpack . prettyVer $ ver
                 )
              <> exeExt

    let srcPath = path </> f
    let destPath = fromInstallDir installDir </> toF

    -- destination could be an existing symlink
    -- for new make-based HLSes
    liftIO $ rmFileForce destPath

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
                 _                    -> ("-" <>) . T.unpack . prettyVer $ ver
               )
            <> exeExt
      srcWrapperPath = path </> wrapper <> exeExt
      destWrapperPath = fromInstallDir installDir </> toF

  liftIO $ rmFileForce destWrapperPath
  copyFileE
    srcWrapperPath
    destWrapperPath
    (not forceInstall)

  lift $ chmod_755 destWrapperPath



-- | Installs hls binaries @haskell-language-server-\<ghcver\>@
-- into @~\/.ghcup\/bin/@, as well as @haskell-languager-server-wrapper@.



    -----------------
    --[ Set/Unset ]--
    -----------------

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
  whenM (lift $ not <$> hlsInstalled ver) (throwE (NotInstalled hls (GHCTargetVersion Nothing ver)))

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
      when (null bins) $ throwE $ NotInstalled hls (GHCTargetVersion Nothing ver)

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

      liftIO (isShadowed wrapper) >>= \case
        Nothing -> pure ()
        Just pa -> lift $ logWarn $ T.pack $ prettyHFError (ToolShadowed hls pa wrapper ver)


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




    ---------------
    --[ Removal ]--
    ---------------


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
  whenM (lift $ fmap not $ hlsInstalled ver) $ throwE (NotInstalled hls (GHCTargetVersion Nothing ver))

  isHlsSet <- lift hlsSet

  liftE $ rmMinorHLSSymlinks ver

  when (Just ver == isHlsSet) $ do
    -- delete all set symlinks
    liftE rmPlainHLS

  hlsDir' <- ghcupHLSDir ver
  let hlsDir = fromGHCupPath hlsDir'
  lift (getInstalledFiles hls (mkTVer ver)) >>= \case
    Just files -> do
      lift $ logInfo $ "Removing files safely from: " <> T.pack hlsDir
      forM_ files (lift . hideError NoSuchThing . recycleFile . (\f -> hlsDir </> dropDrive f))
      removeEmptyDirsRecursive hlsDir
      survivors <- liftIO $ hideErrorDef [doesNotExistErrorType] [] $ listDirectory hlsDir
      f <- recordedInstallationFile hls (mkTVer ver)
      lift $ recycleFile f
      when (not (null survivors)) $ throwE $ UninstallFailed hlsDir survivors
    Nothing -> do
      isDir <- liftIO $ doesDirectoryExist hlsDir
      isSyml <- liftIO $ handleIO (\_ -> pure False) $ pathIsSymbolicLink hlsDir
      when (isDir && not isSyml) $ do
        lift $ logInfo $ "Removing legacy directory recursively: " <> T.pack hlsDir
        recyclePathForcibly hlsDir'

  when (Just ver == isHlsSet) $ do
    -- set latest hls
    hlsVers <- lift $ fmap rights getInstalledHLSs
    case headMay . sortBy (comparing Down) $ hlsVers of
      Just latestver -> liftE $ setHLS latestver SetHLSOnly Nothing
      Nothing        -> pure ()


