{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Query.Symlink where

import GHCup.Errors
import GHCup.Legacy.Utils
import GHCup.Prelude
import GHCup.Types
import GHCup.Types.Optics

import Control.Exception      ( Exception (..) )
import Control.Exception.Safe ( handle )
import Control.Monad          ( forM, when )
import Control.Monad.Reader   ( MonadReader )
import Data.Either            ( partitionEithers )
import Data.Maybe             ( catMaybes )
import Data.Variant.Excepts   ( Excepts, throwE )
import Data.Versions          ( Version, prettyVer )
import System.FilePath        ( (</>) )

import qualified Data.Text as T


-- | These are the symlinks we need to create
-- in order to 'set' a tool version.
getUnqualifiedSymlinks ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => [SymlinkFileSpec]
  -> FilePath
  -> m [(FilePath, FilePath)]
getUnqualifiedSymlinks specs toolDir = do
  Dirs {..}  <- getDirs
  getUnqualifiedSymlinks' binDir specs toolDir

-- | Like 'getUnqualifiedSymlinks', except takes
-- the binary directory as an explicit argument.
getUnqualifiedSymlinks' ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => FilePath
  -> [SymlinkFileSpec]
  -> FilePath
  -> m [(FilePath, FilePath)]
getUnqualifiedSymlinks' bindir specs toolDir =
  fmap catMaybes $ forM specs $ \SymlinkSpec{..} ->
    case _slSetName of
      Nothing -> pure Nothing
      Just setName -> do
       target <- binarySymLinkDestination bindir (toolDir </> _slTarget)
       pure $ Just (target, bindir </> setName)

getPVPMajorSymlinks' ::
  ( MonadReader env m
  , HasLog env
  , MonadIOish m
  )
  => FilePath
  -> [SymlinkSpec [Either Char Version]]
  -> Version
  -> FilePath
  -> Excepts '[MalformedInstallInfo] m [(FilePath, FilePath)]
getPVPMajorSymlinks' bindir spec ver toolBinDir =
  fmap catMaybes $ forM spec $ \SymlinkSpec{..} -> do
    if _slPVPMajorLinks
    then do
      let target = mconcat $ either (:[]) (T.unpack . prettyVer) <$> _slTarget
      targetResolved <- binarySymLinkDestination bindir (toolBinDir </> target)
      handle
          (\(e :: ParseError) -> logWarn (T.pack $ displayException e) >> pure Nothing)
        $ do
          when (null . snd . partitionEithers $ _slLinkName) $ throwE $ MalformedInstallInfo "No ${PKGVER} found in linkName"
          (mj, mi) <- getMajorMinorV ver
          let major' = T.unpack (intToText mj <> "." <> intToText mi)
              link_name = mconcat $ either (:[]) (const major') <$> _slLinkName
          logDebug2 $ T.pack (show _slLinkName)
          logDebug2 $ T.pack (show link_name)
          logDebug2 $ T.pack (show major')

          -- if our tool has only two components anyway, we skip this
          if (intToText mj <> "." <> intToText mi) == prettyVer ver
          then pure Nothing
          else pure (Just (targetResolved, bindir </> link_name))
    else pure Nothing

getPVPMajorSymlinks ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => [SymlinkSpec [Either Char Version]]
  -> Version
  -> FilePath
  -> Excepts '[MalformedInstallInfo] m [(FilePath, FilePath)]
getPVPMajorSymlinks specs ver toolBinDir = do
  Dirs {..}  <- getDirs
  getPVPMajorSymlinks' binDir specs ver toolBinDir

getPVPSymlinks' ::
  ( MonadReader env m
  , HasLog env
  , MonadIOish m
  )
  => FilePath
  -> [SymlinkFileSpec]
  -> FilePath
  -> m [(FilePath, FilePath)]
getPVPSymlinks' bindir specs toolDir = do
  forM specs $ \SymlinkSpec{..} -> do
      target <- binarySymLinkDestination bindir (toolDir </> _slTarget)
      pure (target, bindir </> _slLinkName)

getPVPSymlinks ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => [SymlinkFileSpec]
  -> FilePath
  -> m [(FilePath, FilePath)]
getPVPSymlinks specs toolDir = do
  Dirs {..}  <- getDirs
  getPVPSymlinks' binDir specs toolDir
