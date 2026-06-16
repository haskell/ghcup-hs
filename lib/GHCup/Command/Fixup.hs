{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Command.Fixup where

import GHCup.Command.Install.LowLevel
import GHCup.Command.List
import GHCup.Errors
import GHCup.Prelude
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.Types
import GHCup.Types.Optics

import Control.Monad                  ( forM_, when )
import Control.Monad.Reader           ( MonadReader )
import Data.Variant.Excepts
import Text.PrettyPrint.HughesPJClass ( prettyShow )

import qualified Data.Map.Strict as M
import qualified Data.Text       as T


fixupSymlinks ::
  ( HasLog env
  , MonadReader env m
  , HasDirs env
  , HasPlatformReq env
  , HasGHCupInfo env
  , MonadIOish m
  )
  => Excepts '[MalformedInstallInfo, ParseError] m ()
fixupSymlinks = do
  Dirs{..} <- getDirs
  tlr <- liftE $ listVersions Nothing [ListInstalled True] ShowNone False NShowAll (Nothing, Nothing)

  forM_ (M.toList $ M.filterWithKey (\t _ -> t /= ghcup) tlr) $ \(tool, (_mtd, lrs)) ->
    forM_ lrs $ \ListResult{..} -> do
      let tver = TargetVersion lCross lVer
      logInfo $ "Fixing symlinks for " <> T.pack (prettyShow tool) <> " version " <> T.pack (prettyShow tver)
      idr <- toolInstallDestination tool tver
      rawSpec <- liftE $ getSymlinkSpecPortable' tool tver
      when (null rawSpec) $ logWarn $ "Could not find symlink specification in the DB. Consider reinstalling " <> T.pack (prettyShow tool) <> "-" <> T.pack (prettyShow tver)
      liftE $ symlinkBinaries (GHCupDir idr) rawSpec (GHCupBinDir binDir) tool tver

