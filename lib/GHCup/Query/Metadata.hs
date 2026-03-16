{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : GHCup.Query.Metadata
Description : Queries on GHCup metadata and its data
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Query.Metadata where

import GHCup.Errors
import GHCup.Prelude.Version
import GHCup.Types
import GHCup.Types.JSON
    ()
import GHCup.Types.Optics

import Control.Applicative
import Control.Monad.Catch  ( MonadThrow )
import Control.Monad.Reader ( MonadReader )
import Data.List            ( find )
import Data.Maybe
import Data.Text            ( Text )
import Data.Time            ( Day, addDays, diffDays )
import Data.Variant.Excepts ( Excepts, throwE, liftE )
import Data.Versions        ( PVP )
import Optics
import Prelude              hiding ( readFile, writeFile )
import Safe                 ( headMay )
import URI.ByteString       ( URI )

import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import Control.Monad.Trans (lift)
import GHCup.Download (getDownloadInfo')


    --------------------
    --[ Requirements ]--
    --------------------

-- | Get the requirements. Right now this combines GHC and cabal
-- and doesn't do fine-grained distinction. However, the 'ToolRequirements'
-- type allows it.
getCommonRequirements :: Tool
                      -> PlatformResult
                      -> ToolRequirements
                      -> Maybe Requirements
getCommonRequirements tool pr tr =
  with_distro <|> without_distro_ver <|> without_distro
 where
  with_distro        = distro_preview _platform _distroVersion
  without_distro_ver = distro_preview _platform (const Nothing)
  without_distro     = distro_preview (set _Linux UnknownLinux . _platform) (const Nothing)

  distro_preview f g =
    let platformVersionSpec =
          preview (ix tool % ix Nothing % to unMapIgnoreUnknownKeys % ix (f pr)) tr
        mv' = g pr
    in  fmap snd
          .   find
                (\(mverRange, _) -> maybe
                  (isNothing mv')
                  (\range -> maybe False (`versionRange` range) mv')
                  mverRange
                )
          .   M.toList
          =<< platformVersionSpec


prettyRequirements :: Requirements -> T.Text
prettyRequirements Requirements {..} =
  let d = if not . null $ _distroPKGs
        then "\n  Please ensure the following distro packages "
          <> "are installed before continuing (you can exit ghcup "
          <> "and return at any time): "
          <> T.intercalate " " _distroPKGs
        else ""
      n = if not . T.null $ _notes then "\n  Note: " <> _notes else ""
  in  "System requirements " <> d <> n

rawRequirements :: Requirements -> T.Text
rawRequirements Requirements {..} =
  if not . null $ _distroPKGs
  then T.intercalate " " _distroPKGs
  else ""



    ----------------
    --[ Versions ]--
    ----------------


-- | Get the latest available ghc for the given PVP version, which
-- may only contain parts.
--
-- >>> (fmap . fmap) (\(p, _, _) -> p) $ getLatestToolFor GHC Nothing [pver|8|] r
-- Just (PVP {_pComponents = 8 :| [10,7]})
-- >>> (fmap . fmap) (\(p, _, _) -> p) $ getLatestToolFor GHC Nothing [pver|8.8|] r
-- Just (PVP {_pComponents = 8 :| [8,4]})
-- >>> (fmap . fmap) (\(p, _, _) -> p) $ getLatestToolFor GHC Nothing [pver|8.8.4|] r
-- Just (PVP {_pComponents = 8 :| [8,4]})
getLatestToolFor :: MonadThrow m
                 => Tool
                 -> Maybe Text
                 -> PVP
                 -> GHCupDownloads
                 -> m (Maybe (PVP, VersionInfo, Maybe Text))
getLatestToolFor tool target pvpIn dls = do
  let ls :: [(GHCTargetVersion, VersionInfo)]
      ls = fromMaybe [] $ preview (ix tool % toolVersions % to Map.toDescList) dls
  let ps :: [((PVP, Text), VersionInfo, Maybe Text)]
      ps = catMaybes $ fmap (\(v, vi) -> (,vi, _tvTarget v) <$> versionToPVP (_tvVersion v)) ls
  pure . fmap (\((pv', _), vi, mt) -> (pv', vi, mt)) . headMay . filter (\((v, _), _, t) -> matchPVPrefix pvpIn v && t == target) $ ps


    ------------
    --[ Tags ]--
    ------------


-- | Get the tool version that has this tag. If multiple have it,
-- picks the greatest version.
getTagged :: Tag
          -> Fold (Map.Map GHCTargetVersion VersionInfo) (GHCTargetVersion, VersionInfo)
getTagged tag =
  to (Map.toDescList . Map.filter (\VersionInfo {..} -> tag `elem` _viTags))
  % folding id

getByReleaseDay :: GHCupDownloads -> Tool -> Day -> Either (Maybe Day) (GHCTargetVersion, VersionInfo)
getByReleaseDay av tool day = let mvv = fromMaybe mempty $ headOf (ix tool % toolVersions) av
                                  mdv = Map.foldrWithKey (\k vi@VersionInfo{..} m ->
                                            maybe m (\d -> let diff = diffDays d day
                                                           in Map.insert (abs diff) (diff, (k, vi)) m) _viReleaseDay)
                                          Map.empty mvv
                              in case headMay (Map.toAscList mdv) of
                                   Nothing -> Left Nothing
                                   Just (absDiff, (diff, (k, vi)))
                                     | absDiff == 0 -> Right (k, vi)
                                     | otherwise -> Left (Just (addDays diff day))

getByReleaseDayFold :: Day -> Fold (Map.Map GHCTargetVersion VersionInfo) (GHCTargetVersion, VersionInfo)
getByReleaseDayFold day = to (Map.toDescList . Map.filter (\VersionInfo {..} -> Just day == _viReleaseDay)) % folding id

getLatest :: GHCupDownloads -> Tool -> Maybe (GHCTargetVersion, VersionInfo)
getLatest av tool = headOf (ix tool % toolVersions % getTagged Latest) av

getLatestPrerelease :: GHCupDownloads -> Tool -> Maybe (GHCTargetVersion, VersionInfo)
getLatestPrerelease av tool = headOf (ix tool % toolVersions % getTagged LatestPrerelease) av

getLatestNightly :: GHCupDownloads -> Tool -> Maybe (GHCTargetVersion, VersionInfo)
getLatestNightly av tool = headOf (ix tool % toolVersions % getTagged LatestNightly) av

getRecommended :: GHCupDownloads -> Tool -> Maybe (GHCTargetVersion, VersionInfo)
getRecommended av tool = headOf (ix tool % toolVersions % getTagged Recommended) av


-- | Gets the latest GHC with a given base version.
getLatestBaseVersion :: GHCupDownloads -> PVP -> Maybe (GHCTargetVersion, VersionInfo)
getLatestBaseVersion av pvpVer =
  headOf (ix ghc % toolVersions % getTagged (Base pvpVer)) av


getVersionInfo :: GHCTargetVersion
               -> Tool
               -> GHCupDownloads
               -> Maybe VersionInfo
getVersionInfo v' tool =
  headOf
    ( ix tool
    % toolVersions
    % to (Map.filterWithKey (\k _ -> k == v'))
    % to Map.elems
    % _head
    )


    -----------------
    --[ Changelog ]--
    -----------------


getChangeLog :: GHCupDownloads -> Tool -> ToolVersion -> Maybe URI
getChangeLog dls tool (GHCVersion v') =
  preview (ix tool % toolVersions % ix v' % viChangeLog % _Just) dls
getChangeLog dls tool (ToolVersion (mkTVer -> v')) =
  preview (ix tool % toolVersions % ix v' % viChangeLog % _Just) dls
getChangeLog dls tool (ToolTag tag) =
  preview (ix tool % toolVersions % pre (getTagged tag) % to snd % viChangeLog % _Just) dls
getChangeLog dls tool (ToolDay day) =
  preview (ix tool % toolVersions % pre (getByReleaseDayFold day) % to snd % viChangeLog % _Just) dls



    -------------
    --[ Other ]--
    -------------


-- NOTE: there might be installed tools that are not in the available tools set
-- (e.g. because the metadata was removed)
allAvailableTools :: GHCupDownloads -> [(Tool, [GHCTargetVersion])]
allAvailableTools = Map.toList . Map.mapWithKey (\_ (ToolInfo v _) -> Map.keys v)

installationSpecFromMetadata' ::
  ( MonadReader env m
  , HasPlatformReq env
  )
  => DownloadInfo
  -> Tool
  -> GHCTargetVersion
  -> Excepts '[NoInstallInfo] m InstallationSpecInput
installationSpecFromMetadata' dlinfo tool tver = do
  pfreq <- lift getPlatformReq
  case _dlInstallSpec dlinfo of
    Just installInfo -> pure installInfo
    Nothing
     | Just i <- toInstallationInputSpec <$> defaultToolInstallSpec tool pfreq tver
     -> pure i
     | otherwise
     -> throwE $ NoInstallInfo tool tver

-- has defaults, so works with legacy metadata
installationSpecFromMetadata ::
  ( MonadReader env m
  , HasGHCupInfo env
  , HasPlatformReq env
  )
  => Tool
  -> GHCTargetVersion
  -> Excepts '[NoDownload, NoInstallInfo] m InstallationSpecInput
installationSpecFromMetadata tool tver = do
  dlinfo <- liftE $ getDownloadInfo' tool tver
  liftE $ installationSpecFromMetadata' dlinfo tool tver
