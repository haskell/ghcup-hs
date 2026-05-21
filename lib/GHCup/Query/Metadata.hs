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
import Safe                 ( headMay, lastMay )
import URI.ByteString       ( URI )

import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import Control.Monad.Trans (lift)
import Control.Monad (guard)
import GHCup.Prelude



    --------------------
    --[ Requirements ]--
    --------------------


getDownloadInfoE ::
  ( MonadReader env m
  , HasPlatformReq env
  , HasGHCupInfo env
  )
  => Tool
  -> VersionReq
  -- ^ tool version
  -> Excepts
       '[NoDownload]
       m
       (Int, DownloadInfo)
getDownloadInfoE t VersionReq{..} = getDownloadInfoE' t (TargetVersionReq (mkTVer _vqVersion) _vqRev)


getDownloadInfoE' ::
  ( MonadReader env m
  , HasPlatformReq env
  , HasGHCupInfo env
  )
  => Tool
  -> TargetVersionReq
  -- ^ tool version
  -> Excepts
       '[NoDownload]
       m
       (Int, DownloadInfo)
getDownloadInfoE' t tvr = do
  pfreq <- lift getPlatformReq
  ghcupInfo <- lift getGHCupInfo
  lE $ getDownloadInfo t tvr ghcupInfo pfreq


getDownloadInfo ::
     Tool
  -> TargetVersionReq
  -> GHCupInfo
  -> PlatformRequest
  -> Either NoDownload (Int, DownloadInfo)
getDownloadInfo t tvr@TargetVersionReq{..} (GHCupInfo { _ghcupDownloads = dls }) pfreq =
  let mRev_Vi = preview (_GHCupDownloads
                        % ix t
                        % toolVersionsL
                        % ix _tvqTargetVer
                        % revisionSpecL
                        % ixOrLast _tvqRev
                        ) dls
  in maybe (Left $ NoDownload tvr t (Just pfreq)) Right (mRev_Vi >>= \(rev, vi) -> (rev,) <$> getDownloadInfo' pfreq vi)


getDownloadInfo' ::
     PlatformRequest
  -> VersionInfo
  -> Maybe DownloadInfo
getDownloadInfo' (PlatformRequest a p mv) vi =
  let distro_preview f g =
        let platformVersionSpec = preview
                                      ( archL
                                      % to unMapIgnoreUnknownKeys
                                      % ix a
                                      % _PlatformSpec
                                      % to unMapIgnoreUnknownKeys
                                      % ix (f p)
                                      % _PlatformVersionSpec
                                      ) vi
            mv' = g mv
        in  (\m ->
                fmap snd
              . find
                  (\(mverRange, _) -> maybe
                     (isNothing mv')
                     (\range -> maybe False (`versionRange` range) mv')
                     mverRange
                  )
              . M.toList
              $ m
            ) =<< platformVersionSpec
      with_distro        = distro_preview id id
      without_distro_ver = distro_preview id (const Nothing)
      without_distro     = distro_preview (set _Linux UnknownLinux) (const Nothing)

  in case p of
       -- non-musl won't work on alpine
       Linux Alpine -> with_distro <|> without_distro_ver
       _            -> with_distro <|> without_distro_ver <|> without_distro


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
          <> "\n    "
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
                 -> m (Maybe (PVP, VersionMetadata, Maybe Text))
getLatestToolFor tool target pvpIn dls = do
  let ls :: [(TargetVersion, VersionMetadata)]
      ls = fromMaybe [] $ preview (_GHCupDownloads % ix tool % toolVersionsL % to Map.toDescList) dls
  let ps :: [(PVP, VersionMetadata, Maybe Text)]
      ps = catMaybes $ fmap (\(v, vm) -> do
             (pvp, unparsable) <- versionToPVP (_tvVersion v)
             guard (T.null unparsable) -- TODO: hm
             pure (pvp, vm, _tvTarget v)
           ) ls
  pure . headMay . filter (\(v, _, t) -> matchPVPrefix pvpIn v && t == target) $ ps


    ------------
    --[ Tags ]--
    ------------


selectLatestRevL :: Getter ToolVersionSpec (M.Map TargetVersion (Int, VersionInfo))
selectLatestRevL = _ToolVersionSpec % to (M.mapMaybe (lastMay . Map.toAscList . Map.mapKeys unRev . unRevisionSpec . _vmRevisionSpec))

selectRevL :: Maybe Int -> AffineFold VersionMetadata (Int, VersionInfo)
selectRevL mrev = revisionSpecL % ixOrLast mrev

-- | Get the tool version that has this tag. If multiple have it,
-- picks the greatest version.
getTaggedL' :: Tag
            -> Lens' a [Tag]
            -> Fold (Map.Map TargetVersion a) (TargetVersion, a)
getTaggedL' tag getTags =
  to (Map.toDescList . Map.filter (\a -> tag `elem` fromMaybe [] (preview getTags a)))
  % folding id

getTaggedL :: Tag
           -> Fold ToolVersionSpec (TargetVersion, VersionMetadata)
getTaggedL tag = _ToolVersionSpec % getTaggedL' tag vmTags

getFirstTag :: Tag -> GHCupDownloads -> Tool -> Maybe TargetVersion
getFirstTag tag av tool = do
  (tv, _) <- headOf (_GHCupDownloads % ix tool % toolVersions % getTaggedL tag) av
  pure tv

getRev :: GHCupDownloads -> Tool -> TargetVersion -> Maybe Int -> Maybe (VersionMetadata, Int, VersionInfo)
getRev av tool tv mRev = do
  vm <- preview (_GHCupDownloads % ix tool % toolVersionsL % ix tv) av
  (rev, vi) <- preview (selectRevL mRev) vm
  pure (vm, rev, vi)

getByReleaseDay :: GHCupDownloads -> Tool -> Day -> Either (Maybe Day) (TargetVersion, VersionMetadata)
getByReleaseDay av tool day =
  let mvv = fromMaybe mempty $ headOf (_GHCupDownloads % ix tool % toolVersions % _ToolVersionSpec) av
      mdv = Map.foldrWithKey (\k vi@VersionMetadata{..} m ->
                maybe m (\d -> let diff = diffDays d day
                               in Map.insert (abs diff) (diff, (k, vi)) m) _vmReleaseDay)
              Map.empty
              mvv
  in case headMay (Map.toAscList mdv) of
       Nothing -> Left Nothing
       Just (absDiff, (diff, (k, vi)))
         | absDiff == 0 -> Right (k, vi)
         | otherwise -> Left (Just (addDays diff day))

getByReleaseDayFold :: Day -> Fold (Map.Map TargetVersion VersionMetadata) TargetVersion
getByReleaseDayFold day = to (fmap fst . Map.toDescList . Map.filter (\VersionMetadata {..} -> Just day == _vmReleaseDay)) % folding id

getLatest :: GHCupDownloads -> Tool -> Maybe TargetVersion
getLatest = getFirstTag Latest

getLatestPrerelease :: GHCupDownloads -> Tool -> Maybe TargetVersion
getLatestPrerelease = getFirstTag LatestPrerelease

getLatestNightly :: GHCupDownloads -> Tool -> Maybe TargetVersion
getLatestNightly = getFirstTag LatestNightly

getRecommended :: GHCupDownloads -> Tool -> Maybe TargetVersion
getRecommended = getFirstTag Recommended

-- | Gets the latest GHC with a given base version.
getLatestBaseVersion :: GHCupDownloads -> PVP -> Maybe TargetVersion
getLatestBaseVersion g pvpVer = getFirstTag (Base pvpVer) g ghc


getVersionMetadata :: TargetVersion
                   -> Tool
                   -> GHCupDownloads
                   -> Maybe VersionMetadata
getVersionMetadata v' tool =
  headOf
    ( _GHCupDownloads
    % ix tool
    % toolVersions
    % _ToolVersionSpec
    % to (Map.filterWithKey (\k _ -> k == v'))
    % to Map.elems
    % _head
    )


    -----------------
    --[ Changelog ]--
    -----------------


getChangeLog :: GHCupDownloads -> Tool -> TargetVersion -> Maybe URI
getChangeLog dls tool tv =
  preview (_GHCupDownloads % ix tool % toolVersionsL % ix tv % vmChangeLog % _Just) dls



    -------------
    --[ Other ]--
    -------------


-- NOTE: there might be installed tools that are not in the available tools set
-- (e.g. because the metadata was removed)
allAvailableTools :: GHCupDownloads -> [(Tool, [(TargetVersion, VersionMetadata)])]
allAvailableTools = Map.toList . Map.mapWithKey (\_ (ToolInfo (ToolVersionSpec v) _) -> Map.toList v) . unGHCupDownloads

installationSpecFromMetadata' ::
  ( MonadReader env m
  , HasPlatformReq env
  )
  => DownloadInfo
  -> Tool
  -> TargetVersion
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
  -> TargetVersionReq
  -> Excepts '[NoDownload, NoInstallInfo] m (Int, InstallationSpecInput)
installationSpecFromMetadata tool tver = do
  pfreq <- lift getPlatformReq
  ghcupInfo <- lift getGHCupInfo
  (rev, dlinfo) <- lE $ getDownloadInfo tool tver ghcupInfo pfreq
  spec <- liftE $ installationSpecFromMetadata' dlinfo tool (_tvqTargetVer tver)
  pure (rev, spec)

