{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : GHCup.Command.List
Description : Listing versions and tools
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Command.List where

import GHCup.Errors
import GHCup.Query.DB
import GHCup.Query.DB.HLS   ( getHLSGHCs )
import GHCup.Query.Metadata
import GHCup.Types
import GHCup.Types.JSON
    ()
import GHCup.Types.Optics

import Control.Applicative
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Data.Bifunctor
import Data.Either
import Data.IORef
import Data.List
import Data.Maybe
import Data.Set             ( Set )
import Data.Text            ( Text )
import Data.Time.Calendar   ( Day )
import Data.Variant.Excepts
import Data.Versions        hiding ( patch )
import Optics
import Prelude              hiding ( abs, writeFile )

import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set






    ------------------
    --[ List tools ]--
    ------------------


-- | Filter data type for 'listVersions'.
data ListCriteria
  = ListInstalled Bool
  | ListSet Bool
  | ListAvailable Bool
  deriving (Eq, Show)


type ToolListResult = M.Map Tool (Maybe ToolDescription, [ListResult])
type ProcessedListResult = M.Map Tool (Maybe ToolDescription, M.Map (Maybe Text) (M.Map Version (M.Map Int ListResult)))

data RevTag = RevUpdate
            | RevOutdated
            | RevNormal
  deriving (Eq, Ord, Show)

-- | A list result describes a single tool version
-- and various of its properties.
data ListResult = ListResult
  { lVer :: Version
  , lCross :: Maybe Text
    -- ^ currently only for GHC
  , lRev :: (Int, RevTag)  -- the Bool indicates whether this is an update from an older rev
  , lTag :: [Tag]
  , lInstalled :: Bool
  , lSet :: Bool
    -- ^ currently active version
  , lStray :: Bool
    -- ^ not in download info
  , lNoBindist :: Bool
    -- ^ whether the version is available for this platform/arch
  , hlsPowered :: Bool
  , lReleaseDay :: Maybe Day
  }
  deriving (Eq, Ord, Show)


makeLensesWith (lensRules & lensField .~ mappingNamer (\n -> [n <> "L"])) ''ListResult

-- | Extract all available tool versions and their tags.
availableToolVersions :: GHCupDownloads -> Tool -> Map.Map TargetVersion VersionMetadata
availableToolVersions av tool = view
  (_GHCupDownloads % at tool % to ((unToolVersionSpec . _toolVersions) <$>) % non Map.empty)
  av


-- | List all versions from the download info, as well as stray
-- versions.
listVersions ::
  forall m env .
  ( HasLog env
  , MonadReader env m
  , HasDirs env
  , HasPlatformReq env
  , HasGHCupInfo env
  , MonadIOish m
  )
  => Maybe [Tool]
  -> [ListCriteria]
  -> Bool
  -> Bool
  -> (Maybe Day, Maybe Day)
  -> Excepts '[ParseError] m ToolListResult
-- TODO: show latest revision if available
listVersions lt' criteria hideOld showNightly days = do
  ghcinfo@GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
  pfreq <- lift getPlatformReq
  instTools <- getAllInstalledTools lt'
  hlsGHCs <- let hlsSet = do
                   hlsMap <- instTools M.!? hls
                   (_, mSet) <- hlsMap M.!? Nothing
                   mSet
             in case hlsSet of
               Just VersionRev{..} -> do
                 getHLSGHCs _vrVersion
               Nothing -> pure []

  -- available tools from the metadata
  let allAvailableTools' :: [(Tool, [(TargetVersion, VersionMetadata)])]
        = maybe (allAvailableTools dls) (\ts -> filter (\(t, _) -> t `elem` ts) $ allAvailableTools dls) lt'
  let avTools :: M.Map Tool (M.Map (Maybe Text) (Set (Version, VersionMetadata)))
        = M.fromList $ (fmap . fmap) groupByTargetS allAvailableTools'

  -- ioref for recording observed versions from the metadata
  ioRefAvToolsProcessed :: IORef ProcessedListResult <- liftIO $ newIORef mempty


  -- process installed tools first
  forM_ (M.toList instTools) $ \(tool, targetMap) -> do
    forM_ (M.toList targetMap) $ \(target, (vers', mset)) ->
      forM_ vers' $ \vr@VersionRev{..} -> do
        -- versions from the metadata
        let avVers :: Set (Version, VersionMetadata) = fromMaybe mempty $ (avTools M.!? tool) >>= (M.!? target)

        let tver = TargetVersion target _vrVersion
        let treq = TargetVersionReq tver (Just _vrRev)
        -- we fetch the tags from the metadata first, because e.g. 'Recommended' and 'Latest'
        -- are rolling tags and may be more up to date there
        dli <- case getDownloadInfo tool treq ghcinfo pfreq of
          Left _ -> fmap (veitherToEither . fmap _imDownloadInfo) $ runE (getInstallMetadata tool tver)
          Right (_rev, m) -> pure (Right m)
        let bTags = either (const []) (fromMaybe [] . _dlTag) dli

        let mvm = getVersionMetadata tver tool dls
        let tags = maybe [] _vmTags mvm
        let lReleaseDay = mvm >>= _vmReleaseDay
        let lStray = _vrVersion `Set.notMember` (fst `Set.map` avVers)
        let lRev = (_vrRev, RevNormal)
        let tDesc = preview (_GHCupDownloads % ix tool % toolDetails % _Just) dls

        let hlsPowered = getHlsPowered tool target _vrVersion hlsGHCs
        let lr = ListResult { lVer = _vrVersion
                            , lCross = target
                            , lTag = tags <> bTags
                            , lSet = mset == Just vr
                            , lInstalled = True
                            , lNoBindist = isLeft dli && not lStray
                            , ..
                            }

        liftIO $ modifyIORef' ioRefAvToolsProcessed (M.alter (alterTools vr lr target tDesc) tool)

  avToolsProcessed <- liftIO $ readIORef ioRefAvToolsProcessed

  -- then process tools in the metadata, that are not installed
  -- and add the installed ones in the process
  forM_ (M.toList avTools) $ \(tool, targetMap) -> do
    let tDesc = preview (ix tool % _1 % _Just) avToolsProcessed <|> preview (_GHCupDownloads % ix tool % toolDetails % _Just) dls
    forM_ (M.toList targetMap) $ \(target, vers') ->
      forM_ (Set.toList vers') $ \(ver', vm@VersionMetadata{..}) -> do
        -- versions from the metadata that's already installed
        let avInstVers :: M.Map Version (M.Map Int ListResult) =
              fromMaybe mempty $ do
                (_, m) <- avToolsProcessed M.!? tool
                m M.!? target

        let mdli = do
                    (rev, vi) <- preview (selectRevL Nothing) vm
                    dli <- getDownloadInfo' pfreq vi
                    pure (rev, dli)
        let latestRev = maybe 0 fst mdli

        let bTags = fromMaybe [] $ mdli >>= _dlTag . snd
        let lReleaseDay = _vmReleaseDay

        let hlsPowered = getHlsPowered tool target ver' hlsGHCs

        -- check if the same version is installed
        case maybe [] M.toList $ M.lookup ver' avInstVers of
          -- since only one revision can be installed, we assume
          -- there are no further revisions in the output
          ((rev, lr):_) -> do
            let vr = VersionRev ver' rev
            -- check if it's the same revision
            if rev == latestRev
            then do
              liftIO $ modifyIORef' ioRefAvToolsProcessed (M.alter (alterTools vr lr target tDesc) tool)
            else do
              let lrNew = ListResult
                            { lVer = ver'
                            , lCross = target
                            , lTag = filter (/= Old) (_vmTags <> bTags)
                            , lSet = False
                            , lInstalled = False
                            , lNoBindist = isNothing mdli
                            , lStray = False
                            , lRev = (latestRev, RevUpdate)
                            , ..
                            }

              liftIO $ modifyIORef' ioRefAvToolsProcessed
                (over (ix tool % _2 % ix target % ix ver' % ix rev) $ \lr' -> lr'
                  & lRevL % _2 .~ RevOutdated
                  & lTagL      %~ filter (`notElem` [Latest, Recommended])
                )
              liftIO $ modifyIORef' ioRefAvToolsProcessed (M.alter (alterTools (VersionRev (_vrVersion vr) latestRev) lrNew target tDesc) tool)
          [] -> do
            let lr = ListResult
                       { lVer = ver'
                       , lCross = target
                       , lTag = _vmTags <> bTags
                       , lSet = False
                       , lInstalled = False
                       , lNoBindist = isNothing mdli
                       , lStray = False
                       , lRev = (latestRev, RevNormal)
                       , ..
                       }
            liftIO $ modifyIORef' ioRefAvToolsProcessed (M.alter (alterTools (VersionRev ver' latestRev) lr target tDesc) tool)

  toToolListResult <$> liftIO (readIORef ioRefAvToolsProcessed)

 where
  toToolListResult :: ProcessedListResult -> ToolListResult
  toToolListResult = M.map (second (filter' . toListOf (traversed % traversed % traversed)))

  -- TODO: probably easier with lens
  alterTools ::
       VersionRev
    -> ListResult
    -> Maybe Text
    -> Maybe ToolDescription
    -> Maybe (Maybe ToolDescription, M.Map (Maybe Text) (M.Map Version (M.Map Int ListResult)))
    -> Maybe (Maybe ToolDescription, M.Map (Maybe Text) (M.Map Version (M.Map Int ListResult)))
  alterTools (VersionRev{..}) lr target tDesc Nothing       =
    Just (tDesc, M.singleton target (M.singleton _vrVersion (M.singleton _vrRev lr)))
  alterTools (VersionRev{..}) lr target tDesc (Just (_, m)) =
    Just (tDesc, M.alter alterTarget target m)
   where
    alterTarget :: Maybe (M.Map Version (M.Map Int ListResult))
                -> Maybe (M.Map Version (M.Map Int ListResult))
    alterTarget Nothing   = Just $ M.singleton _vrVersion (M.singleton _vrRev lr)
    alterTarget (Just xs) = Just $ M.alter alterVersion _vrVersion xs
     where
      alterVersion :: Maybe (M.Map Int ListResult)
                   -> Maybe (M.Map Int ListResult)
      alterVersion Nothing   = Just $ M.singleton _vrRev lr
      alterVersion (Just ys) = Just $ M.insert _vrRev lr ys

  getHlsPowered tool target ver' hlsGHCs =
    (tool == ghc && isNothing target) && ver' `elem` hlsGHCs


  filter' :: [ListResult] -> [ListResult]
  filter' = filterNightly . filterOld . filter (\lr -> foldr (\a b -> fromCriteria a lr && b) True criteria) . filterDays

  filterDays :: [ListResult] -> [ListResult]
  filterDays lrs = case days of
                     (Nothing, Nothing)    -> lrs
                     (Just from, Just to') -> filter (\ListResult{..} -> maybe False (\d -> d >= from && d <= to') lReleaseDay) lrs
                     (Nothing, Just to')   -> filter (\ListResult{..} -> maybe False (<= to')                      lReleaseDay) lrs
                     (Just from, Nothing)  -> filter (\ListResult{..} -> maybe False (>= from)                     lReleaseDay) lrs

  fromCriteria :: ListCriteria -> ListResult -> Bool
  fromCriteria lc ListResult{..} = case lc of
    ListInstalled  b -> f b lInstalled
    ListSet        b -> f b lSet
    ListAvailable  b -> f b $ not lNoBindist
   where
    f b
      | b         = id
      | otherwise = not

  filterOld :: [ListResult] -> [ListResult]
  filterOld lr
    | hideOld   = filter (\ListResult {..} -> lInstalled || Old `notElem` lTag) lr
    | otherwise = lr

  filterNightly :: [ListResult] -> [ListResult]
  filterNightly lr
    | showNightly = lr
    | otherwise   = filter (\ListResult {..} -> lInstalled || (Nightly `notElem` lTag && LatestNightly `notElem` lTag)) lr

