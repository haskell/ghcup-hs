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
import Control.Monad.ST
import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.STRef
import Data.Set             ( Set )
import Data.Text            ( Text )
import Data.Time.Calendar   ( Day )
import Data.Traversable.WithIndex ( iforM )
import Data.Variant.Excepts
import Data.Versions        hiding ( patch )
import Optics
import Prelude              hiding ( abs, writeFile )

import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map






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
  -> ShowRevisions
  -> Bool
  -> Bool
  -> (Maybe Day, Maybe Day)
  -> Excepts '[ParseError] m ToolListResult
listVersions lt' criteria showRevisions hideOld showNightly days = do
  GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
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

  let avToolsProcessed = runST $ do
        stRefAvToolsProcessed <- newSTRef mempty

        -- process installed tools first
        void $ iforM instTools $ \tool targetMap -> do
          iforM targetMap $ \target (vers', mset) ->
            forM_ vers' $ \vr@VersionRev{..} -> do
              let tver = TargetVersion target _vrVersion
                  mvm = getVersionMetadata tver tool dls
                  tags = maybe [] _vmTags mvm
                  lReleaseDay = mvm >>= _vmReleaseDay
                  lRev = (_vrRev, RevNormal)
                  tDesc = preview (_GHCupDownloads % ix tool % toolDetails % _Just) dls

                  hlsPowered = getHlsPowered tool target _vrVersion hlsGHCs

              -- lNoBindist and lStray are updated when we traverse the metadata
              -- bindist tags (as opposed to tool tags) will also be added later
              let lr = ListResult { lVer = _vrVersion
                                  , lCross = target
                                  , lTag = tags
                                  , lSet = mset == Just vr
                                  , lInstalled = True
                                  , lNoBindist = False
                                  , lStray = True
                                  , ..
                                  }

              insertListResult stRefAvToolsProcessed tool target tDesc _vrVersion _vrRev lr

        installedProcessed <- readSTRef stRefAvToolsProcessed

        -- then process tools in the metadata, that are not installed
        -- and add the installed ones in the process
        void $ iforM avTools $ \tool targetMap -> do
          let tDesc = preview (ix tool % _1 % _Just) installedProcessed <|> preview (_GHCupDownloads % ix tool % toolDetails % _Just) dls
          iforM targetMap $ \target vers' ->
            forM_ vers' $ \(ver', VersionMetadata{..}) -> do
              -- versions from the metadata that's already installed
              let avInstVers :: M.Map Version (M.Map Int ListResult) =
                    fromMaybe mempty $ do
                      (_, m) <- installedProcessed M.!? tool
                      m M.!? target

              let lReleaseDay = _vmReleaseDay

              let latestMetaRev = maximum $ M.keys (unRev _vmRevisionSpec)


              -- add all revision
              forM_ (M.toList (unRev _vmRevisionSpec)) $ \(metaRev, vi) -> do
                let mdli = getDownloadInfo' pfreq vi
                let bTags = fromMaybe [] $ mdli >>= _dlTag

                -- this has to consider the installed rev as well,
                -- which may or may not be in the metadata

                case iheadOf (ix ver' % itraversed) avInstVers of
                  -- it's installed
                  Just (installedRev, installedLr) -> do
                    if installedRev == metaRev
                    then do -- and even the same revision
                      -- add information from the metadata to the installed rev
                      modifySTRef' stRefAvToolsProcessed
                        (over (ix tool % _2 % ix target % ix ver' % ix installedRev) $ \lr' -> lr'
                          & lNoBindistL .~ (isNothing mdli && not (lStray installedLr))
                          & lTagL       %~ (<> bTags)
                          & lStrayL     .~ False
                        )
                    else do -- installed, but not this revision
                      when (showRevisions /= ShowNone) $ do

                        -- is it the latest revision?
                        let isLatestRev = metaRev == latestMetaRev && (metaRev > installedRev)
                        -- it could be an update
                        let isUpdate = isLatestRev

                        -- tag shenanigans
                        let dropOld = if isUpdate then filter (/= Old) else id
                            filterNotLatestRec = filter (`notElem` [Latest, Recommended])
                            dropLatestRec = if isLatestRev then id else filterNotLatestRec

                        when isUpdate $
                          -- remove the latest/recommended tag from the installed revision
                          modifySTRef' stRefAvToolsProcessed
                            (over (ix tool % _2 % ix target % ix ver' % ix installedRev) $ \lr' -> lr'
                              & lRevL % _2 .~ RevOutdated
                              & lTagL      %~ filterNotLatestRec
                            )

                        -- Insert the installed revision
                        -- updates are always inserted, but non-updates are only relevant if the user wants to see all revisions
                        let lrNew = ListResult
                                      { lVer = ver'
                                      , lCross = target
                                      , lTag = dropLatestRec $ dropOld (_vmTags <> bTags)
                                      , lSet = False
                                      , lInstalled = False
                                      , hlsPowered = False
                                      , lNoBindist = isNothing mdli
                                      , lStray = False
                                      , lRev = (metaRev, if isUpdate then RevUpdate else RevNormal)
                                      , ..
                                      }
                        when (showRevisions == ShowAll || isUpdate) $
                          insertListResult stRefAvToolsProcessed tool target tDesc ver' metaRev lrNew
                  -- not installed, just insert
                  Nothing -> do
                    let isLatestRev = metaRev == latestMetaRev
                    let lr = ListResult
                               { lVer = ver'
                               , lCross = target
                               , lTag = _vmTags <> bTags
                               , lSet = False
                               , lInstalled = False
                               , hlsPowered = False
                               , lNoBindist = isNothing mdli
                               , lStray = False
                               , lRev = (metaRev, RevNormal)
                               , ..
                               }
                    when (showRevisions == ShowAll || isLatestRev) $
                      insertListResult stRefAvToolsProcessed tool target tDesc ver' metaRev lr
        readSTRef stRefAvToolsProcessed

  pure $ toToolListResult avToolsProcessed

 where
  toToolListResult :: ProcessedListResult -> ToolListResult
  toToolListResult = M.map (second (filter' . toListOf (traversed % traversed % traversed)))

  insertListResult ::
       STRef s ProcessedListResult
    -> Tool
    -> Maybe Text
    -> Maybe ToolDescription
    -> Version
    -> Int
    -> ListResult
    -> ST s ()
  insertListResult ref tool target tDesc ver' rev lr =
    modifySTRef' ref
    (at tool % non (tDesc, mempty) % _2 % at target % non mempty % at ver' % non mempty % at rev ?~ lr)

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

