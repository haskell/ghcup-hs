{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import GHCup.Download
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

-- | A list result describes a single tool version
-- and various of its properties.
data ListResult = ListResult
  { lVer :: Version
  , lCross :: Maybe Text
    -- ^ currently only for GHC
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


-- | Extract all available tool versions and their tags.
availableToolVersions :: GHCupDownloads -> Tool -> Map.Map TargetVersion VersionInfo
availableToolVersions av tool = view
  (at tool % to (_toolVersions <$>) % non Map.empty)
  av


-- | List all versions from the download info, as well as stray
-- versions.
listVersions ::
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
listVersions lt' criteria hideOld showNightly days = do
  GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
  instTools <- getAllInstalledTools lt'

  -- available tools from the metadata
  let allAvailableTools' = maybe (allAvailableTools dls) (\ts -> filter (\(t, _) -> t `elem` ts) $ allAvailableTools dls) lt'
  let avTools :: M.Map Tool (M.Map (Maybe Text) (Set Version))
        = M.fromList $ (fmap . fmap) groupByTarget' allAvailableTools'

  -- ioref for recording observed versions from the metadata
  ioRefAvToolsProcessed :: IORef (M.Map Tool (M.Map (Maybe Text) (Set Version))) <- liftIO $ newIORef mempty

  -- process installed tools first
  lInst :: ToolListResult <- fmap M.fromList $ forM instTools $ \(tool, targetMap) -> do
    toolVersions' <- forM (M.toList targetMap) $ \(target, (vers', mset)) ->
      forM vers' $ \ver' -> do
        -- versions from the metadata
        let avVers :: Set Version = fromMaybe mempty $ (avTools M.!? tool) >>= (M.!? target)

        let tver = TargetVersion target ver'
        -- we fetch the tags from the metadata first, because e.g. 'Recommended' and 'Latest'
        -- are rolling tags and may be more up to date there
        dli <- runE @'[NoDownload] (getDownloadInfo' tool tver) >>= \case
          VLeft _ -> fmap (veitherToEither . fmap _imDownloadInfo) $ runE (getInstallMetadata tool tver)
          VRight m -> pure (Right m)
        let bTags = either (const []) (fromMaybe [] . _dlTag) dli

        let mvi = getVersionInfo tver tool dls
        let tags = maybe [] _viTags mvi
        let lReleaseDay = mvi >>= _viReleaseDay
        let lStray = ver' `Set.notMember` avVers

        -- record observed versions that are also in the metadata, so
        -- we can skip them in the second pass
        when (not lStray) $ do
          let alterAvTools Nothing  = Just $ M.singleton target (Set.singleton ver')
              alterAvTools (Just m) = Just $ M.alter alterAvTarget target m
              alterAvTarget Nothing   = Just $ Set.singleton ver'
              alterAvTarget (Just xs) = Just $ Set.insert ver' xs
          liftIO $ modifyIORef ioRefAvToolsProcessed (M.alter alterAvTools tool)

        hlsPowered <- getHlsPowered tool target ver'
        pure ListResult { lVer = ver'
                        , lCross = target
                        , lTag = tags <> bTags
                        , lSet = mset == Just ver'
                        , lInstalled = True
                        , lNoBindist = isLeft dli && not lStray
                        , ..
                        }

    let tDesc = preview (ix tool % toolDetails % _Just) dls
    pure (tool, (tDesc, mconcat toolVersions'))

  avToolsProcessed <- liftIO $ readIORef ioRefAvToolsProcessed

  -- then process tools in the metadata, that are not installed
  lAv <- fmap M.fromList $ forM (M.toList avTools) $ \(tool, targetMap) -> do
    toolVersions' <- forM (M.toList targetMap) $ \(target, vers') -> fmap catMaybes $
      forM (Set.toList vers') $ \ver' -> do
        -- versions from the metadata that's already installed
        let avInstVers :: Set Version = fromMaybe mempty $ (avToolsProcessed M.!? tool) >>= (M.!? target)

        if Set.member ver' avInstVers
        then do
          pure Nothing
        else do
          let tver = TargetVersion target ver'
          dli <- fmap veitherToEither $ runE @'[NoDownload] $ getDownloadInfo' tool tver
          let bTags = either (const []) (fromMaybe [] . _dlTag) dli
          let mvi = getVersionInfo tver tool dls
          let tags = maybe [] _viTags mvi
          let lReleaseDay = mvi >>= _viReleaseDay

          hlsPowered <- getHlsPowered tool target ver'

          pure $ Just $ ListResult { lVer = ver'
                                   , lCross = target
                                   , lTag = tags <> bTags
                                   , lSet = False
                                   , lInstalled = False
                                   , lNoBindist = isLeft dli
                                   , lStray = False
                                   , ..
                                   }
    let tDesc = preview (ix tool % toolDetails % _Just) dls
    pure (tool, (tDesc, mconcat toolVersions'))

  pure $ M.unionWith (\(d, vs) (d', vs') -> (d <|> d', sort $ filter' (vs <> vs'))) lInst lAv

 where
  getHlsPowered tool target ver' =
    if tool == ghc && isNothing target
    then do
      hlsSet <- liftE $ getSetVersion' hls Nothing
      case hlsSet of
        Just (hlsVer, _) -> do
          ghcs <- getHLSGHCs hlsVer
          pure (ver' `elem` ghcs)
        Nothing -> pure False
    else pure False



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

