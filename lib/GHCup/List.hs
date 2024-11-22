{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : GHCup.List
Description : Listing versions and tools
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.List where

import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Prelude.Logger
import           GHCup.Version

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Time.Calendar             ( Day )
import           Data.Versions                hiding ( patch )
import           Data.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , writeFile
                                                )

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T










    ------------------
    --[ List tools ]--
    ------------------


-- | Filter data type for 'listVersions'.
data ListCriteria = ListInstalled  Bool
                  | ListSet        Bool
                  | ListAvailable  Bool
                  deriving (Eq, Show)

-- | A list result describes a single tool version
-- and various of its properties.
data ListResult = ListResult
  { lTool      :: Tool
  , lVer       :: Version
  , lCross     :: Maybe Text -- ^ currently only for GHC
  , lTag       :: [Tag]
  , lInstalled :: Bool
  , lSet       :: Bool -- ^ currently active version
  , lStray     :: Bool -- ^ not in download info
  , lNoBindist :: Bool -- ^ whether the version is available for this platform/arch
  , hlsPowered :: Bool
  , lReleaseDay :: Maybe Day
  }
  deriving (Eq, Ord, Show)


-- | Extract all available tool versions and their tags.
availableToolVersions :: GHCupDownloads -> Tool -> Map.Map GHCTargetVersion VersionInfo
availableToolVersions av tool = view
  (at tool % non Map.empty)
  av


-- | List all versions from the download info, as well as stray
-- versions.
listVersions :: ( MonadCatch m
                             , HasLog env
                             , MonadThrow m
                             , HasLog env
                             , MonadIO m
                             , MonadReader env m
                             , HasDirs env
                             , HasPlatformReq env
                             , HasGHCupInfo env
                             )
                          => Maybe Tool
                          -> [ListCriteria]
                          -> Bool
                          -> Bool
                          -> (Maybe Day, Maybe Day)
                          -> m [ListResult]
listVersions lt' criteria hideOld showNightly days = do
  -- some annoying work to avoid too much repeated IO
  cSet <- cabalSet
  cabals <- getInstalledCabals
  hlsSet' <- hlsSet
  hlses <- getInstalledHLSs
  sSet <- stackSet
  stacks <- getInstalledStacks

  go lt' cSet cabals hlsSet' hlses sSet stacks
 where
  go lt cSet cabals hlsSet' hlses sSet stacks = do
    case lt of
      Just t -> do
        GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
        -- get versions from GHCupDownloads
        let avTools = availableToolVersions dls t
        lr <- filter' <$> forM (Map.toList avTools) (toListResult t cSet cabals hlsSet' hlses sSet stacks)

        case t of
          GHC -> do
            slr <- strayGHCs avTools
            pure (sort (slr ++ lr))
          Cabal -> do
            slr <- strayCabals (Map.mapKeys _tvVersion avTools) cSet cabals
            pure (sort (slr ++ lr))
          HLS -> do
            slr <- strayHLS (Map.mapKeys _tvVersion avTools) hlsSet' hlses
            pure (sort (slr ++ lr))
          Stack -> do
            slr <- strayStacks (Map.mapKeys _tvVersion avTools) sSet stacks
            pure (sort (slr ++ lr))
          GHCup -> do
            let cg = maybeToList $ currentGHCup avTools
            pure (sort (cg ++ lr))
      Nothing -> do
        ghcvers   <- go (Just GHC) cSet cabals hlsSet' hlses sSet stacks
        cabalvers <- go (Just Cabal) cSet cabals hlsSet' hlses sSet stacks
        hlsvers   <- go (Just HLS) cSet cabals hlsSet' hlses sSet stacks
        ghcupvers <- go (Just GHCup) cSet cabals hlsSet' hlses sSet stacks
        stackvers <- go (Just Stack) cSet cabals hlsSet' hlses sSet stacks
        pure (ghcvers <> cabalvers <> hlsvers <> stackvers <> ghcupvers)
  strayGHCs :: ( MonadCatch m
               , MonadReader env m
               , HasDirs env
               , MonadThrow m
               , HasLog env
               , MonadIO m
               )
            => Map.Map GHCTargetVersion VersionInfo
            -> m [ListResult]
  strayGHCs avTools = do
    ghcs <- getInstalledGHCs
    fmap catMaybes $ forM ghcs $ \case
      Right tver@GHCTargetVersion{ .. } -> do
        case Map.lookup tver avTools of
          Just _  -> pure Nothing
          Nothing -> do
            lSet    <- fmap (maybe False (\(GHCTargetVersion _ v ) -> v == _tvVersion)) $ ghcSet _tvTarget
            hlsPowered <- fmap (elem _tvVersion) hlsGHCVersions
            pure $ Just $ ListResult
              { lTool      = GHC
              , lVer       = _tvVersion
              , lCross     = _tvTarget
              , lTag       = []
              , lInstalled = True
              , lStray     = isNothing (Map.lookup tver avTools)
              , lNoBindist = False
              , lReleaseDay = Nothing
              , ..
              }
      Left e -> do
        logWarn
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  strayCabals :: ( MonadReader env m
                 , HasDirs env
                 , MonadCatch m
                 , MonadThrow m
                 , HasLog env
                 , MonadIO m
                 )
            => Map.Map Version VersionInfo
            -> Maybe Version
            -> [Either FilePath Version]
            -> m [ListResult]
  strayCabals avTools cSet cabals = do
    fmap catMaybes $ forM cabals $ \case
      Right ver ->
        case Map.lookup ver avTools of
          Just _  -> pure Nothing
          Nothing -> do
            let lSet = cSet == Just ver
            pure $ Just $ ListResult
              { lTool      = Cabal
              , lVer       = ver
              , lCross     = Nothing
              , lTag       = []
              , lInstalled = True
              , lStray     = isNothing (Map.lookup ver avTools)
              , lNoBindist = False
              , hlsPowered = False
              , lReleaseDay = Nothing
              , ..
              }
      Left e -> do
        logWarn
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  strayHLS :: ( MonadReader env m
              , HasDirs env
              , MonadCatch m
              , MonadThrow m
              , HasLog env
              , MonadIO m)
           => Map.Map Version VersionInfo
           -> Maybe Version
           -> [Either FilePath Version]
           -> m [ListResult]
  strayHLS avTools hlsSet' hlss = do
    fmap catMaybes $ forM hlss $ \case
      Right ver ->
        case Map.lookup ver avTools of
          Just _  -> pure Nothing
          Nothing -> do
            let lSet = hlsSet' == Just ver
            pure $ Just $ ListResult
              { lTool      = HLS
              , lVer       = ver
              , lCross     = Nothing
              , lTag       = []
              , lInstalled = True
              , lStray     = isNothing (Map.lookup ver avTools)
              , lNoBindist = False
              , hlsPowered = False
              , lReleaseDay = Nothing
              , ..
              }
      Left e -> do
        logWarn
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  strayStacks :: ( MonadReader env m
                 , HasDirs env
                 , MonadCatch m
                 , MonadThrow m
                 , HasLog env
                 , MonadIO m
                 )
              => Map.Map Version VersionInfo
              -> Maybe Version
              -> [Either FilePath Version]
              -> m [ListResult]
  strayStacks avTools stackSet' stacks = do
    fmap catMaybes $ forM stacks $ \case
      Right ver ->
        case Map.lookup ver avTools of
          Just _  -> pure Nothing
          Nothing -> do
            let lSet = stackSet' == Just ver
            pure $ Just $ ListResult
              { lTool      = Stack
              , lVer       = ver
              , lCross     = Nothing
              , lTag       = []
              , lInstalled = True
              , lStray     = isNothing (Map.lookup ver avTools)
              , lNoBindist = False
              , hlsPowered = False
              , lReleaseDay = Nothing
              , ..
              }
      Left e -> do
        logWarn
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  currentGHCup :: Map.Map GHCTargetVersion VersionInfo -> Maybe ListResult
  currentGHCup av =
    let currentVer = mkTVer $ fromJust $ pvpToVersion ghcUpVer ""
        listVer    = Map.lookup currentVer av
        latestVer  = fst <$> headOf (getTagged Latest) av
        recommendedVer = fst <$> headOf (getTagged Latest) av
        isOld  = maybe True (> currentVer) latestVer && maybe True (> currentVer) recommendedVer
    in if | Map.member currentVer av -> Nothing
          | otherwise -> Just $ ListResult { lVer    = _tvVersion currentVer
                                           , lTag    = maybe (if isOld then [Old] else []) _viTags listVer
                                           , lCross  = Nothing
                                           , lTool   = GHCup
                                           , lStray  = isNothing listVer
                                           , lSet    = True
                                           , lInstalled = True
                                           , lNoBindist = False
                                           , hlsPowered = False
                                           , lReleaseDay = Nothing
                                           }

  -- NOTE: this are not cross ones, because no bindists
  toListResult :: ( HasLog env
                  , MonadReader env m
                  , HasDirs env
                  , HasGHCupInfo env
                  , HasPlatformReq env
                  , MonadIO m
                  , MonadCatch m
                  )
               => Tool
               -> Maybe Version
               -> [Either FilePath Version]
               -> Maybe Version
               -> [Either FilePath Version]
               -> Maybe Version
               -> [Either FilePath Version]
               -> (GHCTargetVersion, VersionInfo)
               -> m ListResult
  toListResult t cSet cabals hlsSet' hlses stackSet' stacks (tver, VersionInfo{..}) = do
    let v = _tvVersion tver
    case t of
      GHC -> do
        dli <- fmap veitherToEither $ runE @'[NoDownload] $ getDownloadInfo' GHC tver
        let lNoBindist = isLeft dli
        let bTags = either (const []) (fromMaybe [] . _dlTag) dli
        lSet       <- fmap (== Just tver) $ ghcSet (_tvTarget tver)
        lInstalled <- ghcInstalled tver
        hlsPowered <- fmap (elem tver) (fmap mkTVer <$> hlsGHCVersions)
        pure ListResult { lVer = _tvVersion tver
                        , lCross = _tvTarget tver
                        , lTag = _viTags <> bTags
                        , lTool = t
                        , lStray = False
                        , lReleaseDay = _viReleaseDay
                        , ..
                        }
      Cabal -> do
        dli <- fmap veitherToEither $ runE @'[NoDownload] $ getDownloadInfo Cabal v
        let lNoBindist = isLeft dli
        let bTags = either (const []) (fromMaybe [] . _dlTag) dli
        let lSet = cSet == Just v
        let lInstalled = elem v $ rights cabals
        pure ListResult { lVer    = v
                        , lCross  = Nothing
                        , lTag    = _viTags <> bTags
                        , lTool   = t
                        , lStray  = False
                        , hlsPowered = False
                        , lReleaseDay = _viReleaseDay
                        , ..
                        }
      GHCup -> do
        let lSet       = prettyPVP ghcUpVer == prettyVer v
        let lInstalled = lSet
        pure ListResult { lVer    = v
                        , lTag    = _viTags
                        , lCross  = Nothing
                        , lTool   = t
                        , lStray  = False
                        , lNoBindist = False
                        , hlsPowered = False
                        , lReleaseDay = _viReleaseDay
                        , ..
                        }
      HLS -> do
        dli <- fmap veitherToEither $ runE @'[NoDownload] $ getDownloadInfo HLS v
        let lNoBindist = isLeft dli
        let bTags = either (const []) (fromMaybe [] . _dlTag) dli
        let lSet = hlsSet' == Just v
        let lInstalled = elem v $ rights hlses
        pure ListResult { lVer    = v
                        , lCross  = Nothing
                        , lTag    = _viTags <> bTags
                        , lTool   = t
                        , lStray  = False
                        , hlsPowered = False
                        , lReleaseDay = _viReleaseDay
                        , ..
                        }
      Stack -> do
        dli <- fmap veitherToEither $ runE @'[NoDownload] $ getDownloadInfo Stack v
        let lNoBindist = isLeft dli
        let bTags = either (const []) (fromMaybe [] . _dlTag) dli
        let lSet = stackSet' == Just v
        let lInstalled = elem v $ rights stacks
        pure ListResult { lVer    = v
                        , lCross  = Nothing
                        , lTag    = _viTags <> bTags
                        , lTool   = t
                        , lStray  = False
                        , hlsPowered = False
                        , lReleaseDay = _viReleaseDay
                        , ..
                        }


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

