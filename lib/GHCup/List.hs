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
import           Data.Versions                hiding ( patch )
import           Haskus.Utils.Variant.Excepts
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
data ListCriteria = ListInstalled
                  | ListSet
                  | ListAvailable
                  deriving Show

-- | A list result describes a single tool version
-- and various of its properties.
data ListResult = ListResult
  { lTool      :: Tool
  , lVer       :: Version
  , lCross     :: Maybe Text -- ^ currently only for GHC
  , lTag       :: [Tag]
  , lInstalled :: Bool
  , lSet       :: Bool -- ^ currently active version
  , fromSrc    :: Bool -- ^ compiled from source
  , lStray     :: Bool -- ^ not in download info
  , lNoBindist :: Bool -- ^ whether the version is available for this platform/arch
  , hlsPowered :: Bool
  }
  deriving (Eq, Ord, Show)


-- | Extract all available tool versions and their tags.
availableToolVersions :: GHCupDownloads -> Tool -> Map.Map Version VersionInfo
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
             -> Maybe ListCriteria
             -> m [ListResult]
listVersions lt' criteria = do
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
            slr <- strayCabals avTools cSet cabals
            pure (sort (slr ++ lr))
          HLS -> do
            slr <- strayHLS avTools hlsSet' hlses
            pure (sort (slr ++ lr))
          Stack -> do
            slr <- strayStacks avTools sSet stacks
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
            => Map.Map Version VersionInfo
            -> m [ListResult]
  strayGHCs avTools = do
    ghcs <- getInstalledGHCs
    fmap catMaybes $ forM ghcs $ \case
      Right tver@GHCTargetVersion{ _tvTarget = Nothing, .. } -> do
        case Map.lookup _tvVersion avTools of
          Just _  -> pure Nothing
          Nothing -> do
            lSet    <- fmap (maybe False (\(GHCTargetVersion _ v ) -> v == _tvVersion)) $ ghcSet Nothing
            fromSrc <- ghcSrcInstalled tver
            hlsPowered <- fmap (elem _tvVersion) hlsGHCVersions
            pure $ Just $ ListResult
              { lTool      = GHC
              , lVer       = _tvVersion
              , lCross     = Nothing
              , lTag       = []
              , lInstalled = True
             , lStray     = isNothing (Map.lookup _tvVersion avTools)
              , lNoBindist = False
              , ..
              }
      Right tver@GHCTargetVersion{ .. } -> do
        lSet    <- fmap (maybe False (\(GHCTargetVersion _ v ) -> v == _tvVersion)) $ ghcSet _tvTarget
        fromSrc <- ghcSrcInstalled tver
        hlsPowered <- fmap (elem _tvVersion) hlsGHCVersions
        pure $ Just $ ListResult
          { lTool      = GHC
          , lVer       = _tvVersion
          , lCross     = _tvTarget
          , lTag       = []
          , lInstalled = True
          , lStray     = True -- NOTE: cross currently cannot be installed via bindist
          , lNoBindist = False
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
              , fromSrc    = False -- actually, we don't know :>
              , hlsPowered = False
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
              , fromSrc    = False -- actually, we don't know :>
              , hlsPowered = False
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
              , fromSrc    = False -- actually, we don't know :>
              , hlsPowered = False
              , ..
              }
      Left e -> do
        logWarn
          $ "Could not parse version of stray directory" <> T.pack e
        pure Nothing

  currentGHCup :: Map.Map Version VersionInfo -> Maybe ListResult
  currentGHCup av =
    let currentVer = fromJust $ pvpToVersion ghcUpVer ""
        listVer    = Map.lookup currentVer av
        latestVer  = fst <$> headOf (getTagged Latest) av
        recommendedVer = fst <$> headOf (getTagged Latest) av
        isOld  = maybe True (> currentVer) latestVer && maybe True (> currentVer) recommendedVer
    in if | Map.member currentVer av -> Nothing
          | otherwise -> Just $ ListResult { lVer    = currentVer
                                           , lTag    = maybe (if isOld then [Old] else []) (view viTags) listVer
                                           , lCross  = Nothing
                                           , lTool   = GHCup
                                           , fromSrc = False
                                           , lStray  = isNothing listVer
                                           , lSet    = True
                                           , lInstalled = True
                                           , lNoBindist = False
                                           , hlsPowered = False
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
               -> (Version, VersionInfo)
               -> m ListResult
  toListResult t cSet cabals hlsSet' hlses stackSet' stacks (v, vi) = do
    let tags = view viTags vi
    case t of
      GHC -> do
        lNoBindist <- fmap (isLeft . veitherToEither) $ runE @'[NoDownload] $ getDownloadInfo GHC v
        let tver = mkTVer v
        lSet       <- fmap (maybe False (\(GHCTargetVersion _ v') -> v' == v)) $ ghcSet Nothing
        lInstalled <- ghcInstalled tver
        fromSrc    <- ghcSrcInstalled tver
        hlsPowered <- fmap (elem v) hlsGHCVersions
        pure ListResult { lVer = v, lCross = Nothing , lTag = tags, lTool = t, lStray = False, .. }
      Cabal -> do
        lNoBindist <- fmap (isLeft . veitherToEither) $ runE @'[NoDownload] $ getDownloadInfo Cabal v
        let lSet = cSet == Just v
        let lInstalled = elem v $ rights cabals
        pure ListResult { lVer    = v
                        , lCross  = Nothing
                        , lTag    = tags
                        , lTool   = t
                        , fromSrc = False
                        , lStray  = False
                        , hlsPowered = False
                        , ..
                        }
      GHCup -> do
        let lSet       = prettyPVP ghcUpVer == prettyVer v
        let lInstalled = lSet
        pure ListResult { lVer    = v
                        , lTag    = tags
                        , lCross  = Nothing
                        , lTool   = t
                        , fromSrc = False
                        , lStray  = False
                        , lNoBindist = False
                        , hlsPowered = False
                        , ..
                        }
      HLS -> do
        lNoBindist <- fmap (isLeft . veitherToEither) $ runE @'[NoDownload] $ getDownloadInfo HLS v
        let lSet = hlsSet' == Just v
        let lInstalled = elem v $ rights hlses
        pure ListResult { lVer    = v
                        , lCross  = Nothing
                        , lTag    = tags
                        , lTool   = t
                        , fromSrc = False
                        , lStray  = False
                        , hlsPowered = False
                        , ..
                        }
      Stack -> do
        lNoBindist <- fmap (isLeft . veitherToEither) $ runE @'[NoDownload] $ getDownloadInfo Stack v
        let lSet = stackSet' == Just v
        let lInstalled = elem v $ rights stacks
        pure ListResult { lVer    = v
                        , lCross  = Nothing
                        , lTag    = tags
                        , lTool   = t
                        , fromSrc = False
                        , lStray  = False
                        , hlsPowered = False
                        , ..
                        }


  filter' :: [ListResult] -> [ListResult]
  filter' lr = case criteria of
    Nothing            -> lr
    Just ListInstalled -> filter (\ListResult {..} -> lInstalled) lr
    Just ListSet       -> filter (\ListResult {..} -> lSet) lr
    Just ListAvailable -> filter (\ListResult {..} -> not lNoBindist) lr

