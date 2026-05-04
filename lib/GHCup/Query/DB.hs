{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module GHCup.Query.DB where

import GHCup.Errors
import GHCup.Hardcoded.Version
import GHCup.Input.SymlinkSpec
import GHCup.Legacy.Utils
import GHCup.Prelude
import GHCup.Prelude.MegaParsec
import GHCup.Query.GHCupDirs
import GHCup.System.Directory
import GHCup.Types
import GHCup.Types.Optics

import Control.DeepSeq                ( force )
import Control.Exception
    ( Exception (displayException), SomeException, evaluate )
import Control.Exception.Safe         ( try )
import Control.Monad                  ( forM )
import Control.Monad.Catch            ( MonadCatch )
import Control.Monad.IO.Class         ( MonadIO, liftIO )
import Control.Monad.Reader           ( MonadReader )
import Control.Monad.Trans            ( lift )
import Data.Either                    ( rights )
import Data.Functor                   ( (<&>) )
import Data.List                      ( nub )
import Data.Set                       ( Set )
import Data.Text                      ( Text )
import Data.Variant.Excepts
    ( Excepts, liftE, pattern V, pattern VLeft, pattern VRight, runE, throwE )
import Data.Versions                  ( Version, version )
import Data.Yaml                      ( decodeEither' )
import Optics                         ( preview, (%) )
import System.FilePath                ( takeExtension, (</>) )
import System.IO.Error                ( doesNotExistErrorType )
import Text.PrettyPrint.HughesPJClass ( prettyShow )

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified Text.Megaparsec as MP



    --------------------
    --[ DownloadInfo ]--
    --------------------


getInstallMetadata ::
  ( MonadIO m
  , MonadCatch m
  , MonadReader env m
  , HasDirs env
  , MonadFail m
  )
  => Tool
  -> TargetVersion
  -> Excepts '[FileDoesNotExistError, ParseError] m InstallMetadata
getInstallMetadata tool tver = do
  f <- lift $ recordedInstallationSpecFile tool tver
  -- we have to trigger 'doesNotExistErrorType' explicitly, since libyaml swallows it, so
  -- we have to avoid 'decodeFileEither':
  --   https://github.com/snoyberg/yaml/blob/7380d7f560daa2f45ff265d425866f497ca07966/libyaml/src/Text/Libyaml.hs#L656-L657
  r <- liftIOException doesNotExistErrorType (FileDoesNotExistError f) $ liftIO $ do
    contents <- B.readFile f
    pure $ decodeEither' contents
  either (throwE . ParseError . displayException) pure r



    --------------------
    --[ Symlink spec ]--
    --------------------


getSymlinkSpec ::
  ( MonadReader env m
  , HasDirs env
  , HasPlatformReq env
  , HasLog env
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
  -> Excepts '[FileDoesNotExistError, ParseError, NoInstallInfo] m [SymlinkFileSpec]
getSymlinkSpec tool tver = do
  spec <- getSymlinkSpec' tool tver

  -- substitute
  pure $ spec <&> substituteSpec

getSymlinkSpec' ::
  ( MonadReader env m
  , HasDirs env
  , HasPlatformReq env
  , HasLog env
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
  -> Excepts
       '[FileDoesNotExistError, ParseError, NoInstallInfo]
       m
       [SymlinkSpec [Either Char Version]]
getSymlinkSpec' tool tver = do
  dli <- liftE $ getInstallMetadata tool tver
  logDebug2 $ T.pack $ show dli
  spec <- preview (imResolvedInstallSpec % isExeSymLinked) dli ?? NoInstallInfo tool tver

  forM spec (liftE . parseSymlinkSpec (_tvVersion tver))

getSymlinkSpecPortable ::
  ( MonadReader env m
  , HasDirs env
  , HasPlatformReq env
  , HasLog env
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
  -> m [SymlinkFileSpec]
getSymlinkSpecPortable tool tver = do
  runE (getSymlinkSpec tool tver) >>= \case
    VRight r -> pure r
    VLeft (V pe@(ParseError _)) -> fail $ prettyHFError pe
    VLeft _ -> do -- legacy
      if | tool == ghc   -> do
             pfreq <- getPlatformReq
             pure $ defaultGHCExeSymLinked pfreq tver (ghcBinaries pfreq tver)
         | tool == cabal -> pure []
         | tool == stack -> pure []
         | tool == hls   -> pure []
         | tool == ghcup -> pure [] -- Hm
         | otherwise     -> pure []




    -----------------
    --[ Installed ]--
    -----------------


-- | Returns 'Nothing' for legacy installs.
getInstalledFiles :: ( MonadIO m
                     , MonadCatch m
                     , MonadReader env m
                     , HasDirs env
                     , MonadFail m
                     )
                  => Tool
                  -> TargetVersion
                  -> m (Maybe [FilePath])
getInstalledFiles t v' = hideErrorDef [doesNotExistErrorType] Nothing $ do
  f <- recordedInstallationFile t v'
  (force -> !c) <- liftIO
    (readFile f >>= evaluate)
  pure (Just $ lines c)


isInstalled ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
  -> m Bool
isInstalled tool ver
  | tool == ghc =
      ifM isInstalledNew (pure True) $ ghcInstalled ver
  | tool == hls =
      ifM isInstalledNew (pure True) $ hlsInstalled (_tvVersion ver)
  | tool == cabal =
      ifM isInstalledNew (pure True) $ cabalInstalled (_tvVersion ver)
  | tool == stack =
      ifM isInstalledNew (pure True) $ stackInstalled (_tvVersion ver)
  | tool == ghcup =
      pure $ ghcUpVer' == ver
  | otherwise = isInstalledNew
 where
   -- for new installations we can just check the DB
  isInstalledNew = do
    f <- recordedInstallationFile tool ver
    liftIO (doesFileExist f)

getInstalledVersions ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => Tool
  -> Maybe T.Text
  -> m [Version]
getInstalledVersions tool mtarget = do
  r <- getInstalledVersions' tool
  pure $ extract' r
 where
  extract' = fmap _tvVersion . filter (\TargetVersion{..} -> _tvTarget == mtarget)

getInstalledVersions' ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => Tool
  -> m [TargetVersion]
getInstalledVersions' tool
  | tool == ghc = do
      new <- getInstalledNew
      legacy <- fmap rights getInstalledGHCs
      pure $ nub (new ++ legacy)
  | tool == cabal = do
      new <- getInstalledNew
      legacy <- fmap (fmap mkTVer . rights) getInstalledCabals
      pure $ nub (new ++ legacy)
  | tool == stack = do
      new <- getInstalledNew
      legacy <- fmap (fmap mkTVer . rights)  getInstalledStacks
      pure $ nub (new ++ legacy)
  | tool == hls = do
      new <- getInstalledNew
      legacy <- fmap (fmap mkTVer . rights) getInstalledHLSs
      pure $ nub (new ++ legacy)
  | tool == ghcup = do
      pure [ghcUpVer']
  | otherwise = getInstalledNew
 where
  parseGHCVer = throwEither . MP.parse ghcTargetVerP "getInstalledVersions'" . T.pack
  getInstalledNew = do
    Dirs {..}  <- getDirs
    let dbPath = fromGHCupPath dbDir </> prettyShow tool
    -- we have to ignore '.spec' and the 'set' file
    contents <- fmap (filter (\f -> takeExtension f `notElem` [".spec", ".set"] && f /= "set"))
      $ liftIO $ handleIO' doesNotExistErrorType (\_ -> pure []) $ listDirectoryFiles dbPath
    forM contents parseGHCVer

getAllInstalledTools ::
  ( MonadReader env m
  , HasGHCupInfo env
  , HasDirs env
  , HasPlatformReq env
  , HasLog env
  , MonadIOish m
  )
  => Maybe [Tool]
  -> Excepts '[ParseError] m [(Tool, Map.Map (Maybe Text) ([Version], Maybe Version))]
getAllInstalledTools mtools = do
  Dirs{..} <- lift getDirs
  tools <- case mtools of
    Nothing -> do
      newTools <- fmap Tool <$> liftIO (listDirectoryDirs $ fromGHCupPath dbDir)
      pure (nub $ ghcup:ghc:cabal:hls:stack:newTools)
    Just tools' -> pure (nub tools')
  forM tools $ \newTool -> do
    vs <- lift $ getInstalledVersions' newTool
    -- add information on which is the 'set' version, if any
    nm <- Map.traverseWithKey (trav newTool) (groupByTarget vs)

    pure (newTool, nm)
 where
  trav tool mtarget vers' = do
    mv <- getSetVersion tool mtarget
    pure (vers', mv)



    -----------
    --[ Set ]--
    -----------


getSetVersion' ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => Tool
  -> Maybe T.Text
  -> Excepts '[ParseError] m (Maybe (Version, Maybe FilePath))
getSetVersion' tool target = do
  setFile <- lift $ recordedSetVersionFile tool target
  ec <- liftIO $ try @_ @SomeException (T.readFile setFile)
  case ec of
    Left _
      | tool == ghc ->
          lift $ fmap ((,Nothing) . _tvVersion) <$> ghcSet target
      | tool == cabal ->
          lift $ fmap (,Nothing) <$> cabalSet
      | tool == hls ->
          lift $ fmap (,Nothing) <$> hlsSet
      | tool == stack ->
          lift $ fmap (,Nothing) <$> stackSet
      | tool == ghcup ->
          pure (Just (_tvVersion ghcUpVer', Nothing))
      | otherwise -> pure Nothing
    Right c -> do
      ver <- either (throwE . ParseError . displayException) pure . version $ c
      pure $ Just (ver, Just setFile)

getSetVersion ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => Tool
  -> Maybe T.Text
  -> Excepts '[ParseError] m (Maybe Version)
getSetVersion tool = (fmap . fmap) fst . getSetVersion' tool

isSet ::
  ( MonadReader env m
  , HasDirs env
  , HasLog env
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
  -> Excepts '[ParseError] m Bool
isSet tool tver = do
  v <- (fmap . fmap) fst . getSetVersion' tool $ _tvTarget tver
  pure $ Just (_tvVersion tver) == v




    -------------
    --[ Other ]--
    -------------


groupByTarget :: [TargetVersion] -> Map.Map (Maybe Text) [Version]
groupByTarget = foldr (\TargetVersion{..} -> Map.alter (f _tvVersion) _tvTarget) mempty
 where
  f tvVersion' Nothing   = Just [tvVersion']
  f tvVersion' (Just xs) = Just (tvVersion':xs)

groupByTarget' :: [TargetVersion] -> Map.Map (Maybe Text) (Set Version)
groupByTarget' = foldr (\TargetVersion{..} -> Map.alter (f _tvVersion) _tvTarget) mempty
 where
  f tvVersion' Nothing   = Just $ Set.singleton tvVersion'
  f tvVersion' (Just xs) = Just $ Set.insert tvVersion' xs
