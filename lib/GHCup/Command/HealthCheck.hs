{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}

module GHCup.Command.HealthCheck where

import GHCup.Errors
import GHCup.Legacy.Utils
import GHCup.Prelude
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.Types
import GHCup.Types.Optics

import Control.Exception
    (SomeException )
import Control.Exception.Safe         ( try, handleIO )
import Control.Monad                  ( forM )
import Control.Monad.IO.Class         ( liftIO )
import Control.Monad.Reader           ( MonadReader )
import Data.Functor                   ( (<&>) )
import Data.List                      ( foldl', sort )
import Data.Variant.Excepts
    ( pattern V, pattern VLeft, pattern VRight, runE )
import System.Directory               ( doesFileExist )
import System.FilePath                ( pathSeparator, (</>) )
import Text.PrettyPrint.HughesPJClass (Pretty(..), text, nest, vcat, ($$), ($+$))
import Witherable                     ( catMaybes, forMaybe )


data SymlinkState = Fine FilePath FilePath
                  | DestinationDoesn'tExist FilePath FilePath
                  | WrongDestination FilePath FilePath FilePath
                  | AllWrong FilePath FilePath FilePath
                  | Missing FilePath
  deriving (Show, Ord, Eq)

instance Pretty SymlinkState where
  pPrint (DestinationDoesn'tExist f dest) = vcat $ fmap text
    [ "- Target does not exist:"
    , "  * Symlink: " <> f
    , "  * Target: "  <> dest
    ]
  pPrint (WrongDestination f should actual) = vcat $ fmap text
    [ "- Wrong target:"
    , "  * Symlink: " <> f
    , "  * Target (should): "  <> should
    , "  * Target (actual): "  <> actual
    ]
  pPrint (AllWrong f should actual) = vcat $ fmap text
    [ "- Wrong and broken target:"
    , "  * Symlink: " <> f
    , "  * Target (should): "  <> should
    , "  * Target (actual and broken): "  <> actual
    ]
  pPrint (Missing f) = vcat $ fmap text
    [ "- Symlink missing:"
    , "  * Symlink: " <> f
    ]
  pPrint (Fine f link) = vcat $ fmap text
    [ "- All good:"
    , "  * Symlink: " <> f
    , "  * Target: "  <> link
    ]

data ToolHealthCheck = ToolHealthCheck {
    thHaveInstFile   :: (FilePath, Bool)
  , thHaveSpecFile   :: (FilePath, Bool)
  , thParseSpecFile  :: Bool
  , thInstallDest    :: FilePath
  , thSupposedInstalledFiles :: Maybe Int
  , thMissingFiles   :: Maybe [FilePath]
  , thStrayFiles     :: Maybe [FilePath]
  , thSymlinkState   :: Maybe [SymlinkState]
  , thSetState       :: Maybe [SymlinkState]
  }
  deriving (Show)

instance Pretty ToolHealthCheck where
  pPrint ToolHealthCheck{..} = vcat
    [ let (f, e) = thHaveInstFile in text $ "Installed artifacts file exists at " <> f <> ": " <> show e
    , let (f, e) = thHaveSpecFile in text $ "Installed specification file exists at " <> f <> ": " <> show e
    , text $ "Can parse specification file: " <> show thParseSpecFile
    , text $ "Install destination: " <> show thInstallDest
    , text $ "Number of files that should be installed: " <> maybe "(no information)" show thSupposedInstalledFiles
    , text "Missing files:" $$ nest 4 (ppFiles thMissingFiles)
    , text "Stray files:" $$ nest 4 (ppFiles thStrayFiles)
    , text "Symlink status:" $$ nest 4 (ppFiles thSymlinkState)
    , text "Set symlink status:" $$ nest 4 (ppFiles thSetState)
    ]
   where
    ppFiles Nothing   = text "(no information)"
    ppFiles (Just []) = text "none"
    ppFiles (Just fs) = foldl' ($+$) mempty (map pPrint $ sort fs)

-- TODO
--   -     (_:_:_) -> lift $ do
--            logWarn $ "Looks like you have a corrupted DB/installation\n"
--                    <> "More than one revision installed of: " <> T.pack (prettyShow tool)
--   - detect legacy installations
dbHealthCheck ::
  ( MonadReader env m
  , HasDirs env
  , HasPlatformReq env
  , HasLog env
  , MonadIOish m
  )
  => Tool
  -> TargetVersion
  -> m ToolHealthCheck
dbHealthCheck tool tver = do
  Dirs{..} <- getDirs

  instFile <- recordedInstallationFile tool tver
  thHaveInstFile <- fmap (instFile,) $ liftIO $ doesFileExist instFile
  toolDest <- toolInstallDestination tool tver
  let thInstallDest = fromGHCupPath toolDest
  specFile <- recordedInstallationSpecFile tool tver

  (thHaveSpecFile, thParseSpecFile) <- fmap (\(a, b) -> ((specFile, a), b)) $ runE (getInstallMetadata tool tver) >>= \case
    VRight _ -> pure (True, True)
    VLeft (V (FileDoesNotExistError{})) -> pure (False, False)
    VLeft (V (ParseError{})) -> pure (True, False)
    VLeft _ -> fail "oops"

  symLSpec <- runE (getSymlinkSpec tool tver)

  thSymlinkState <- case symLSpec of
    VRight spec -> fmap Just $ do
      forMaybe spec $ \(SymlinkSpec target link _ _) -> do
        inspectSymlink (binDir </> link) target binDir toolDest
    VLeft _ -> pure Nothing

  installedFiles <- getInstalledFiles tool tver

  allFiles <- fmap (dropPrefix (fromGHCupPath toolDest <> [pathSeparator])) <$> liftIO (handleIO (\_ -> pure []) $ getFilesDeep toolDest)
  let thStrayFiles :: Maybe [FilePath] = installedFiles <&> \thif -> catMaybes $ allFiles <&> \f -> if f `notElem` thif then Just f else Nothing

  thMissingFiles <- forM installedFiles $ \mf -> forMaybe mf $ \f -> do
    b <- liftIO $ doesFileExist (fromGHCupPath toolDest </> f)
    if b then pure Nothing else pure (Just f)

  let thSupposedInstalledFiles = length <$> installedFiles

  thSetState <- runE (isSet tool tver) >>= \case
    VRight (Just _)
      | VRight spec <- symLSpec
      -> fmap Just $ forMaybe spec $ \case
           (SymlinkSpec _ _ _ Nothing) -> pure Nothing
           (SymlinkSpec target _ _ (Just setName)) -> inspectSymlink (binDir </> setName) target binDir toolDest
    VRight (Just _)
      | VLeft _ <- symLSpec
      -> pure Nothing
    VRight Nothing -> pure Nothing
    VLeft _ -> pure Nothing

  pure ToolHealthCheck{..}
 where
  inspectSymlink f target bindir tooldest = do
    mtfp <- try @_ @SomeException $ liftIO $ getLinkTarget f
    case mtfp of
      Left _ -> pure $ Just $ Missing f
      Right tfp -> do
        dest <- binarySymLinkDestination bindir (fromGHCupPath tooldest </> target)
        broken <- liftIO $ isBrokenSymlink f
        pure $ Just $ if | broken
                         , tfp /= dest
                         -> AllWrong f dest tfp
                         | broken
                         -> DestinationDoesn'tExist f tfp
                         | tfp /= dest
                         -> WrongDestination f dest tfp
                         | otherwise
                         -> Fine f dest

