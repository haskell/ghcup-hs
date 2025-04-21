{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}

module GHCup.Brick.App.CompileGHCMenu where

import qualified GHCup
import           GHCup.Errors
import GHCup.List ( ListResult (..))
import           GHCup.Types         hiding ( LeanAppState(..) )
import qualified GHCup.Utils.Parsers as Utils
import           GHCup.Utils

import qualified GHCup.Brick.Actions as Actions
import GHCup.Brick.Widgets.BaseWidget
import GHCup.Brick.Widgets.BasicOverlay
import GHCup.Brick.Widgets.InputField.Class
import GHCup.Brick.Widgets.InputField.CheckBox
import GHCup.Brick.Widgets.InputField.EditInput as EditInput
import GHCup.Brick.Widgets.InputField.SelectInput as SelectInput
import GHCup.Brick.Widgets.GenericMenu
import qualified GHCup.Brick.App.Common as Common
import qualified GHCup.Brick.Common as Common
import GHCup.Brick.App.Tutorial (Tutorial(..))
import qualified GHCup.GHC as GHC
import           GHCup.Prelude.Logger


import Brick
    ( BrickEvent(..),
      Padding(Max),
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import           Brick.Widgets.Center ( center )
import Control.Applicative (Alternative((<|>)))
import Control.Concurrent (threadDelay)
import           Control.Exception.Safe
import           Control.Monad (when, forM, forM_, void)
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource
import Data.Bifunctor (Bifunctor(..))
import Data.Char (isSpace)
import Data.List.NonEmpty             ( NonEmpty (..) )
import Data.Some
import qualified Data.Text                     as T
import           Data.Variant.Excepts
import Data.Versions (prettyVer, Version)
import GHC.Generics (Generic)
import           Prelude                 hiding ( appendFile )
import qualified Graphics.Vty as Vty
import Optics (Lens', lens, over, (^.), (%), (&), (.~), (%~))
import Optics.State.Operators ((.=), (?=))
import Optics.TH (makeLenses)
import Text.Read (readEither)
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import URI.ByteString (URI)

data CompileGHCMenuFields n = CompileGHCMenuFields
  { _bootstrapGhcF   :: SelectInput n Version (Maybe FilePath)
  , _hadrianGhcF     :: SelectInput n Version (Maybe FilePath)
  , _jobsF           :: EditInput n (Maybe Int)
  , _setCompileF     :: CheckBoxInput n Bool
  , _flavourF        :: EditInput n (Maybe String)
  , _addConfArgsF    :: EditInput n [T.Text]
  , _buildConfigF    :: EditInput n (Maybe FilePath)
  , _patchesF        :: EditInput n (Maybe (Either FilePath [URI]))
  , _crossTargetF    :: EditInput n (Maybe T.Text)
  , _buildSystemF    :: SelectInput n (Maybe BuildSystem) ()
  , _overwriteVerF   :: EditInput n (Maybe [VersionPattern])
  , _isolateDirF     :: EditInput n (Maybe FilePath)
  , _gitRefF         :: EditInput n (Maybe String)
  , _installTargetsF :: EditInput n (T.Text)
  } deriving Generic

data CompileGHCOptions = CompileGHCOptions
  { _bootstrapGhc :: Either Version FilePath
  , _hadrianGhc   :: Maybe (Either Version FilePath)
  , _jobs         :: Maybe Int
  , _buildConfig  :: Maybe FilePath
  , _patches      :: Maybe (Either FilePath [URI])
  , _crossTarget  :: Maybe T.Text
  , _addConfArgs  :: [T.Text]
  , _setCompile   :: Bool
  , _overwriteVer :: Maybe [VersionPattern]
  , _buildFlavour :: Maybe String
  , _buildSystem  :: Maybe BuildSystem
  , _isolateDir   :: Maybe FilePath
  , _gitRef       :: Maybe String
  , _installTargets :: T.Text
  } deriving (Eq, Show)

concat <$> mapM makeLenses [''CompileGHCMenuFields, ''CompileGHCOptions]

type CompileGHCMenu = GenericMenu Common.Name CompileGHCMenuFields (AppState, ListResult) CompileGHCOptions

create :: KeyBindings -> ListResult -> AppState -> [Version] -> CompileGHCMenu
create kb lr s availableGHCs = mkGenericMenu
  Common.CompileGHCBox
  menuFields
  validateInputs
  (s, lr)
  (\(s, lr) opts -> void $ Actions.suspendBrickAndRunAction s $ compileGHC opts lr)
  (Common.toMenuKeyBindings kb)
  "Compile GHC"
  (Button (Common.MenuElement Common.OkButton)
   "Compile"
   "Compile GHC from source with options below\nRequired fields: bootstrap-ghc")
  where
    menuFields = CompileGHCMenuFields
      { _bootstrapGhcF = bootstrapGhcField
      , _hadrianGhcF = hadrianGhcField
      , _jobsF = jobsField
      , _setCompileF = setCompileField
      , _flavourF = flavourField
      , _addConfArgsF = addConfArgsField
      , _buildConfigF = buildConfigField
      , _patchesF = patchesField
      , _crossTargetF = crossTargetField
      , _buildSystemF = buildSystemField
      , _overwriteVerF = overwriteVerField
      , _isolateDirF = isolateDirField
      , _gitRefF = gitRefField
      , _installTargetsF = installTargetsField
      }

    validateInputs :: CompileGHCMenuFields Common.Name -> Either ErrorMessage CompileGHCOptions
    validateInputs CompileGHCMenuFields {..} = do
      let
        setCompileVal = _checked _setCompileF
        isolateDirVal = editInputValue _isolateDirF
        bootstrapGhc = case getSelection _bootstrapGhcF of
          ([], Just (Right (Just path, _))) -> Right $ Right path
          ([], Just (Left msg)) -> Left msg
          ((v:_), _) -> Right $ Left v
          ([], _) -> Left "bootstrap-ghc: No version selected / no path specified"

        hadrianGhc = case getSelection _hadrianGhcF of
          ([], Just (Right (Just path, _))) -> Right $ Just $ Right path
          ([], Just (Left msg)) -> Left msg
          ((v:_), _) -> Right $ Just $ Left v
          ([], _) -> Right $ Nothing

        buildSystem = case getSelection _buildSystemF of
          ([], _) -> Right $ Nothing
          ((v:_), _) -> Right $ v
      case (setCompileVal, isolateDirVal) of
        (True, Right (Just _)) -> Left "Cannot set active when doing an isolated install"
        _ -> CompileGHCOptions
          <$> bootstrapGhc
          <*> hadrianGhc
          <*> editInputValue _jobsF
          <*> editInputValue _buildConfigF
          <*> editInputValue _patchesF
          <*> editInputValue _crossTargetF
          <*> editInputValue _addConfArgsF
          <*> (Right $ _checked $ _setCompileF)
          <*> editInputValue _overwriteVerF
          <*> editInputValue _flavourF
          <*> buildSystem
          <*> editInputValue _isolateDirF
          <*> editInputValue _gitRefF
          <*> editInputValue _installTargetsF

    bootstrapGhcField = SelectInput.createSelectInputWithEditable
      (Common.MenuElement Common.BootstrapGhcSelectBox)
      (Common.MenuElement Common.BootstrapGhcEditBox)
      "bootstrap-ghc"
      "The GHC version (or full path) to bootstrap with (must be installed)"
      availableGHCs
      (T.pack . prettyShow)
      filepathV
      (Common.toMenuKeyBindings kb)

    hadrianGhcField = SelectInput.createSelectInputWithEditable
      (Common.MenuElement Common.HadrianGhcSelectBox)
      (Common.MenuElement Common.HadrianGhcEditBox)
      "hadrian-ghc"
      "The GHC version (or full path) that will be used to compile hadrian (must be installed)"
      availableGHCs
      (T.pack . prettyShow)
      filepathV
      (Common.toMenuKeyBindings kb)

    jobsField = EditInput.create
      (Common.MenuElement Common.JobsEditBox)
      "jobs"
      "How many jobs to use for make"
      jobsV
      ""

    setCompileField = CheckBoxInput
      (Common.MenuElement Common.SetCheckBox)
      "set"
      "Set as active version after install"
      False

    flavourField = EditInput.create
      (Common.MenuElement Common.BuildFlavourEditBox)
      "flavour"
      "Set the compile build flavour (this value depends on the build system type: 'make' vs 'hadrian')"
      (whenEmpty Nothing (Right . Just . T.unpack))
      ""

    addConfArgsField = EditInput.create
      (Common.MenuElement Common.AdditionalEditBox)
      "CONFIGURE_ARGS"
      "Additional arguments to compile configure"
      (Right . T.split isSpace)
      ""

    buildConfigField = EditInput.create
      (Common.MenuElement Common.BuildConfigEditBox)
      "build config"
      "Absolute path to build config file (make build system only)"
      filepathV
      ""

    patchesField = EditInput.create
      (Common.MenuElement Common.PatchesEditBox)
      "patches"
      "Either a URI to a patch (https/http/file) or Absolute path to patch directory"
      patchesV
      ""

    crossTargetField = EditInput.create
      (Common.MenuElement Common.CrossTargetEditBox)
      "cross target"
      "Build cross-compiler for this platform"
      (Right . Just)
      ""

    buildSystemField = SelectInput.createSelectInput
      (Common.MenuElement Common.BuildSystemEditBox)
      "build system"
      "Select the build system"
      (Nothing :| [Just Hadrian, Just Make])
      (\case
        Nothing -> "Auto select (prefer hadrian if available, and build config is not specified)"
        Just Hadrian -> "hadrian"
        Just Make -> "make")
      (Common.toMenuKeyBindings kb)

    overwriteVerField = EditInput.create
      (Common.MenuElement Common.OvewrwiteVerEditBox)
      "overwrite-version"
      "Allows to overwrite the finally installed VERSION with a different one. Allows to specify patterns: %v (version), %b (branch name), %h (short commit hash), %H (long commit hash), %g ('git describe' output)"
      versionV
      ""

    isolateDirField = EditInput.create
      (Common.MenuElement Common.IsolateEditBox)
      "isolated"
      "install in an isolated absolute directory instead of the default one"
      filepathV
      ""

    gitRefField = EditInput.create
      (Common.MenuElement Common.GitRefEditBox)
      "git-ref"
      "The git commit/branch/ref to build from"
      (whenEmpty Nothing (Right . Just . T.unpack))
      ""

    installTargetsField = EditInput.create
      (Common.MenuElement Common.GHCInstallTargets)
      "install-targets"
      "Specify space separated list of make install targets"
      Right
      initialInstallTargets

    initialInstallTargets = "install"

    -- Brick's internal editor representation is [mempty].
    emptyEditor i = T.null i || (i == "\n")
    whenEmpty :: a -> (T.Text -> Either ErrorMessage a) -> T.Text -> Either ErrorMessage a
    whenEmpty emptyval f i = if not (emptyEditor i) then f i else Right emptyval

    filepathV :: T.Text -> Either ErrorMessage (Maybe FilePath)
    filepathV = whenEmpty Nothing (bimap T.pack Just . Utils.absolutePathParser . T.unpack)

    jobsV :: T.Text -> Either ErrorMessage (Maybe Int)
    jobsV =
      let parseInt = bimap (const "Invalid value. Must be an integer") Just . readEither @Int . T.unpack
       in whenEmpty Nothing parseInt

    patchesV :: T.Text -> Either ErrorMessage (Maybe (Either FilePath [URI]))
    patchesV = whenEmpty Nothing readPatches
      where
        readPatches j =
          let
            x = second (Just . Left) $ Utils.absolutePathParser (T.unpack j)
            y = second (Just . Right) $ traverse (Utils.uriParser . T.unpack) (T.split isSpace j)
          in first T.pack $ x <|> y

    versionV :: T.Text -> Either ErrorMessage (Maybe [VersionPattern])
    versionV = whenEmpty Nothing (bimap T.pack Just . Utils.overWriteVersionParser . T.unpack)

updateAvailableGHCs :: [Version] -> CompileGHCMenu -> CompileGHCMenu
updateAvailableGHCs availableGHCs v = v
  & fields % bootstrapGhcF %~ SelectInput.updateItems availableGHCs
  & fields % hadrianGhcF %~ SelectInput.updateItems availableGHCs

compileGHC :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m, Alternative m)
           => CompileGHCOptions -> ListResult -> m (Either String ())
compileGHC compopts lr@ListResult{lTool = GHC, ..} = do
  appstate <- ask
  let run =
        runResourceT
         . runE @'[ AlreadyInstalled
                  , BuildFailed
                  , DigestError
                  , ContentLengthError
                  , GPGError
                  , DownloadFailed
                  , GHCupSetError
                  , NoDownload
                  , NotFoundInPATH
                  , PatchFailed
                  , UnknownArchive
                  , TarDirDoesNotExist
                  , NotInstalled
                  , DirNotEmpty
                  , ArchiveResult
                  , FileDoesNotExistError
                  , HadrianNotFound
                  , InvalidBuildConfig
                  , ProcessError
                  , CopyError
                  , BuildFailed
                  , UninstallFailed
                  , MergeFileTreeError
                  , URIParseError
                  ]
  compileResult <- run (do
      AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask
      ghcVer <- case compopts ^. gitRef of
        Just ref -> pure (GHC.GitDist (GitBranch ref Nothing))
        Nothing -> do
          -- Compile the version user is pointing to in the tui
          let vi = getVersionInfo (mkTVer lVer) GHC dls
          forM_ (_viPreInstall =<< vi) $ \msg -> do
            lift $ logWarn msg
            lift $ logWarn
              "...waiting for 5 seconds, you can still abort..."
            liftIO $ threadDelay 5000000 -- give the user a sec to intervene
          forM_ (_viPreCompile =<< vi) $ \msg -> do
            logInfo msg
            logInfo
              "...waiting for 5 seconds, you can still abort..."
            liftIO $ threadDelay 5000000 -- for compilation, give the user a sec to intervene
          pure (GHC.SourceDist lVer)

      targetVer <- liftE $ GHCup.compileGHC
                    ghcVer
                    (compopts ^. crossTarget)
                    (compopts ^. overwriteVer)
                    (compopts ^. bootstrapGhc)
                    (compopts ^. hadrianGhc)
                    (compopts ^. jobs)
                    (compopts ^. buildConfig)
                    (compopts ^. patches)
                    (compopts ^. addConfArgs)
                    (compopts ^. buildFlavour)
                    (compopts ^. buildSystem)
                    (maybe GHCupInternal IsolateDir $ compopts ^. isolateDir)
                    (compopts ^. installTargets)
      AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls2 }} <- ask
      let vi2 = getVersionInfo targetVer GHC dls2
      when
        (compopts ^. setCompile)
        (liftE . void $ GHCup.setGHC targetVer SetGHCOnly Nothing)
      pure (vi2, targetVer)
      )
  case compileResult of
      VRight (vi, tv) -> do
        logInfo "GHC successfully compiled and installed"
        forM_ (_viPostInstall =<< vi) $ \msg -> logInfo msg
        liftIO $ putStr (T.unpack $ tVerToText tv)
        pure $ Right ()
      VLeft (V (AlreadyInstalled _ v)) -> do
        pure $ Left $
          "GHC ver " <> T.unpack (prettyVer v) <> " already installed, remove it first to reinstall"
      VLeft (V (DirNotEmpty fp)) -> do
        pure $ Left $
          "Install directory " <> fp <> " is not empty."
      VLeft err@(V (BuildFailed tmpdir _)) -> pure $ Left $
        case keepDirs (appstate & settings) of
          Never -> prettyHFError err
          _ -> prettyHFError err <> "\n"
            <> "Check the logs at " <> (fromGHCupPath $ appstate & dirs & logsDir)
            <> " and the build directory "
            <> tmpdir <> " for more clues." <> "\n"
            <> "Make sure to clean up " <> tmpdir <> " afterwards."
      VLeft e -> do
        pure $ Left $ prettyHFError e
-- This is the case when the tool is not GHC... which should be impossible but,
-- it exhaustes pattern matches
compileGHC _ ListResult{lTool = _} = pure (Right ())
