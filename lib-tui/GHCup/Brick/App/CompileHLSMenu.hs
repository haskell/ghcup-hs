{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}

module GHCup.Brick.App.CompileHLSMenu where

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
import qualified GHCup.Brick.Actions as Actions
import qualified GHCup.Brick.App.Common as Common
import qualified GHCup.Brick.Common as Common
import           GHCup.Prelude.Logger
import qualified GHCup.HLS as HLS

import Brick
    ( BrickEvent(..),
      Padding(Max),
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import Control.Applicative (Alternative((<|>)))
import Control.Concurrent (threadDelay)
import           Control.Exception.Safe
import           Control.Monad (when, forM, forM_, void)
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource
import Data.Bifunctor (Bifunctor(..))
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Some
import qualified Data.Text as T
import           Data.Variant.Excepts
import Data.Versions (Version, prettyVer)
import GHC.Generics (Generic)
import Prelude hiding (appendFile)
import qualified Graphics.Vty as Vty
import Optics ((^.), (&), to, (%), (%~))
import Optics.State.Operators ((.=), (?=))
import Optics.TH (makeLenses)
import Text.Read (readEither)
import Text.PrettyPrint.HughesPJClass (prettyShow)
import URI.ByteString (URI)

data CompileHLSMenuFields n = CompileHLSMenuFields
  { _targetGHCsF :: SelectInput n ToolVersion [ToolVersion]
  , _updateCabalF :: CheckBoxInput n Bool
  , _jobsF :: EditInput n (Maybe Int)
  , _setCompileF :: CheckBoxInput n Bool
  , _cabalArgsF :: EditInput n [T.Text]
  , _isolateDirF :: EditInput n (Maybe FilePath)
  , _overwriteVerF :: EditInput n (Maybe [VersionPattern])
  , _patchesF :: EditInput n (Maybe (Either FilePath [URI]))
  , _cabalProjectF :: EditInput n (Maybe (Either FilePath URI))
  , _cabalProjectLocalF :: EditInput n (Maybe URI)
  , _gitRefF :: EditInput n (Maybe String)
  } deriving Generic

data CompileHLSOptions = CompileHLSOptions
  { _jobs :: Maybe Int
  , _setCompile :: Bool
  , _updateCabal :: Bool
  , _overwriteVer :: Maybe [VersionPattern]
  , _isolateDir :: Maybe FilePath
  , _cabalProject :: Maybe (Either FilePath URI)
  , _cabalProjectLocal :: Maybe URI
  , _patches :: Maybe (Either FilePath [URI])
  , _targetGHCs :: [ToolVersion]
  , _cabalArgs :: [T.Text]
  , _gitRef :: Maybe String
  } deriving (Eq, Show)

concat <$> mapM makeLenses [''CompileHLSMenuFields, ''CompileHLSOptions]

type CompileHLSMenu = GenericMenu Common.Name CompileHLSMenuFields (AppState, ListResult) CompileHLSOptions

create :: KeyBindings -> ListResult -> AppState -> [Version] -> CompileHLSMenu
create kb lr s availableGHCs = mkGenericMenu
  Common.CompileGHCBox
  menuFields
  validateInputs
  (s, lr)
  (\(s, lr) opts -> void $ Actions.suspendBrickAndRunAction s $ compileHLS opts lr)
  (Common.toMenuKeyBindings kb)
  "Compile HLS"
  (Button (Common.MenuElement Common.OkButton)
   "Compile"
   "Compile HLS from source with options below\nRequired fields: target GHC(s)")
  where
    menuFields = CompileHLSMenuFields
      { _targetGHCsF = targetGHCsField
      , _updateCabalF = updateCabalField
      , _jobsF = jobsField
      , _setCompileF = setCompileField
      , _overwriteVerF = overwriteVerField
      , _isolateDirF = isolateDirField
      , _cabalProjectF = cabalProjectField
      , _cabalProjectLocalF = cabalProjectLocalField
      , _patchesF = patchesField
      , _cabalArgsF = cabalArgsField
      , _gitRefF = gitRefField
      }

    validateInputs :: CompileHLSMenuFields Common.Name -> Either ErrorMessage CompileHLSOptions
    validateInputs CompileHLSMenuFields {..} = do
      let setCompileVal = _checked _setCompileF
          isolateDirVal = editInputValue _isolateDirF
          targetGHCs = case getSelection _targetGHCsF of
            (vs, Just (Right (xs, _))) -> Right $ vs ++ xs
            (_, Just (Left msg)) -> Left msg
            (vs, Nothing) -> Right $ vs
            ([], _) -> Left "target GHC(s): No version selected"

      case (setCompileVal, isolateDirVal) of
        (True, Right (Just _)) -> Left "Cannot set active when doing an isolated install"
        _ -> CompileHLSOptions
          <$> editInputValue _jobsF
          <*> Right setCompileVal
          <*> Right (_checked _updateCabalF)
          <*> editInputValue _overwriteVerF
          <*> isolateDirVal
          <*> editInputValue _cabalProjectF
          <*> editInputValue _cabalProjectLocalF
          <*> editInputValue _patchesF
          <*> targetGHCs
          <*> editInputValue _cabalArgsF
          <*> editInputValue _gitRefF

    targetGHCsField = SelectInput.createMultiSelectInputWithEditable
      (Common.MenuElement Common.BootstrapGhcSelectBox)
      (Common.MenuElement Common.TargetGhcEditBox)
      "target GHC(s)"
      "GHC versions to compile for (Press Enter to edit)"
      (fmap ToolVersion availableGHCs)
      (T.pack . prettyShow)
      ghcVersionTagEither
      (Common.toMenuKeyBindings kb)

    cabalArgsField = EditInput.create
      (Common.MenuElement Common.AdditionalEditBox)
      "CABAL_ARGS"
      "Additional arguments to cabal install"
      additionalValidator
      ""

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

    updateCabalField = CheckBoxInput
      (Common.MenuElement Common.UpdateCabalCheckBox)
      "cabal update"
      "Run 'cabal update' before the build"
      False

    overwriteVerField = EditInput.create
      (Common.MenuElement Common.OvewrwiteVerEditBox)
      "overwrite version"
      "Allows to overwrite the finally installed VERSION with a different one. Allows to specify patterns: %v (version), %b (branch name), %h (short commit hash), %H (long commit hash), %g ('git describe' output)"
      overWriteVersionParser
      ""

    isolateDirField = EditInput.create
      (Common.MenuElement Common.IsolateEditBox)
      "isolated"
      "install in an isolated absolute directory instead of the default one"
      filepathV
      ""

    cabalProjectField = EditInput.create
      (Common.MenuElement Common.CabalProjectEditBox)
      "cabal project"
      "If relative filepath, specifies the path to cabal.project inside the unpacked HLS tarball/checkout. Otherwise expects a full URI with https/http/file scheme."
      cabalProjectV
      ""

    cabalProjectLocalField = EditInput.create
      (Common.MenuElement Common.CabalProjectLocalEditBox)
      "cabal project local"
      "URI (https/http/file) to a cabal.project.local to be used for the build. Will be copied over."
      cabalProjectLocalV
      ""

    patchesField = EditInput.create
      (Common.MenuElement Common.PatchesEditBox)
      "patches"
      "Either a URI to a patch (https/http/file) or Absolute path to patch directory"
      patchesV
      ""

    gitRefField = EditInput.create
      (Common.MenuElement Common.GitRefEditBox)
      "git-ref"
      "The git commit/branch/ref to build from"
      (whenEmpty Nothing (Right . Just . T.unpack))
      ""

    -- Brick's internal editor representation is [mempty].
    emptyEditor i = T.null i || (i == "\n")
    whenEmpty :: a -> (T.Text -> Either ErrorMessage a) -> T.Text -> Either ErrorMessage a
    whenEmpty emptyval f i = if not (emptyEditor i) then f i else Right emptyval

    readUri :: T.Text -> Either ErrorMessage URI
    readUri = first T.pack . Utils.uriParser . T.unpack

    cabalProjectV :: T.Text -> Either ErrorMessage (Maybe (Either FilePath URI))
    cabalProjectV = whenEmpty Nothing parseFileOrUri
      where
        parseFileOrUri i =
          let x = bimap T.unpack Right (readUri i)
              y = Right . Left . T.unpack $ i
           in bimap T.pack Just $ x <|> y

    cabalProjectLocalV :: T.Text -> Either ErrorMessage (Maybe URI)
    cabalProjectLocalV = whenEmpty Nothing (second Just . readUri)

    ghcVersionTagEither :: T.Text -> Either ErrorMessage [ToolVersion]
    ghcVersionTagEither = whenEmpty [] $ first T.pack . traverse (Utils.ghcVersionTagEither . T.unpack) . T.split isSpace

    additionalValidator :: T.Text -> Either ErrorMessage [T.Text]
    additionalValidator = whenEmpty [] $ Right . T.split isSpace

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

    filepathV :: T.Text -> Either ErrorMessage (Maybe FilePath)
    filepathV = whenEmpty Nothing (bimap T.pack Just . Utils.absolutePathParser . T.unpack)

    overWriteVersionParser :: T.Text -> Either ErrorMessage (Maybe [VersionPattern])
    overWriteVersionParser = whenEmpty Nothing $ bimap T.pack Just . Utils.overWriteVersionParser . T.unpack

updateAvailableGHCs :: [Version] -> CompileHLSMenu -> CompileHLSMenu
updateAvailableGHCs availableGHCs v = v
  & fields % targetGHCsF %~ SelectInput.updateItems (fmap ToolVersion availableGHCs)

compileHLS :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m, Alternative m)
           => CompileHLSOptions -> ListResult -> m (Either String ())
compileHLS compopts lr@ListResult{lTool = HLS, ..} = do
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
                  , TagNotFound
                  , DayNotFound
                  , NextVerNotFound
                  , NoToolVersionSet
                  , NotInstalled
                  , DirNotEmpty
                  , ArchiveResult
                  , UninstallFailed
                  , MergeFileTreeError
                  , URIParseError
                  ]
  compileResult <- run (do
      AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask
      hlsVer <- case compopts ^. gitRef of
        Just ref -> pure (HLS.GitDist (GitBranch ref Nothing))
        Nothing -> do
          -- Compile the version user is pointing to in the tui
          let vi = getVersionInfo (mkTVer lVer) HLS dls
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
          pure (HLS.SourceDist lVer)

      ghcs <-
        liftE $ forM (compopts ^. targetGHCs)
                     (\ghc -> fmap (_tvVersion . fst) . Utils.fromVersion (Just ghc) GStrict $ GHC)
      targetVer <- liftE $ GHCup.compileHLS
                      hlsVer
                      ghcs
                      (compopts ^. jobs)
                      (compopts ^. overwriteVer)
                      (maybe GHCupInternal IsolateDir $ compopts ^. isolateDir)
                      (compopts ^. cabalProject)
                      (compopts ^. cabalProjectLocal)
                      (compopts ^. updateCabal)
                      (compopts ^. patches)
                      (compopts ^. cabalArgs)
      AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls2 }} <- ask
      let vi2 = getVersionInfo (mkTVer targetVer) GHC dls2
      when
        (compopts ^. setCompile)
        (liftE . void $ GHCup.setHLS targetVer SetHLSOnly Nothing)
      pure (vi2, targetVer)
      )
  case compileResult of
      VRight (vi, tv) -> do
        logInfo "HLS successfully compiled and installed"
        forM_ (_viPostInstall =<< vi) $ \msg -> logInfo msg
        liftIO $ putStr (T.unpack $ prettyVer tv)
        pure $ Right ()
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
-- This is the case when the tool is not HLS... which should be impossible but,
-- it exhaustes pattern matches
compileHLS _ ListResult{lTool = _} = pure (Right ())
