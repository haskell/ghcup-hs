{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module GHCup.Brick.Widgets.Menus.CompileGHC (
  CompileGHCOptions,
  CompileGHCMenu,
  create,
  handler,
  draw,
  bootstrapGhc,
  hadrianGhc,
  jobs,
  buildConfig,
  patches,
  crossTarget,
  addConfArgs,
  setCompile,
  overwriteVer,
  buildFlavour,
  buildSystem,
  isolateDir,
  gitRef,
  installTargets,
) where

import GHCup.Brick.Widgets.Menu (Menu, MenuKeyBindings)
import qualified GHCup.Brick.Widgets.Menu as Menu
import           GHCup.Brick.Common(Name(..))
import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..))
import           Prelude                 hiding ( appendFile )
import           Optics.TH (makeLenses)
import qualified GHCup.Brick.Common as Common
import GHCup.Types
    ( BuildSystem(..), VersionPattern )
import URI.ByteString (URI)
import Control.Monad (join)
import qualified Data.Text as T
import Data.Bifunctor (Bifunctor(..))
import Data.Function ((&))
import Optics ((.~), iso, (%))
import Data.Char (isSpace)
import Data.List.NonEmpty             ( NonEmpty (..) )
import qualified Data.List.NonEmpty            as NE
import Data.Versions (Version, version)
import System.FilePath (isPathSeparator)
import Control.Applicative (Alternative((<|>)))
import Text.Read (readEither)
import qualified GHCup.Utils.Parsers as Utils
import           Text.PrettyPrint.HughesPJClass ( prettyShow )

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

makeLenses ''CompileGHCOptions

type CompileGHCMenu = Menu CompileGHCOptions Name

create :: MenuKeyBindings -> [Version] -> CompileGHCMenu
create k availableGHCs = Menu.createMenu CompileGHCBox initialState "Compile GHC" validator k buttons fields
  where
    initialInstallTargets = "install"
    initialState =
      CompileGHCOptions
        (Right "")
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        []
        False
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        initialInstallTargets
    validator CompileGHCOptions {..} = case (_setCompile, _isolateDir) of
      (True, Just _) -> Just "Cannot set active when doing an isolated install"
      _ -> case (_buildConfig, _buildSystem) of
        (Just _, Just Hadrian) -> Just "Build config can be specified only for make build system"
        _ -> Nothing
    -- Brick's internal editor representation is [mempty].
    emptyEditor i = T.null i || (i == "\n")
    whenEmpty :: a -> (T.Text -> Either Menu.ErrorMessage a) -> T.Text -> Either Menu.ErrorMessage a
    whenEmpty emptyval f i = if not (emptyEditor i) then f i else Right emptyval

    bootstrapV :: T.Text -> Either Menu.ErrorMessage (Either Version FilePath)
    bootstrapV i =
      case not $ emptyEditor i of
        True  ->
          let readVersion = bimap (const "Not a valid version") Left (version i)
              readPath = do
                mfilepath <- filepathV i
                case mfilepath of
                  Nothing -> Left "Invalid path"
                  Just f  -> Right (Right f)
           in if T.any isPathSeparator i
                then readPath
                else readVersion
        False -> Left "No version selected / no path specified"

    hadrianstrapV :: T.Text -> Either Menu.ErrorMessage (Maybe (Either Version FilePath))
    hadrianstrapV i' =
        let readVersion = bimap (const "Not a valid version") (Just . Left) . version
            readPath = bimap T.pack (Just . Right) . Utils.absolutePathParser . T.unpack
         in if T.any isPathSeparator i'
              then whenEmpty Nothing readPath i'
              else whenEmpty Nothing readVersion i'

    versionV :: T.Text -> Either Menu.ErrorMessage (Maybe [VersionPattern])
    versionV = whenEmpty Nothing (bimap T.pack Just . Utils.overWriteVersionParser . T.unpack)

    jobsV :: T.Text -> Either Menu.ErrorMessage (Maybe Int)
    jobsV =
      let parseInt = bimap (const "Invalid value. Must be an integer") Just . readEither @Int . T.unpack
       in whenEmpty Nothing parseInt

    patchesV :: T.Text -> Either Menu.ErrorMessage (Maybe (Either FilePath [URI]))
    patchesV = whenEmpty Nothing readPatches
      where
        readPatches j =
          let
            x = second (Just . Left) $ Utils.absolutePathParser (T.unpack j)
            y = second (Just . Right) $ traverse (Utils.uriParser . T.unpack) (T.split isSpace j)
          in first T.pack $ x <|> y

    filepathV :: T.Text -> Either Menu.ErrorMessage (Maybe FilePath)
    filepathV = whenEmpty Nothing (bimap T.pack Just . Utils.absolutePathParser . T.unpack)

    additionalValidator :: T.Text -> Either Menu.ErrorMessage [T.Text]
    additionalValidator = Right . T.split isSpace

    showMaybeBuildSystem :: Maybe BuildSystem -> T.Text
    showMaybeBuildSystem  = \case
      Nothing -> "Auto select (prefer hadrian if available, and build config is not specified)"
      Just Hadrian -> "hadrian"
      Just Make -> "make"

    bootstrapGHCFields = case NE.nonEmpty availableGHCs of
        Just ne ->
          let bootstrapGhc' = bootstrapGhc % (iso (either (Left . Left) (Left . Right)) (either id Left))
          in [ Menu.createSelectFieldWithEditable (Common.MenuElement Common.BootstrapGhcSelectBox) (Common.MenuElement Common.BootstrapGhcEditBox) bootstrapGhc' bootstrapV ne (T.pack . prettyShow) k
               & Menu.fieldLabelL .~ "bootstrap-ghc"
               & Menu.fieldHelpMsgL .~ "The GHC version (or full path) to bootstrap with (must be installed)"
               & Menu.fieldStatusL .~ Menu.Invalid "No version selected / no path specified"
             ]
        _ -> [ Menu.createEditableField (Common.MenuElement Common.BootstrapGhcEditBox) bootstrapV bootstrapGhc
               & Menu.fieldLabelL .~ "bootstrap-ghc"
               & Menu.fieldHelpMsgL .~ "The GHC version (or full path) to bootstrap with (must be installed)"
               & Menu.fieldStatusL .~ Menu.Invalid "Invalid empty value"
             ]

    hadrianGHCFields = case NE.nonEmpty availableGHCs of
        Just ne ->
          let hadrianGhc' = hadrianGhc % (iso Left (either id (Just . Left)))
          in [ Menu.createSelectFieldWithEditable (Common.MenuElement Common.HadrianGhcSelectBox) (Common.MenuElement Common.HadrianGhcEditBox) hadrianGhc' hadrianstrapV ne (T.pack . prettyShow) k
               & Menu.fieldLabelL .~ "hadrian-ghc"
               & Menu.fieldHelpMsgL .~ "The GHC version (or full path) that will be used to compile hadrian (must be installed)"
             ]
        _ -> [ Menu.createEditableField (Common.MenuElement Common.HadrianGhcEditBox) hadrianstrapV hadrianGhc
               & Menu.fieldLabelL .~ "hadrian-ghc"
               & Menu.fieldHelpMsgL .~ "The GHC version (or full path) that will be used to compile hadrian (must be installed)"
             ]

    fields = bootstrapGHCFields ++ hadrianGHCFields ++
      [ Menu.createEditableField (Common.MenuElement Common.JobsEditBox) jobsV jobs
          & Menu.fieldLabelL .~ "jobs"
          & Menu.fieldHelpMsgL .~ "How many jobs to use for make"
      , Menu.createCheckBoxField (Common.MenuElement Common.SetCheckBox) setCompile
          & Menu.fieldLabelL .~ "set"
          & Menu.fieldHelpMsgL .~ "Set as active version after install"
      , Menu.createEditableField (Common.MenuElement Common.BuildFlavourEditBox) (Right . Just . T.unpack) buildFlavour
          & Menu.fieldLabelL .~ "flavour"
          & Menu.fieldHelpMsgL .~ "Set the compile build flavour (this value depends on the build system type: 'make' vs 'hadrian')"
      , Menu.createEditableField (Common.MenuElement Common.AdditionalEditBox) additionalValidator addConfArgs
          & Menu.fieldLabelL .~ "CONFIGURE_ARGS"
          & Menu.fieldHelpMsgL .~ "Additional arguments to compile configure"
      , Menu.createEditableField (Common.MenuElement Common.BuildConfigEditBox) filepathV buildConfig
          & Menu.fieldLabelL .~ "build config"
          & Menu.fieldHelpMsgL .~ "Absolute path to build config file (make build system only)"
      , Menu.createEditableField (Common.MenuElement Common.PatchesEditBox) patchesV patches
          & Menu.fieldLabelL .~ "patches"
          & Menu.fieldHelpMsgL .~ "Either a URI to a patch (https/http/file) or Absolute path to patch directory"
      , Menu.createEditableField (Common.MenuElement Common.CrossTargetEditBox) (Right . Just) crossTarget
          & Menu.fieldLabelL .~ "cross target"
          & Menu.fieldHelpMsgL .~ "Build cross-compiler for this platform"
      , Menu.createSelectField (Common.MenuElement Common.BuildSystemEditBox) (buildSystem % (iso Just join)) (Nothing :| [Just Hadrian, Just Make]) showMaybeBuildSystem  k
          & Menu.fieldLabelL .~ "build system"
          & Menu.fieldHelpMsgL .~ "Select the build system"
      , Menu.createEditableField (Common.MenuElement Common.OvewrwiteVerEditBox) versionV overwriteVer
          & Menu.fieldLabelL .~ "overwrite-version"
          & Menu.fieldHelpMsgL .~ "Allows to overwrite the finally installed VERSION with a different one. Allows to specify patterns: %v (version), %b (branch name), %h (short commit hash), %H (long commit hash), %g ('git describe' output)"
      , Menu.createEditableField (Common.MenuElement Common.IsolateEditBox) filepathV isolateDir
          & Menu.fieldLabelL .~ "isolated"
          & Menu.fieldHelpMsgL .~ "install in an isolated absolute directory instead of the default one"
      , Menu.createEditableField (Common.MenuElement Common.GitRefEditBox) (Right . Just . T.unpack) gitRef
          & Menu.fieldLabelL .~ "git-ref"
          & Menu.fieldHelpMsgL .~ "The git commit/branch/ref to build from"
      , Menu.createEditableField' initialInstallTargets (Common.MenuElement Common.GHCInstallTargets) Right installTargets
          & Menu.fieldLabelL .~ "install-targets"
          & Menu.fieldHelpMsgL .~ "Specify space separated list of make install targets"
      ]

    buttons = [
       Menu.createButtonField (Common.MenuElement Common.OkButton)
           & Menu.fieldLabelL .~ "Compile"
           & Menu.fieldHelpMsgL .~ "Compile GHC from source with options below\nRequired fields: bootstrap-ghc"
           & Menu.fieldStatusL .~ Menu.Invalid "bootstrap GHC is mandatory"
      ]

handler :: BrickEvent Name e -> EventM Name CompileGHCMenu ()
handler = Menu.handlerMenu


draw :: CompileGHCMenu -> [Widget Name]
draw = Menu.drawMenu
