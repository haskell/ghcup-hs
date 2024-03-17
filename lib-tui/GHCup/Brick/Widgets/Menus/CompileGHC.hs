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
) where

import GHCup.Brick.Widgets.Menu (Menu)
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
    ( KeyCombination, BuildSystem(..), VersionPattern )
import URI.ByteString (URI)
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as UTF8
import GHCup.Utils (parseURI)
import Data.Bifunctor (Bifunctor(..))
import Data.Function ((&))
import Optics ((.~))
import Data.Char (isSpace)
import Data.Versions (Version, version)
import System.FilePath (isPathSeparator, isValid, isAbsolute, normalise)
import Control.Applicative (Alternative((<|>)))
import Text.Read (readEither)
import GHCup.Prelude (stripNewlineEnd)
import qualified GHCup.OptParse.Common as OptParse

data CompileGHCOptions = CompileGHCOptions
  { _bootstrapGhc :: Either Version FilePath
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
  } deriving (Eq, Show)

makeLenses ''CompileGHCOptions

type CompileGHCMenu = Menu CompileGHCOptions Name

create :: KeyCombination -> CompileGHCMenu
create k = Menu.createMenu CompileGHCBox initialState k buttons fields
  where
    initialState =
      CompileGHCOptions
        (Right "")
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
                  Nothing -> Left "Invalid Empty value"
                  Just f  -> Right (Right f)
           in if T.any isPathSeparator i
                then readPath
                else readVersion
        False -> Left "Invalid Empty value"

    versionV :: T.Text -> Either Menu.ErrorMessage (Maybe [VersionPattern])
    versionV = whenEmpty Nothing (bimap T.pack Just . OptParse.overWriteVersionParser . T.unpack)

    jobsV :: T.Text -> Either Menu.ErrorMessage (Maybe Int)
    jobsV =
      let parseInt = bimap (const "Invalid value. Must be an integer") Just . readEither @Int . T.unpack
       in whenEmpty Nothing parseInt

    patchesV :: T.Text -> Either Menu.ErrorMessage (Maybe (Either FilePath [URI]))
    patchesV = whenEmpty Nothing readPatches
      where
        readUri :: T.Text -> Either String URI
        readUri = first show . parseURI . UTF8.fromString . T.unpack
        readPatches j =
          let
            x = (bimap T.unpack (fmap Left) $ filepathV j)
            y = second (Just . Right) $ traverse readUri (T.split isSpace j)
          in first T.pack $ x <|> y

    filepathV :: T.Text -> Either Menu.ErrorMessage (Maybe FilePath)
    filepathV i =
      case not $ emptyEditor i of
        True  -> absolutePathParser (T.unpack i)
        False -> Right Nothing

    absolutePathParser :: FilePath -> Either Menu.ErrorMessage (Maybe FilePath)
    absolutePathParser f = case isValid f && isAbsolute f of
                  True -> Right . Just . stripNewlineEnd . normalise $ f
                  False -> Left "Please enter a valid absolute filepath."

    additionalValidator :: T.Text -> Either Menu.ErrorMessage [T.Text]
    additionalValidator = Right . T.split isSpace

    systemV :: T.Text -> Either Menu.ErrorMessage (Maybe BuildSystem)
    systemV = whenEmpty Nothing readSys
      where
        readSys i
          | T.toLower i == "hadrian" = Right $ Just Hadrian
          | T.toLower i == "make"    = Right $ Just Make
          | otherwise = Left "Not a valid Build System"

    fields =
      [ Menu.createEditableField (Common.MenuElement Common.BootstrapGhcEditBox) bootstrapV bootstrapGhc
           & Menu.fieldLabelL .~ "bootstrap-ghc"
           & Menu.fieldHelpMsgL .~ "The GHC version (or full path) to bootstrap with (must be installed)"
           & Menu.fieldStatusL .~ Menu.Invalid "Invalid Empty value"
      , Menu.createEditableField (Common.MenuElement Common.JobsEditBox) jobsV jobs
          & Menu.fieldLabelL .~ "jobs"
          & Menu.fieldHelpMsgL .~ "How many jobs to use for make"
      , Menu.createEditableField (Common.MenuElement Common.BuildConfigEditBox) filepathV buildConfig
          & Menu.fieldLabelL .~ "build config"
          & Menu.fieldHelpMsgL .~ "Absolute path to build config file"
      , Menu.createEditableField (Common.MenuElement Common.PatchesEditBox) patchesV patches
          & Menu.fieldLabelL .~ "patches"
          & Menu.fieldHelpMsgL .~ "Either a URI to a patch (https/http/file) or Absolute path to patch directory"
      , Menu.createEditableField (Common.MenuElement Common.CrossTargetEditBox) (Right . Just) crossTarget
          & Menu.fieldLabelL .~ "cross target"
          & Menu.fieldHelpMsgL .~ "Build cross-compiler for this platform"
      , Menu.createEditableField (Common.MenuElement Common.AdditionalEditBox) additionalValidator addConfArgs
          & Menu.fieldLabelL .~ "CONFIGURE_ARGS"
          & Menu.fieldHelpMsgL .~ "Additional arguments to bindist configure, prefix with '-- ' (longopts)"
      , Menu.createCheckBoxField (Common.MenuElement Common.SetCheckBox) setCompile
          & Menu.fieldLabelL .~ "set"
          & Menu.fieldHelpMsgL .~ "Set as active version after install"
      , Menu.createEditableField (Common.MenuElement Common.OvewrwiteVerEditBox) versionV overwriteVer
          & Menu.fieldLabelL .~ "overwrite-version"
          & Menu.fieldHelpMsgL .~ "Allows to overwrite the finally installed VERSION with a different one"
      , Menu.createEditableField (Common.MenuElement Common.BuildSystemEditBox) systemV buildSystem
          & Menu.fieldLabelL .~ "build system"
          & Menu.fieldHelpMsgL .~ "either 'make' or 'hadrian'"
      , Menu.createEditableField (Common.MenuElement Common.BuildFlavourEditBox) (Right . Just . T.unpack) buildFlavour
          & Menu.fieldLabelL .~ "flavour"
          & Menu.fieldHelpMsgL .~ "Set the compile build flavour (this value depends on the build system type: 'make' vs 'hadrian')"
      , Menu.createEditableField (Common.MenuElement Common.IsolateEditBox) filepathV isolateDir
          & Menu.fieldLabelL .~ "isolated"
          & Menu.fieldHelpMsgL .~ "install in an isolated absolute directory instead of the default one"
      ]

    buttons = [
       Menu.createButtonField (Common.MenuElement Common.OkButton)
           & Menu.fieldLabelL .~ "Compile"
           & Menu.fieldHelpMsgL .~ "Compile GHC from source with options below"
           & Menu.fieldStatusL .~ Menu.Invalid "bootstrap GHC is mandatory"
      ]

handler :: BrickEvent Name e -> EventM Name CompileGHCMenu ()
handler = Menu.handlerMenu


draw :: CompileGHCMenu -> Widget Name
draw = Common.frontwardLayer "Compile GHC" . Menu.drawMenu
