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

module GHCup.Brick.Widgets.Menus.CompileHLS (
  CompileHLSOptions,
  CompileHLSMenu,
  create,
  handler,
  draw,
  jobs,
  setCompile,
  updateCabal,
  overwriteVer,
  isolateDir,
  cabalProject,
  cabalProjectLocal,
  patches,
  targetGHCs,
  cabalArgs,
)
where

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
import GHCup.Types (KeyCombination, VersionPattern, ToolVersion)
import URI.ByteString (URI)
import qualified Data.Text as T
import Data.Bifunctor (Bifunctor(..))
import Data.Function ((&))
import Optics ((.~))
import Data.Char (isSpace)
import Control.Applicative (Alternative((<|>)))
import Text.Read (readEither)
import qualified GHCup.Utils.Parsers as Utils

data CompileHLSOptions = CompileHLSOptions
  { _jobs         :: Maybe Int
  , _setCompile   :: Bool
  , _updateCabal  :: Bool
  , _overwriteVer :: Maybe [VersionPattern]
  , _isolateDir   :: Maybe FilePath
  , _cabalProject :: Maybe (Either FilePath URI)
  , _cabalProjectLocal :: Maybe URI
  , _patches      :: Maybe (Either FilePath [URI])
  , _targetGHCs   :: [ToolVersion]
  , _cabalArgs    :: [T.Text]
  } deriving (Eq, Show)

makeLenses ''CompileHLSOptions

type CompileHLSMenu = Menu CompileHLSOptions Name

create :: KeyCombination -> CompileHLSMenu
create k = Menu.createMenu CompileGHCBox initialState k buttons fields
  where
    initialState =
      CompileHLSOptions
        Nothing
        False
        False
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        []
        []
    -- Brick's internal editor representation is [mempty].
    emptyEditor i = T.null i || (i == "\n")
    whenEmpty :: a -> (T.Text -> Either Menu.ErrorMessage a) -> T.Text -> Either Menu.ErrorMessage a
    whenEmpty emptyval f i = if not (emptyEditor i) then f i else Right emptyval

    readUri :: T.Text -> Either Menu.ErrorMessage URI
    readUri = first T.pack . Utils.uriParser . T.unpack

    cabalProjectV :: T.Text -> Either Menu.ErrorMessage (Maybe (Either FilePath URI))
    cabalProjectV = whenEmpty Nothing parseFileOrUri
      where
        parseFileOrUri i =
          let x = bimap T.unpack Right (readUri i)
              y = Right . Left . T.unpack $ i
           in bimap T.pack Just $ x <|> y

    cabalProjectLocalV :: T.Text -> Either Menu.ErrorMessage (Maybe URI)
    cabalProjectLocalV = whenEmpty Nothing (second Just . readUri)

    ghcVersionTagEither :: T.Text -> Either Menu.ErrorMessage [ToolVersion]
    ghcVersionTagEither = whenEmpty [] $ first T.pack . traverse (Utils.ghcVersionTagEither . T.unpack) . T.split isSpace

    overWriteVersionParser :: T.Text -> Either Menu.ErrorMessage (Maybe [VersionPattern])
    overWriteVersionParser = whenEmpty Nothing $ bimap T.pack Just . Utils.overWriteVersionParser . T.unpack

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
    filepathV = whenEmpty Nothing (bimap T.pack Just . Utils.isolateParser . T.unpack)

    additionalValidator :: T.Text -> Either Menu.ErrorMessage [T.Text]
    additionalValidator = Right . T.split isSpace

    fields =
      [ Menu.createEditableField (Common.MenuElement Common.CabalProjectEditBox) cabalProjectV cabalProject
           & Menu.fieldLabelL .~ "cabal project"
           & Menu.fieldHelpMsgL .~ "If relative filepath, specifies the path to cabal.project inside the unpacked HLS tarball/checkout. Otherwise expects a full URI with https/http/file scheme."
      , Menu.createEditableField (Common.MenuElement Common.CabalProjectLocalEditBox) cabalProjectLocalV cabalProjectLocal
          & Menu.fieldLabelL .~ "cabal project local"
          & Menu.fieldHelpMsgL .~ "URI (https/http/file) to a cabal.project.local to be used for the build. Will be copied over."
      , Menu.createCheckBoxField (Common.MenuElement Common.UpdateCabalCheckBox) updateCabal
          & Menu.fieldLabelL .~ "cabal update"
          & Menu.fieldHelpMsgL .~ "Run 'cabal update' before the build"
      , Menu.createEditableField (Common.MenuElement Common.JobsEditBox) jobsV jobs
          & Menu.fieldLabelL .~ "jobs"
          & Menu.fieldHelpMsgL .~ "How many jobs to use for make"
      , Menu.createEditableField (Common.MenuElement Common.TargetGhcEditBox) ghcVersionTagEither targetGHCs
          & Menu.fieldLabelL .~ "target GHC"
          & Menu.fieldHelpMsgL .~ "For which GHC version to compile for (can be specified multiple times)"
      , Menu.createEditableField (Common.MenuElement Common.PatchesEditBox) patchesV patches
          & Menu.fieldLabelL .~ "patches"
          & Menu.fieldHelpMsgL .~ "Either a URI to a patch (https/http/file) or Absolute path to patch directory"
      , Menu.createCheckBoxField (Common.MenuElement Common.SetCheckBox) setCompile
          & Menu.fieldLabelL .~ "set"
          & Menu.fieldHelpMsgL .~ "Set as active version after install"
      , Menu.createEditableField (Common.MenuElement Common.AdditionalEditBox) additionalValidator cabalArgs
          & Menu.fieldLabelL .~ "CABAL_ARGS"
          & Menu.fieldHelpMsgL .~ "Additional arguments to cabal install"
      , Menu.createEditableField (Common.MenuElement Common.IsolateEditBox) filepathV isolateDir
          & Menu.fieldLabelL .~ "isolated"
          & Menu.fieldHelpMsgL .~ "install in an isolated absolute directory instead of the default one"
      , Menu.createEditableField (Common.MenuElement Common.OvewrwiteVerEditBox) overWriteVersionParser overwriteVer
          & Menu.fieldLabelL .~ "overwrite version"
          & Menu.fieldHelpMsgL .~ "Allows to overwrite the finally installed VERSION with a different one"
      ]

    buttons = [
       Menu.createButtonField (Common.MenuElement Common.OkButton)
           & Menu.fieldLabelL .~ "Compile"
           & Menu.fieldHelpMsgL .~ "Compile HLS from source with options below"
      ]

handler :: BrickEvent Name e -> EventM Name CompileHLSMenu ()
handler = Menu.handlerMenu


draw :: CompileHLSMenu -> Widget Name
draw = Common.frontwardLayer "Compile HLS" . Menu.drawMenu
