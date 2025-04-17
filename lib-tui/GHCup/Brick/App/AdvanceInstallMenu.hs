{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module GHCup.Brick.App.AdvanceInstallMenu where

import GHCup.List ( ListResult (..))
import GHCup.Types (GHCTargetVersion(..), KeyBindings)
import GHCup.Brick.Widgets.BaseWidget
import GHCup.Brick.Widgets.BasicOverlay
import GHCup.Brick.Widgets.InputField.Class
import GHCup.Brick.Widgets.InputField.CheckBox
import GHCup.Brick.Widgets.InputField.EditInput as EditInput
import GHCup.Brick.Widgets.GenericMenu
import qualified GHCup.Brick.Actions as Actions
import qualified GHCup.Brick.App.Common as Common
import GHCup.Brick.App.AdvanceInstallOptions
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Utils.Parsers as Utils

import Brick
    ( BrickEvent(..),
      Padding(Max),
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import           Brick.Widgets.Center ( center )
import           Control.Monad (when, forM, forM_, void)
import Data.Bifunctor (Bifunctor(..))
import Data.Char (isSpace)
import Data.List.NonEmpty             ( NonEmpty (..) )
import Data.Some
import qualified Data.Text                     as T
import GHC.Generics (Generic)
import           Prelude                 hiding ( appendFile )
import qualified Graphics.Vty as Vty
import Optics.State.Operators ((.=), (?=))
import Optics.TH (makeLenses)
import URI.ByteString (URI)

data AdvanceInstallMenuFields n = AdvanceInstallMenuFields
  { _instBindistF  :: EditInput n (Maybe URI)
  , _instSetF      :: CheckBoxInput n Bool
  , _instVersionF  :: EditInput n (Maybe GHCTargetVersion)
  , _isolateDirF   :: EditInput n (Maybe FilePath)
  , _forceInstallF :: CheckBoxInput n Bool
  , _addConfArgsF  :: EditInput n [T.Text]
  , _installTargetsF :: EditInput n T.Text
  } deriving Generic

makeLenses ''AdvanceInstallMenuFields

type AdvanceInstallMenu = GenericMenu Common.Name AdvanceInstallMenuFields ListResult InstallOptions

create :: KeyBindings -> ListResult -> AdvanceInstallMenu
create kb lr = mkGenericMenu
  Common.AdvanceInstallBox
  menuFields
  validateInputs
  lr
  (\lr opts -> void $ Actions.suspendBrickAndRunAction $ Actions.installWithOptions opts lr)
  (Common.toMenuKeyBindings kb)
  "Advance Install"
  (Button (Common.MenuElement Common.OkButton)
   "Advance Install"
   "Install with options below")
  where
    menuFields = AdvanceInstallMenuFields
      { _instBindistF = instBindistField
      , _instSetF = instSetField
      , _instVersionF = instVersionField
      , _isolateDirF = isolateDirField
      , _forceInstallF = forceInstallField
      , _addConfArgsF = addConfArgsField
      , _installTargetsF = installTargetsField
      }

    validateInputs :: AdvanceInstallMenuFields Common.Name -> Either ErrorMessage InstallOptions
    validateInputs AdvanceInstallMenuFields {..} = do
      let
        instSetVal = _checked _instSetF
        isolateDirVal = editInputValue _isolateDirF
      case (instSetVal, isolateDirVal) of
        (True, Right (Just _)) -> Left "Cannot set active when doing an isolated install"
        _ -> InstallOptions
          <$> editInputValue _instBindistF
          <*> (Right $ _checked _instSetF)
          <*> editInputValue _instVersionF
          <*> editInputValue _isolateDirF
          <*> (Right $ _checked _forceInstallF)
          <*> editInputValue _addConfArgsF
          <*> editInputValue _installTargetsF

    instBindistField = EditInput.create
      (Common.MenuElement Common.UrlEditBox)
      "url"
      "Install the specified version from this bindist"
      uriValidator
      ""

    instSetField = CheckBoxInput
      (Common.MenuElement Common.SetCheckBox)
      "set"
      "Set as active version after install"
      False

    instVersionField = EditInput.create
      (Common.MenuElement Common.ToolVersionBox)
      "version"
      "Specify a custom version"
      toolVersionValidator
      ""

    isolateDirField = EditInput.create
      (Common.MenuElement Common.IsolateEditBox)
      "isolated"
      "install in an isolated absolute directory instead of the default one"
      filepathValidator
      ""

    forceInstallField = CheckBoxInput
      (Common.MenuElement Common.ForceCheckBox)
      "force"
      "Force install (THIS IS UNSAFE, only use it in Dockerfiles or CI)"
      False

    addConfArgsField = EditInput.create
      (Common.MenuElement Common.AdditionalEditBox)
      "CONFIGURE_ARGS"
      "Additional arguments to bindist configure"
      additionalValidator
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

    uriValidator :: T.Text -> Either ErrorMessage (Maybe URI)
    uriValidator = whenEmpty Nothing (second Just . readUri)
      where readUri = first T.pack . Utils.uriParser . T.unpack

    filepathValidator :: T.Text -> Either ErrorMessage (Maybe FilePath)
    filepathValidator = whenEmpty Nothing (bimap T.pack Just . Utils.absolutePathParser . T.unpack)

    toolVersionValidator :: T.Text -> Either ErrorMessage (Maybe GHCTargetVersion)
    toolVersionValidator = whenEmpty Nothing (bimap T.pack Just . Utils.ghcVersionEither . T.unpack)

    additionalValidator :: T.Text -> Either ErrorMessage [T.Text]
    additionalValidator = Right . T.split isSpace
