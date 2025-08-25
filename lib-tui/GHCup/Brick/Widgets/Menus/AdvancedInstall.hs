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

module GHCup.Brick.Widgets.Menus.AdvancedInstall (
  InstallOptions (..),
  AdvancedInstallMenu,
  create,
  handler,
  draw,
  instBindistL,
  instSetL,
  instVersionL,
  isolateDirL,
  forceInstallL,
  addConfArgsL,
  installTargetsL,
) where

import GHCup.Types (GHCTargetVersion(..))
import GHCup.Brick.Widgets.Menu (Menu, MenuKeyBindings)
import qualified GHCup.Brick.Widgets.Menu as Menu
import           GHCup.Brick.Common(Name(..))
import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..))
import           Prelude                 hiding ( appendFile )
import           Optics.TH (makeLensesFor)
import qualified GHCup.Brick.Common as Common
import URI.ByteString (URI)
import qualified Data.Text as T
import Data.Bifunctor (Bifunctor(..))
import Data.Function ((&))
import Optics ((.~))
import Data.Char (isSpace)
import qualified GHCup.Utils.Parsers as Utils

data InstallOptions = InstallOptions
  { instBindist  :: Maybe URI
  , instSet      :: Bool
  , instVersion :: Maybe GHCTargetVersion
  -- ^ User specified version to override default
  , isolateDir   :: Maybe FilePath
  , forceInstall :: Bool
  , addConfArgs  :: [T.Text]
  , installTargets :: T.Text
  } deriving (Eq, Show)

makeLensesFor [
   ("instBindist", "instBindistL")
  , ("instSet", "instSetL")
  , ("instVersion", "instVersionL")
  , ("isolateDir", "isolateDirL")
  , ("forceInstall", "forceInstallL")
  , ("addConfArgs", "addConfArgsL")
  , ("installTargets", "installTargetsL")
  ]
  ''InstallOptions

type AdvancedInstallMenu = Menu InstallOptions Name

create :: MenuKeyBindings -> AdvancedInstallMenu
create k = Menu.createMenu AdvancedInstallBox initialState "Advanced Install" validator k [ok] fields
  where
    initialInstallTargets = "install"
    initialState = InstallOptions Nothing False Nothing Nothing False [] initialInstallTargets
    validator InstallOptions {..} = case (instSet, isolateDir) of
      (True, Just _) -> Just "Cannot set active when doing an isolated install"
      _ -> Nothing
    -- Brick's internal editor representation is [mempty].
    emptyEditor i = T.null i || (i == "\n")

    whenEmpty :: a -> (T.Text -> Either Menu.ErrorMessage a) -> T.Text -> Either Menu.ErrorMessage a
    whenEmpty emptyval f i = if not (emptyEditor i) then f i else Right emptyval

    uriValidator :: T.Text -> Either Menu.ErrorMessage (Maybe URI)
    uriValidator = whenEmpty Nothing (second Just . readUri)
      where readUri = first T.pack . Utils.uriParser . T.unpack

    filepathValidator :: T.Text -> Either Menu.ErrorMessage (Maybe FilePath)
    filepathValidator = whenEmpty Nothing (bimap T.pack Just . Utils.absolutePathParser . T.unpack)

    toolVersionValidator :: T.Text -> Either Menu.ErrorMessage (Maybe GHCTargetVersion)
    toolVersionValidator = whenEmpty Nothing (bimap T.pack Just . Utils.ghcVersionEither . T.unpack)

    additionalValidator :: T.Text -> Either Menu.ErrorMessage [T.Text]
    additionalValidator = Right . T.split isSpace

    fields =
      [ Menu.createEditableField (Common.MenuElement Common.UrlEditBox) uriValidator instBindistL
          & Menu.fieldLabelL .~ "url"
          & Menu.fieldHelpMsgL .~ "Install the specified version from this bindist"
      , Menu.createCheckBoxField (Common.MenuElement Common.SetCheckBox) instSetL
          & Menu.fieldLabelL .~ "set"
          & Menu.fieldHelpMsgL .~ "Set as active version after install"
      , Menu.createEditableField (Common.MenuElement Common.ToolVersionBox) toolVersionValidator instVersionL
          & Menu.fieldLabelL .~ "version"
          & Menu.fieldHelpMsgL .~ "Specify a custom version"
      , Menu.createEditableField' initialInstallTargets (Common.MenuElement Common.GHCInstallTargets) Right installTargetsL
          & Menu.fieldLabelL .~ "install-targets"
          & Menu.fieldHelpMsgL .~ "Specify space separated list of make install targets"
      , Menu.createEditableField (Common.MenuElement Common.IsolateEditBox) filepathValidator isolateDirL
          & Menu.fieldLabelL .~ "isolated"
          & Menu.fieldHelpMsgL .~ "install in an isolated absolute directory instead of the default one"
      , Menu.createCheckBoxField (Common.MenuElement Common.ForceCheckBox) forceInstallL
          & Menu.fieldLabelL .~ "force"
          & Menu.fieldHelpMsgL .~ "Force install (THIS IS UNSAFE, only use it in Dockerfiles or CI)"
      , Menu.createEditableField (Common.MenuElement Common.AdditionalEditBox) additionalValidator addConfArgsL
          & Menu.fieldLabelL .~ "CONFIGURE_ARGS"
          & Menu.fieldHelpMsgL .~ "Additional arguments to bindist configure"
      ]

    ok = Menu.createButtonField (Common.MenuElement Common.OkButton)
          & Menu.fieldLabelL .~ "Advanced Install"
          & Menu.fieldHelpMsgL .~ "Install with options below"

handler :: BrickEvent Name e -> EventM Name AdvancedInstallMenu ()
handler = Menu.handlerMenu


draw :: AdvancedInstallMenu -> [Widget Name]
draw = Menu.drawMenu
