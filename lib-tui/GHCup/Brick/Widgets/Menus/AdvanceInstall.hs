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

module GHCup.Brick.Widgets.Menus.AdvanceInstall (
  InstallOptions (..),
  AdvanceInstallMenu,
  create,
  handler,
  draw,
  instBindistL,
  instSetL,
  isolateDirL,
  forceInstallL,
  addConfArgsL,
) where

import GHCup.Brick.Widgets.Menu (Menu)
import qualified GHCup.Brick.Widgets.Menu as Menu
import           GHCup.Brick.Common(Name(..))
import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..))
import           Prelude                 hiding ( appendFile )
import           Optics.TH (makeLensesFor)
import qualified GHCup.Brick.Common as Common
import GHCup.Types (KeyCombination)
import URI.ByteString (URI)
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as UTF8
import GHCup.Utils (parseURI)
import Data.Bifunctor (Bifunctor(..))
import Data.Function ((&))
import Optics ((.~))
import Data.Char (isSpace)
import System.FilePath (isValid, isAbsolute, normalise)
import GHCup.Prelude (stripNewlineEnd)

data InstallOptions = InstallOptions
  { instBindist  :: Maybe URI
  , instSet      :: Bool
  , isolateDir   :: Maybe FilePath
  , forceInstall :: Bool
  , addConfArgs  :: [T.Text]
  } deriving (Eq, Show)

makeLensesFor [
   ("instBindist", "instBindistL")
  , ("instSet", "instSetL")
  , ("isolateDir", "isolateDirL")
  , ("forceInstall", "forceInstallL")
  , ("addConfArgs", "addConfArgsL")
  ]
  ''InstallOptions

type AdvanceInstallMenu = Menu InstallOptions Name

create :: KeyCombination -> AdvanceInstallMenu
create k = Menu.createMenu AdvanceInstallBox initialState k [ok] fields
  where
    initialState = InstallOptions Nothing False Nothing False []
    -- Brick's internal editor representation is [mempty].
    emptyEditor i = T.null i || (i == "\n")

    uriValidator :: T.Text -> Either Menu.ErrorMessage (Maybe URI)
    uriValidator i =
      case not $ emptyEditor i of
        True  -> bimap (T.pack . show) Just . parseURI . UTF8.fromString . T.unpack $ i
        False -> Right Nothing

    filepathValidator :: T.Text -> Either Menu.ErrorMessage (Maybe FilePath)
    filepathValidator i =
      case not $ emptyEditor i of
        True  -> absolutePathParser (T.unpack i)
        False -> Right Nothing

    absolutePathParser :: FilePath -> Either Menu.ErrorMessage (Maybe FilePath)
    absolutePathParser f = case isValid f && isAbsolute f of
                  True -> Right . Just . stripNewlineEnd . normalise $ f
                  False -> Left "Please enter a valid absolute filepath."

    additionalValidator :: T.Text -> Either Menu.ErrorMessage [T.Text]
    additionalValidator = Right . T.split isSpace

    fields =
      [ Menu.createEditableField (Common.MenuElement Common.UrlEditBox) uriValidator instBindistL
          & Menu.fieldLabelL .~ "url"
          & Menu.fieldHelpMsgL .~ "Install the specified version from this bindist"
      , Menu.createCheckBoxField (Common.MenuElement Common.SetCheckBox) instSetL
          & Menu.fieldLabelL .~ "set"
          & Menu.fieldHelpMsgL .~ "Set as active version after install"
      , Menu.createEditableField (Common.MenuElement Common.IsolateEditBox) filepathValidator isolateDirL
          & Menu.fieldLabelL .~ "isolated"
          & Menu.fieldHelpMsgL .~ "install in an isolated absolute directory instead of the default one"
      , Menu.createCheckBoxField (Common.MenuElement Common.ForceCheckBox) forceInstallL
          & Menu.fieldLabelL .~ "force"
          & Menu.fieldHelpMsgL .~ "Force install (THIS IS UNSAFE, only use it in Dockerfiles or CI)"
      , Menu.createEditableField (Common.MenuElement Common.AdditionalEditBox) additionalValidator addConfArgsL
          & Menu.fieldLabelL .~ "CONFIGURE_ARGS"
          & Menu.fieldHelpMsgL .~ "Additional arguments to bindist configure, prefix with '-- ' (longopts)"
      ]

    ok = Menu.createButtonField (Common.MenuElement Common.OkButton)
          & Menu.fieldLabelL .~ "Advance Install"
          & Menu.fieldHelpMsgL .~ "Install with options below"

handler :: BrickEvent Name e -> EventM Name AdvanceInstallMenu ()
handler = Menu.handlerMenu


draw :: AdvanceInstallMenu -> Widget Name
draw = Common.frontwardLayer "Advance Install" . Menu.drawMenu
