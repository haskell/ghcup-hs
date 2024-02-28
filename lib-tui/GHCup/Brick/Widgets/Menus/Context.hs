{-# LANGUAGE OverloadedStrings #-}

module GHCup.Brick.Widgets.Menus.Context (ContextMenu, create, draw, handler) where

import Brick (
  Widget (..), BrickEvent, EventM,
 )
import Data.Function ((&))
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.Widgets.Menu as Menu
import Prelude hiding (appendFile)

import qualified Data.Text as T

import Data.Versions (prettyVer)
import GHCup (ListResult (..))
import GHCup.Brick.Common (Name (..))
import GHCup.Brick.Widgets.Menu (Menu)
import GHCup.Types (KeyCombination, Tool (..))
import Optics (to)
import Optics.Operators ((.~), (^.))
import Optics.Optic ((%))

type ContextMenu = Menu ListResult Name

create :: ListResult -> KeyCombination -> ContextMenu
create lr exit_key = Menu.createMenu Common.ContextBox lr exit_key buttons []
 where
  advInstallButton =
    Menu.createButtonField (MenuElement Common.AdvanceInstallButton)
      & Menu.fieldLabelL .~ "Install"
      & Menu.fieldHelpMsgL .~ "Advance Installation Settings"
  compileButton =
    Menu.createButtonField (MenuElement Common.AdvanceInstallButton)
      & Menu.fieldLabelL .~ "Compile"
      & Menu.fieldHelpMsgL .~ "Compile tool from source (to be implemented)"
  buttons =
    case lTool lr of
      GHC -> [advInstallButton, compileButton]
      HLS -> [advInstallButton, compileButton]
      _ -> [advInstallButton]

draw :: ContextMenu -> Widget Name
draw ctx =
  Common.frontwardLayer
    ("Context Menu for " <> tool_str <> " " <> prettyVer (lVer $ ctx ^. Menu.menuStateL))
    (Menu.drawMenu ctx)
 where
  tool_str :: T.Text
  tool_str =
    case ctx ^. Menu.menuStateL % to lTool of
      GHC -> "GHC"
      GHCup -> "GHCup"
      Cabal -> "Cabal"
      HLS -> "HLS"
      Stack -> "Stack"

handler :: BrickEvent Name e -> EventM Name ContextMenu ()
handler = Menu.handlerMenu