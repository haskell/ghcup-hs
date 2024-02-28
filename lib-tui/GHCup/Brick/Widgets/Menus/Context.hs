{-# LANGUAGE OverloadedStrings #-}

module GHCup.Brick.Widgets.Menus.Context (ContextMenu, create, draw, handler) where

import Brick (
  Widget (..), BrickEvent, EventM,
 )
import Data.Function ((&))
import Prelude hiding (appendFile)

import Data.Versions (prettyVer)
import GHCup.List ( ListResult(..) )
import GHCup.Types (KeyCombination, Tool (..))

import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.Widgets.Menu as Menu
import GHCup.Brick.Common (Name (..))
import GHCup.Brick.Widgets.Menu (Menu)
import qualified Brick.Widgets.Core as Brick
import qualified Brick.Widgets.Border as Border
import qualified Brick.Focus as F
import Brick.Widgets.Core ((<+>))

import Optics (to)
import Optics.Operators ((.~), (^.))
import Optics.Optic ((%))
import Data.Foldable (foldl')

type ContextMenu = Menu ListResult Name

create :: ListResult -> KeyCombination -> ContextMenu
create lr exit_key = Menu.createMenu Common.ContextBox lr exit_key buttons []
 where
  advInstallButton =
    Menu.createButtonField (MenuElement Common.AdvanceInstallButton)
      & Menu.fieldLabelL .~ "Install"
      & Menu.fieldHelpMsgL .~ "Advance Installation Settings"
  compileButton =
    Menu.createButtonField (MenuElement Common.CompilieButton)
      & Menu.fieldLabelL .~ "Compile"
      & Menu.fieldHelpMsgL .~ "Compile tool from source"
  buttons =
    case lTool lr of
      GHC -> [advInstallButton, compileButton]
      HLS -> [advInstallButton, compileButton]
      _ -> [advInstallButton]

draw :: ContextMenu -> Widget Name
draw menu = 
  Common.frontwardLayer
    ("Context Menu for " <> tool_str <> " " <> prettyVer (lVer $ menu ^. Menu.menuStateL))
    $ Brick.vBox
        [ Brick.vBox buttonWidgets
        , Brick.txt " "
        , Brick.padRight Brick.Max $ 
            Brick.txt "Press " 
            <+> Common.keyToWidget (menu ^. Menu.menuExitKeyL)
            <+> Brick.txt " to go back"
        ]
  where
    buttonLabels = [button & Menu.fieldLabel | button <- menu ^. Menu.menuButtonsL]
    maxWidth = foldl' max 5 (fmap Brick.textWidth buttonLabels)

    buttonAmplifiers =
      let buttonAsWidgets = fmap Menu.renderAslabel buttonLabels
       in fmap (\f b -> ((Menu.leftify (maxWidth + 10) . Border.border $ f b) <+>) ) buttonAsWidgets
    drawButtons = fmap Menu.drawField buttonAmplifiers
    buttonWidgets = zipWith (F.withFocusRing (menu ^. Menu.menuFocusRingL)) drawButtons (menu ^. Menu.menuButtonsL)
    tool_str =
      case menu ^. Menu.menuStateL % to lTool of
        GHC -> "GHC"
        GHCup -> "GHCup"
        Cabal -> "Cabal"
        HLS -> "HLS"
        Stack -> "Stack"

handler :: BrickEvent Name e -> EventM Name ContextMenu ()
handler = Menu.handlerMenu