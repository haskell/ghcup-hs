{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCup.Brick.App.ContextMenu where

import GHCup.List ( ListResult (..))
import           GHCup.Types ( KeyBindings(..), Tool(..), KeyCombination(..) )
import GHCup.Brick.Widgets.BaseWidget
import GHCup.Brick.Widgets.BasicOverlay
import GHCup.Brick.Widgets.InputField.Class
import GHCup.Brick.Widgets.InputField.CheckBox
import GHCup.Brick.Widgets.GenericMenu (GenericMenu, state)
import qualified GHCup.Brick.App.AdvanceInstallMenu as AdvanceInstallMenu
import qualified GHCup.Brick.App.CompileGHCMenu as CompileGHCMenu
import qualified GHCup.Brick.App.CompileHLSMenu as CompileHLSMenu
import qualified GHCup.Brick.App.Common as Common
import qualified GHCup.Brick.Common as Common
import GHCup.Brick.App.Tutorial (Tutorial(..))


import Brick
    ( BrickEvent(..),
      Padding(Max),
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import qualified Brick.Focus as F
import           Brick.Widgets.Center ( center )
import           Data.Maybe
import Data.Some
import qualified Data.Text                     as T
import Data.Versions (prettyVer, Version)
import           Prelude                 hiding ( appendFile )
import qualified Graphics.Vty as Vty
import Optics (Lens', lens, over, (^.), (%), (&), (.~), (%~))
import Optics.State.Operators ((.=), (?=), (%=))
import Optics.TH (makeLenses)

data ContextMenu = ContextMenu
  { _menuKeys :: Common.MenuKeyBindings
  , _listResult :: ListResult
  , _focusRing :: F.FocusRing Common.Name
  , _overlay :: Maybe (Some (IsSubWidget Common.Name ContextMenu))
  , _advanceInstallMenu :: BasicOverlay Common.Name AdvanceInstallMenu.AdvanceInstallMenu
  , _compileGHCMenu :: BasicOverlay Common.Name CompileGHCMenu.CompileGHCMenu
  , _compileHLSMenu :: BasicOverlay Common.Name CompileHLSMenu.CompileHLSMenu
  }

makeLenses ''ContextMenu

create :: KeyBindings -> ListResult -> [Version] -> ContextMenu
create kb lr availableGHCs = ContextMenu (Common.toMenuKeyBindings kb) lr (mkFocusRing lr) Nothing
  (BasicOverlay (AdvanceInstallMenu.create kb lr) [bQuit kb] (Common.frontwardLayer "Advance Install"))
  (BasicOverlay (CompileGHCMenu.create kb lr availableGHCs) [bQuit kb] (Common.frontwardLayer "Compile GHC"))
  (BasicOverlay (CompileHLSMenu.create kb lr availableGHCs) [bQuit kb] (Common.frontwardLayer "Compile HLS"))

updateListResult :: ListResult -> ContextMenu -> ContextMenu
updateListResult lr v = v
  & focusRing .~ mkFocusRing lr
  & listResult .~ lr
  & advanceInstallMenu % innerWidget % state .~ lr
  & compileGHCMenu % innerWidget % state .~ lr
  & compileHLSMenu % innerWidget % state .~ lr

updateAvailableGHCs :: [Version] -> ContextMenu -> ContextMenu
updateAvailableGHCs availableGHCs v = v
  & compileGHCMenu % innerWidget %~ CompileGHCMenu.updateAvailableGHCs availableGHCs
  & compileHLSMenu % innerWidget %~ CompileHLSMenu.updateAvailableGHCs availableGHCs

mkFocusRing :: ListResult -> F.FocusRing Common.Name
mkFocusRing (ListResult {..}) = F.focusRing $
  case lTool of
    GHC -> [Common.MenuElement Common.CompileGHCButton, Common.MenuElement Common.AdvanceInstallButton]
    HLS -> [Common.MenuElement Common.CompileHLSButton, Common.MenuElement Common.AdvanceInstallButton]
    _ -> [Common.MenuElement Common.AdvanceInstallButton]

mkTitle :: ListResult -> T.Text
mkTitle (ListResult {..}) = "Context Menu for " <> tool_str <> " " <> prettyVer lVer
  where
    tool_str =
      case lTool of
        GHC -> "GHC"
        GHCup -> "GHCup"
        Cabal -> "Cabal"
        HLS -> "HLS"
        Stack -> "Stack"

instance BaseWidget Common.Name ContextMenu where
  draw (ContextMenu {..}) = Brick.vBox
    [ Brick.vBox buttonWidgets
    , Brick.txt " "
    , Brick.padRight Brick.Max $
        Brick.txt "Press "
        <+> Common.keyToWidget (_menuKeys ^. Common.mKbQuit)
        <+> Brick.txt " to go back"
    ]
    where
      buttonWidgets = map drawButtons (F.focusRingToList $ mkFocusRing _listResult)
      maxWidth = 10
      currentFocus = fromMaybe (Common.MenuElement Common.AdvanceInstallButton) $ F.focusGetCurrent _focusRing

      drawButtons n@(Common.MenuElement Common.CompileGHCButton) =
        drawOneField (n, "Compile") (Brick.txt "Compile GHC from source")
      drawButtons n@(Common.MenuElement Common.CompileHLSButton) =
        drawOneField (n, "Compile") (Brick.txt "Compile HLS from source")
      drawButtons n@(Common.MenuElement Common.AdvanceInstallButton) =
        drawOneField (n, "Install") (Brick.txt "Advance Installation Settings")
      drawButtons _ = Brick.txt "<NYI>"

      drawOneField (n, l) f = Common.rightify (maxWidth + 1) (Common.renderAslabel l (n == currentFocus) <+> Brick.txt " ") <+> f

  handleEvent ev = do
    (ContextMenu {..}) <- Brick.get
    let
      currentFocus = fromMaybe (Common.MenuElement Common.AdvanceInstallButton) $ F.focusGetCurrent _focusRing
    case ev of
      VtyEvent (Vty.EvKey k m)
        | KeyCombination k m == _menuKeys ^. Common.mKbUp    -> do
            focusRing %= F.focusPrev
        | KeyCombination k m == _menuKeys ^. Common.mKbDown  -> do
            focusRing %= F.focusNext
      VtyEvent (Vty.EvKey Vty.KEnter [])
        | currentFocus == (Common.MenuElement Common.AdvanceInstallButton) ->
          overlay ?= Some (IsSubWidget advanceInstallMenu)
        | currentFocus == (Common.MenuElement Common.CompileGHCButton) ->
          overlay ?= Some (IsSubWidget compileGHCMenu)
        | currentFocus == (Common.MenuElement Common.CompileHLSButton) ->
          overlay ?= Some (IsSubWidget compileHLSMenu)
      _ -> pure ()
    pure Nothing

  hasOverlay = _overlay
  closeOverlay = overlay .= Nothing
