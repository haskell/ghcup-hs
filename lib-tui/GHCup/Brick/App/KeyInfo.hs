{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCup.Brick.App.KeyInfo where

import           GHCup.Types ( KeyBindings(..) )
import GHCup.Brick.Widgets.BaseWidget
import GHCup.Brick.Widgets.BasicOverlay
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
import           Brick.Widgets.Center ( center )
import Data.Some
import           Prelude                 hiding ( appendFile )
import qualified Graphics.Vty as Vty
import Optics.State.Operators ((.=), (?=))
import Optics.TH (makeLenses)

data KeyInfo = KeyInfo
  { _appKeys :: KeyBindings
  , _tutorial :: BasicOverlay Common.Name Tutorial
  , _overlay :: Maybe (Some (IsSubWidget Common.Name KeyInfo))
  }

makeLenses ''KeyInfo

create :: KeyBindings -> KeyInfo
create kb = KeyInfo kb (BasicOverlay (Tutorial (bQuit kb)) [] (Common.frontwardLayer "Tutorial")) Nothing

instance BaseWidget Common.Name KeyInfo where
  draw (KeyInfo {..}) =
    let
      KeyBindings {..} = _appKeys
      mkTextBox = Brick.hLimitPercent 70 . Brick.vBox . fmap (Brick.padRight Brick.Max)
    in Brick.vBox [
        center $
         mkTextBox [
            Brick.hBox [
              Brick.txt "Press "
            , Common.keyToWidget bUp, Brick.txt " and ", Common.keyToWidget bDown
            , Brick.txtWrap " to navigate the list of tools"
            ]
          , Brick.hBox [
              Brick.txt "Press "
            , Common.keyToWidget bInstall
            , Brick.txtWrap " to install the selected tool. Notice, you may need to set it as default afterwards"
            ]
          , Brick.hBox [
              Brick.txt "Press "
            , Common.keyToWidget bSet
            , Brick.txtWrap " to set a tool as the one for use"
            ]
          , Brick.hBox [
              Brick.txt "Press "
            , Common.keyToWidget bUninstall
            , Brick.txtWrap " to uninstall a tool"
            ]
          , Brick.hBox [
              Brick.txt "Press "
            , Common.keyToWidget bChangelog
            , Brick.txtWrap " to open the tool's changelog. It will open a web browser"
            ]
          , Brick.hBox [
              Brick.txt "Press "
            , Common.keyToWidget bShowAllVersions
            , Brick.txtWrap " to show older version of each tool"
            ]
          ]
        ]
      <=> Brick.hBox [Brick.txt "Press " <+> Common.keyToWidget bQuit <+> Brick.txt " to return to Navigation" <+> Brick.padRight Brick.Max (Brick.txt " ") <+> Brick.txt "Press Enter to go to the Tutorial"]

  handleEvent ev = do
    case ev of
      VtyEvent (Vty.EvKey Vty.KEnter _ ) -> overlay ?= Some (IsSubWidget tutorial)
      _ -> pure ()
    pure Nothing

  hasOverlay = _overlay
  closeOverlay = overlay .= Nothing
