{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-
This module defines the brick App. The pattern is very simple:

- Pattern match on the Mode
- Dispatch drawing/events to the corresponding widget/s

In general each widget should know how to draw itself and how to handle its own events, so this
module should only contain:

- how to draw non-widget information. For example the footer
- how to change between modes (widgets aren't aware of the whole application state)

-}

module GHCup.Brick.App where

import           GHCup.Types ( AppState(AppState, keyBindings), KeyCombination(KeyCombination) )
import           GHCup.Brick.Common ( Name(..), Mode(..))
import qualified GHCup.Brick.Common as Common
import           GHCup.Brick.BrickState (BrickState(..), appState, mode, appKeys, appSettings, contextMenu)
import qualified GHCup.Brick.Attributes as Attributes
import qualified GHCup.Brick.Widgets.Navigation as Navigation
import qualified GHCup.Brick.Widgets.Tutorial as Tutorial
import qualified GHCup.Brick.Widgets.KeyInfo as KeyInfo
import qualified GHCup.Brick.Widgets.Menus.Context as ContextMenu
import qualified GHCup.Brick.Actions as Actions

import Brick
    ( BrickEvent(VtyEvent),
      App(..),
      AttrMap,
      EventM,
      Widget(..),
      (<=>))
import qualified Brick
import Control.Monad.Reader
    ( void, MonadIO(liftIO) )
import Data.List ( find, intercalate)
import           Data.IORef (readIORef)
import           Prelude                 hiding ( appendFile )

import qualified Graphics.Vty                  as Vty

import           Optics.State (use)
import           Optics.State.Operators ( (.=))
import           Optics.Operators ((^.))
import qualified Data.Text as T
import qualified GHCup.Brick.Widgets.Menu as Menu
import Optics.Optic ((%))
import qualified Brick.Focus as F
import Optics.Getter (to)


app :: AttrMap -> AttrMap -> App BrickState () Name
app attrs dimAttrs =
  App { appDraw         = drawUI dimAttrs
      , appHandleEvent  = eventHandler
      , appStartEvent   = return ()
      , appAttrMap      = const attrs
      , appChooseCursor = Brick.showFirstCursor
      }

drawUI :: AttrMap -> BrickState -> [Widget Name]
drawUI dimAttrs st =
  let 
    footer = Brick.withAttr Attributes.helpAttr
      . Brick.txtWrap
      . T.pack
      . foldr1 (\x y -> x <> "  " <> y)
      . fmap (\(KeyCombination key mods, pretty_setting, _) 
                  -> intercalate "+" (Common.showKey key : (Common.showMod <$> mods)) <> ":" <> pretty_setting (st ^. appSettings)
             )
      $ Actions.keyHandlers (st ^. appKeys)
    navg = Navigation.draw dimAttrs (st ^. appState) <=> footer
  in case st ^. mode of
       Navigation   -> [navg]
       Tutorial     -> [Tutorial.draw, navg]
       KeyInfo      -> [KeyInfo.draw (st ^. appKeys), navg]
       ContextPanel -> [ContextMenu.draw (st ^. contextMenu), navg]

-- | On q, go back to navigation. 
--   On Enter, to go to tutorial
keyInfoHandler :: BrickEvent Name e -> EventM Name BrickState ()
keyInfoHandler ev = case ev of
  VtyEvent (Vty.EvKey (Vty.KChar 'q') _ ) -> mode .= Navigation
  VtyEvent (Vty.EvKey Vty.KEnter _ )   -> mode .= Tutorial
  _ -> pure ()

-- | On q, go back to navigation. Else, do nothing
tutorialHandler :: BrickEvent Name e -> EventM Name BrickState ()
tutorialHandler ev =
  case ev of
    VtyEvent (Vty.EvKey (Vty.KChar 'q') _ ) -> mode .= Navigation
    _ -> pure ()

-- | Tab/Arrows to navigate. 
navigationHandler :: BrickEvent Name e -> EventM Name BrickState ()
navigationHandler ev = do
  AppState { keyBindings = kb } <- liftIO $ readIORef Actions.settings'
  case ev of
    inner_event@(VtyEvent (Vty.EvKey key _)) ->
      case find (\(key', _, _) -> key' == KeyCombination key []) (Actions.keyHandlers kb) of
        Just (_, _, handler) -> handler
        Nothing -> void $ Common.zoom appState $ Navigation.handler inner_event
    inner_event -> Common.zoom appState $ Navigation.handler inner_event

contextMenuHandler :: BrickEvent Name e -> EventM Name BrickState ()
contextMenuHandler ev = do
  ctx <- use contextMenu 
  let focusedElement = ctx ^. Menu.menuFocusRingL % to F.focusGetCurrent
      buttons = ctx ^. Menu.menuButtonsL
      (KeyCombination exitKey mods) = ctx ^. Menu.menuExitKeyL
  case (ev, focusedElement) of
    (_ , Nothing) -> pure ()
    (VtyEvent (Vty.EvKey k m), Just n ) 
      |  k == exitKey 
          && m == mods 
          && n `elem` [Menu.fieldName button | button <- buttons]
      -> mode .= Navigation
    (VtyEvent (Vty.EvKey Vty.KEnter []),  Just (Common.MenuElement Common.AdvanceInstallButton) ) -> pure ()
    (VtyEvent (Vty.EvKey Vty.KEnter []),  Just (Common.MenuElement Common.CompilieButton) ) -> pure ()
    _ -> Common.zoom contextMenu $ ContextMenu.handler ev

eventHandler :: BrickEvent Name e -> EventM Name BrickState ()
eventHandler ev = do
  m <- use mode
  case m of
    KeyInfo      -> keyInfoHandler ev
    Tutorial     -> tutorialHandler ev
    Navigation   -> navigationHandler ev
    ContextPanel -> contextMenuHandler ev
