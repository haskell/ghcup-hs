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

import qualified GHCup.Brick.Actions as Actions
import qualified GHCup.Brick.Attributes as Attributes
import GHCup.Brick.BrickState (BrickState (..), advanceInstallMenu, appKeys, appSettings, appState, contextMenu, mode, compileGHCMenu)
import GHCup.Brick.Common (Mode (..), Name (..))
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.Widgets.KeyInfo as KeyInfo
import qualified GHCup.Brick.Widgets.Menus.Context as ContextMenu
import qualified GHCup.Brick.Widgets.Navigation as Navigation
import qualified GHCup.Brick.Widgets.Tutorial as Tutorial
import qualified GHCup.Brick.Widgets.Menu as Menu
import qualified GHCup.Brick.Widgets.Menus.AdvanceInstall as AdvanceInstall

import GHCup.Types (AppState (AppState, keyBindings), KeyCombination (KeyCombination))

import qualified Brick.Focus as F
import Brick (
  App (..),
  AttrMap,
  BrickEvent (VtyEvent),
  EventM,
  Widget (..),
  (<=>),
 )
import qualified Brick
import Control.Monad.Reader (
  MonadIO (liftIO),
  void,
 )
import Data.IORef (readIORef)
import Data.List (find, intercalate)
import Prelude hiding (appendFile)

import qualified Graphics.Vty as Vty

import qualified Data.Text as T

import Optics.Getter (to)
import Optics.Operators ((^.))
import Optics.Optic ((%))
import Optics.State (use)
import Optics.State.Operators ((.=))
import qualified GHCup.Brick.Widgets.Menus.CompileGHC as CompileGHC

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
       AdvanceInstallPanel -> [AdvanceInstall.draw (st ^. advanceInstallMenu), navg]
       CompileGHCPanel     -> [CompileGHC.draw (st ^. compileGHCMenu), navg]


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
    (VtyEvent (Vty.EvKey k m), Just n) 
      |  k == exitKey 
          && m == mods 
          && n `elem` [Menu.fieldName button | button <- buttons]
      -> mode .= Navigation
    (VtyEvent (Vty.EvKey Vty.KEnter []),  Just (Common.MenuElement Common.AdvanceInstallButton) ) -> mode .= Common.AdvanceInstallPanel
    (VtyEvent (Vty.EvKey Vty.KEnter []),  Just (Common.MenuElement Common.CompilieButton) ) -> mode .= Common.CompileGHCPanel
    _ -> Common.zoom contextMenu $ ContextMenu.handler ev
-- 
advanceInstallHandler :: BrickEvent Name e -> EventM Name BrickState ()
advanceInstallHandler ev = do
  ctx <- use advanceInstallMenu 
  let focusedElement = ctx ^. Menu.menuFocusRingL % to F.focusGetCurrent
      buttons = ctx ^. Menu.menuButtonsL
      (KeyCombination exitKey mods) = ctx ^. Menu.menuExitKeyL
  case (ev, focusedElement) of
    (_ , Nothing) -> pure ()
    (VtyEvent (Vty.EvKey k m), Just n)
      | k == exitKey
          && m == mods
          && n `elem` [Menu.fieldName button | button <- buttons]
      -> mode .= ContextPanel
    _ -> Common.zoom advanceInstallMenu $ AdvanceInstall.handler ev

compileGHCHandler :: BrickEvent Name e -> EventM Name BrickState ()
compileGHCHandler ev = do
  ctx <- use compileGHCMenu 
  let focusedElement = ctx ^. Menu.menuFocusRingL % to F.focusGetCurrent
      buttons = ctx ^. Menu.menuButtonsL
      (KeyCombination exitKey mods) = ctx ^. Menu.menuExitKeyL
  case (ev, focusedElement) of
    (_ , Nothing) -> pure ()
    (VtyEvent (Vty.EvKey k m), Just n)
      | k == exitKey
          && m == mods
          && n `elem` [Menu.fieldName button | button <- buttons]
      -> mode .= ContextPanel
    _ -> Common.zoom compileGHCMenu $ CompileGHC.handler ev

eventHandler :: BrickEvent Name e -> EventM Name BrickState ()
eventHandler ev = do
  m <- use mode
  case m of
    KeyInfo      -> keyInfoHandler ev
    Tutorial     -> tutorialHandler ev
    Navigation   -> navigationHandler ev
    ContextPanel -> contextMenuHandler ev
    AdvanceInstallPanel -> advanceInstallHandler ev
    CompileGHCPanel     -> compileGHCHandler ev
