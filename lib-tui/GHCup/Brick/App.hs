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
import GHCup.Brick.BrickState (BrickState (..), advanceInstallMenu, appKeys, appSettings, appState, contextMenu, mode, compileGHCMenu, compileHLSMenu)
import GHCup.Brick.App.Common (Mode (..), Name (..))
import qualified GHCup.Brick.App.Common as Common
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.Widgets.BaseWidget as BaseWidget
import qualified GHCup.Brick.Widgets.BasicOverlay as BasicOverlay
import qualified GHCup.Brick.App.Navigation as Navigation
import qualified GHCup.Brick.Widgets.Menus.Context as ContextMenu
import qualified GHCup.Brick.Widgets.Menu as Menu
import qualified GHCup.Brick.Widgets.Menus.AdvanceInstall as AdvanceInstall

import GHCup.List (ListResult)
import GHCup.Types (AppState (AppState, keyBindings), KeyCombination (KeyCombination), KeyBindings (..))

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
  MonadIO (liftIO), ReaderT,
 )
import Data.IORef (readIORef)
import Data.List (find, intercalate)
import Prelude hiding (appendFile)

import qualified Graphics.Vty as Vty

import qualified Data.Text as T

import Optics (Lens')
import Optics.Getter (to)
import Optics.Operators ((^.))
import Optics.Optic ((%))
import Optics.State (use)
import Optics.State.Operators ((.=))
import qualified GHCup.Brick.Widgets.Menus.CompileGHC as CompileGHC
import qualified GHCup.Brick.Widgets.Menus.CompileHLS as CompileHLS
import Control.Monad (void, when)

app :: AttrMap -> App Navigation.Navigation () Name
app attrs =
  App { appDraw         = BaseWidget.drawBaseWidget
      , appHandleEvent  = void . BaseWidget.handleEventBaseWidget
      , appStartEvent   = setupVtyMode
      , appAttrMap      = const attrs
      , appChooseCursor = Brick.showFirstCursor
      }

-- | Enable mouse mode if supported by the terminal
setupVtyMode :: EventM Name s ()
setupVtyMode = do
  vty <- Brick.getVtyHandle
  let output = Vty.outputIface vty
  when (Vty.supportsMode output Vty.Mouse) $
      liftIO $ Vty.setMode output Vty.Mouse True

-- drawUI :: AttrMap -> BrickState -> [Widget Name]
-- drawUI dimAttrs st =
--   let
--     footer = Brick.withAttr Attributes.helpAttr
--       . Brick.txtWrap
--       . T.pack
--       . foldr1 (\x y -> x <> "  " <> y)
--       . fmap (\(KeyCombination key mods, pretty_setting, _)
--                   -> intercalate "+" (Common.showKey key : (Common.showMod <$> mods)) <> ":" <> pretty_setting (st ^. appSettings)
--              )
--       $ Actions.keyHandlers (st ^. appKeys)
--     navg = Navigation.draw dimAttrs (st ^. appState) <=> footer
--   in case st ^. mode of
--        Navigation   -> [navg]
--        Tutorial     -> [Tutorial.draw (bQuit $ st ^. appKeys), navg]
--        KeyInfo      -> [KeyInfo.draw (st ^. appKeys), navg]
--        ContextPanel -> [ContextMenu.draw (st ^. contextMenu), navg]
--        AdvanceInstallPanel -> AdvanceInstall.draw (st ^. advanceInstallMenu) ++ [navg]
--        CompileGHCPanel     -> CompileGHC.draw (st ^. compileGHCMenu) ++ [navg]
--        CompileHLSPanel     -> CompileHLS.draw (st ^. compileHLSMenu) ++ [navg]

-- -- | On q, go back to navigation.
-- --   On Enter, to go to tutorial
-- keyInfoHandler :: BrickEvent Name e -> EventM Name BrickState ()
-- keyInfoHandler ev = do
--   AppState { keyBindings = kb } <- liftIO $ readIORef Actions.settings'
--   case ev of
--     VtyEvent (Vty.EvKey Vty.KEnter _ )   -> mode .= Tutorial
--     VtyEvent (Vty.EvKey key mods)
--       | bQuit kb == KeyCombination key mods -> mode .= Navigation
--     _ -> pure ()

-- -- | On q, go back to navigation. Else, do nothing
-- tutorialHandler :: BrickEvent Name e -> EventM Name BrickState ()
-- tutorialHandler ev = do
--   AppState { keyBindings = kb } <- liftIO $ readIORef Actions.settings'
--   case ev of
--     VtyEvent (Vty.EvKey key mods)
--       | bQuit kb == KeyCombination key mods -> mode .= Navigation
--     _ -> pure ()

-- -- | Tab/Arrows to navigate.
-- navigationHandler :: BrickEvent Name e -> EventM Name BrickState ()
-- navigationHandler ev = do
--   AppState { keyBindings = kb } <- liftIO $ readIORef Actions.settings'
--   case ev of
--     inner_event@(VtyEvent (Vty.EvKey key mods)) ->
--       case find (\(key', _, _) -> key' == KeyCombination key mods) (Actions.keyHandlers kb) of
--         Just (_, _, handler) -> handler
--         Nothing -> void $ Common.zoom appState $ Navigation.handler inner_event
--     inner_event -> Common.zoom appState $ Navigation.handler inner_event

-- contextMenuHandler :: BrickEvent Name e -> EventM Name BrickState ()
-- contextMenuHandler ev = do
--   ctx <- use contextMenu
--   let focusedElement = ctx ^. Menu.menuFocusRingL % to F.focusGetCurrent
--       (KeyCombination exitKey mods) = ctx ^. Menu.menuKeyBindingsL % Menu.mKbQuitL
--   case (ev, focusedElement) of
--     (_ , Nothing) -> pure ()
--     (VtyEvent (Vty.EvKey k m), Just n) |  k == exitKey && m == mods -> mode .= Navigation
--     (VtyEvent (Vty.EvKey Vty.KEnter []),  Just (Common.MenuElement Common.AdvanceInstallButton) ) -> mode .= Common.AdvanceInstallPanel
--     (VtyEvent (Vty.EvKey Vty.KEnter []),  Just (Common.MenuElement Common.CompileGHCButton) ) -> mode .= Common.CompileGHCPanel
--     (VtyEvent (Vty.EvKey Vty.KEnter []),  Just (Common.MenuElement Common.CompileHLSButton) ) -> mode .= Common.CompileHLSPanel
--     _ -> Common.zoom contextMenu $ ContextMenu.handler ev
-- --
-- advanceInstallHandler :: BrickEvent Name e -> EventM Name BrickState ()
-- advanceInstallHandler = menuWithOverlayHandler advanceInstallMenu Actions.installWithOptions AdvanceInstall.handler

-- compileGHCHandler :: BrickEvent Name e -> EventM Name BrickState ()
-- compileGHCHandler = menuWithOverlayHandler compileGHCMenu Actions.compileGHC CompileGHC.handler

-- compileHLSHandler :: BrickEvent Name e -> EventM Name BrickState ()
-- compileHLSHandler = menuWithOverlayHandler compileHLSMenu Actions.compileHLS CompileHLS.handler

-- -- | Passes all events to innerHandler if an overlay is opened
-- -- else handles the exitKey and Enter key for the Menu's "OkButton"
-- menuWithOverlayHandler :: Lens' BrickState (Menu.Menu t Name)
--   -> (t -> ((Int, ListResult) -> ReaderT AppState IO (Either String a)))
--   -> (BrickEvent Name e -> EventM Name (Menu.Menu t Name) ())
--   -> BrickEvent Name e
--   -> EventM Name BrickState ()
-- menuWithOverlayHandler accessor action innerHandler ev = do
--   ctx <- use accessor
--   let focusedElement = ctx ^. Menu.menuFocusRingL % to F.focusGetCurrent
--       focusedField = (\n -> find (\x -> Brick.getName x == n) $ ctx ^. Menu.menuFieldsL) =<< focusedElement
--       (KeyCombination exitKey mods) = ctx ^. Menu.menuKeyBindingsL % Menu.mKbQuitL
--   case (ev, focusedElement, Menu.drawFieldOverlay =<< focusedField) of
--     (_ , Nothing, _) -> pure ()
--     (_ , _, Just _) -> Common.zoom accessor $ innerHandler ev
--     (VtyEvent (Vty.EvKey k m), Just n, _) | k == exitKey && m == mods -> mode .= ContextPanel
--     (VtyEvent (Vty.EvKey Vty.KEnter []), Just (MenuElement Common.OkButton), _) -> do
--         let iopts = ctx ^. Menu.menuStateL
--         when (Menu.isValidMenu ctx)
--           (Actions.withIOAction $ action iopts)
--     _ -> Common.zoom accessor $ innerHandler ev

-- eventHandler :: BrickEvent Name e -> EventM Name BrickState ()
-- eventHandler ev = do
--   m <- use mode
--   case m of
--     KeyInfo      -> keyInfoHandler ev
--     Tutorial     -> tutorialHandler ev
--     Navigation   -> navigationHandler ev
--     ContextPanel -> contextMenuHandler ev
--     AdvanceInstallPanel -> advanceInstallHandler ev
--     CompileGHCPanel     -> compileGHCHandler ev
--     CompileHLSPanel     -> compileHLSHandler ev
