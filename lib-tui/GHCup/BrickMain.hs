{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-
This module contains the entrypoint for the brick application and nothing else.

-}

module GHCup.BrickMain where

import GHCup.List ( ListResult (..))
import GHCup.Types
    ( Settings(noColor), Tool (GHC),
      AppState(ghcupInfo, settings, keyBindings, loggerConfig), KeyBindings(..) )
import GHCup.Prelude.Logger ( logError )
import qualified GHCup.Brick.Actions as Actions
import qualified GHCup.Brick.App.Common as Common
import qualified GHCup.Brick.App.Navigation as Navigation
import qualified GHCup.Brick.Widgets.BaseWidget as BaseWidget
import qualified GHCup.Brick.Attributes as Attributes
import           GHCup.Brick.Widgets.Menu (MenuKeyBindings(..))
import qualified Brick
import qualified Graphics.Vty as Vty

import Control.Monad
import Control.Monad.Reader ( ReaderT(runReaderT), liftIO )
import Data.Functor ( ($>) )
import           Data.List.NonEmpty             ( NonEmpty (..) )
import           Data.IORef (writeIORef)
import           Prelude                 hiding ( appendFile )
import System.Exit ( ExitCode(ExitFailure), exitWith )

import qualified Data.Text                     as T



brickMain :: AppState
          -> IO ()
brickMain s = do
  writeIORef Actions.settings' s

  eAppData <- Actions.getAppData (Just $ ghcupInfo s)
  case eAppData of
    Left e -> do
      flip runReaderT s $ logError $ "Error building app state: " <> T.pack (show e)
      exitWith $ ExitFailure 2
    Right [] -> do
      flip runReaderT s $ logError "Error building app state: empty ResultList"
      exitWith $ ExitFailure 2
    Right (x:xs) -> do
      let nav_widget = Navigation.create Common.AllTools (x :| xs)
                  (Attributes.dimAttributes $ noColor $ settings s) (keyBindings s)
          menu_kb =
            let KeyBindings {..} = keyBindings s
            in MenuKeyBindings { mKbUp = bUp, mKbDown = bDown, mKbQuit = bQuit}
      let initapp = brickApp (Attributes.defaultAttributes $ noColor $ settings s)
      Brick.defaultMain initapp nav_widget $> ()

brickApp :: Brick.AttrMap -> Brick.App Navigation.Navigation () Common.Name
brickApp attrs =
  Brick.App { appDraw         = BaseWidget.drawBaseWidget
            , appHandleEvent  = void . BaseWidget.handleEventBaseWidget
            , appStartEvent   = setupVtyMode
            , appAttrMap      = const attrs
            , appChooseCursor = Brick.showFirstCursor
            }

-- | Enable mouse mode if supported by the terminal
setupVtyMode :: Brick.EventM Common.Name s ()
setupVtyMode = do
  vty <- Brick.getVtyHandle
  let output = Vty.outputIface vty
  when (Vty.supportsMode output Vty.Mouse) $
      liftIO $ Vty.setMode output Vty.Mouse True
