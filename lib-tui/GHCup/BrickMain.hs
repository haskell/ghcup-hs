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

import GHCup.Types
    ( Settings(noColor),
      AppState(ghcupInfo, settings, keyBindings, loggerConfig), KeyBindings (..), KeyCombination (KeyCombination) )
import GHCup.Prelude.Logger ( logError )
import qualified GHCup.Brick.Actions as Actions
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.App as BrickApp
import qualified GHCup.Brick.Attributes as Attributes
import qualified GHCup.Brick.BrickState as AppState
import qualified GHCup.Brick.Widgets.Menus.Context as ContextMenu
import qualified GHCup.Brick.Widgets.SectionList as Navigation
import qualified GHCup.Brick.Widgets.Menus.AdvanceInstall as AdvanceInstall
import qualified GHCup.Brick.Widgets.Menus.CompileGHC as CompileGHC
import qualified Brick
import qualified Graphics.Vty as Vty

import Control.Monad.Reader ( ReaderT(runReaderT) )
import Data.Functor ( ($>) )
import           Data.IORef (writeIORef)
import           Prelude                 hiding ( appendFile )
import System.Exit ( ExitCode(ExitFailure), exitWith )

import qualified Data.Text                     as T
import qualified GHCup.Brick.Widgets.Menus.CompileHLS as CompileHLS



brickMain :: AppState
          -> IO ()
brickMain s = do
  writeIORef Actions.settings' s

  eAppData <- Actions.getAppData (Just $ ghcupInfo s)
  case eAppData of
    Right ad -> do
      let initial_list = Actions.constructList ad Common.defaultAppSettings Nothing
          current_element = Navigation.sectionListSelectedElement initial_list
          exit_key = KeyCombination (Vty.KChar 'c') [Vty.MCtrl] -- bQuit . keyBindings $ s
      case current_element of
        Nothing -> do
          flip runReaderT s $ logError "Error building app state: empty ResultList"
          exitWith $ ExitFailure 2
        Just (_, e) ->
          let initapp =
                BrickApp.app
                  (Attributes.defaultAttributes $ noColor $ settings s)
                  (Attributes.dimAttributes $ noColor $ settings s)
              initstate =
                AppState.BrickState ad
                      Common.defaultAppSettings
                      initial_list
                      (ContextMenu.create e exit_key)
                      (AdvanceInstall.create exit_key)
                      (CompileGHC.create exit_key)
                      (CompileHLS.create exit_key)
                      (keyBindings s)
                      Common.Navigation
          in Brick.defaultMain initapp initstate
          $> ()
    Left e -> do
      flip runReaderT s $ logError $ "Error building app state: " <> T.pack (show e)
      exitWith $ ExitFailure 2
