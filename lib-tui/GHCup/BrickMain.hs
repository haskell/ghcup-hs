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
import qualified GHCup.Brick.App as BrickApp
import qualified GHCup.Brick.App.Navigation as Navigation
import qualified GHCup.Brick.Widgets.SectionList as SectionList
import qualified GHCup.Brick.Attributes as Attributes
import           GHCup.Brick.Widgets.Menu (MenuKeyBindings(..))
import qualified Brick
import qualified Graphics.Vty as Vty

import Control.Monad.Reader ( ReaderT(runReaderT) )
import Data.Functor ( ($>) )
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
    Right ad -> do
      let nav_widget = Navigation.create Common.AllTools ad
                  (Attributes.dimAttributes $ noColor $ settings s) (keyBindings s)
          current_element = SectionList.sectionListSelectedElement (Navigation._sectionList nav_widget)
          menu_kb =
            let KeyBindings {..} = keyBindings s
            in MenuKeyBindings { mKbUp = bUp, mKbDown = bDown, mKbQuit = bQuit}
      case current_element of
        Nothing -> do
          flip runReaderT s $ logError "Error building app state: empty ResultList"
          exitWith $ ExitFailure 2
        Just (_, e) ->
          let initapp =
                BrickApp.app
                  (Attributes.defaultAttributes $ noColor $ settings s)
              -- installedGHCs = fmap lVer $
              --   filter (\(ListResult {..}) -> lInstalled && lTool == GHC && lCross == Nothing) (Common._lr ad)
          in Brick.defaultMain initapp nav_widget
          $> ()
    Left e -> do
      flip runReaderT s $ logError $ "Error building app state: " <> T.pack (show e)
      exitWith $ ExitFailure 2
