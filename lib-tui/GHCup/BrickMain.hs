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
      AppState(ghcupInfo, settings, keyBindings, loggerConfig) )
import GHCup.Prelude.Logger ( logError )
import qualified GHCup.Brick.Actions as Actions
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.App as BrickApp
import qualified GHCup.Brick.Attributes as Attributes
import qualified GHCup.Brick.BrickState as AppState
import qualified Brick

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
    Right ad ->
      Brick.defaultMain
          (BrickApp.app (Attributes.defaultAttributes (noColor $ settings s))
                        (Attributes.dimAttributes (noColor $ settings s)))
          (AppState.BrickState ad
                    Common.defaultAppSettings
                    (Actions.constructList ad Common.defaultAppSettings Nothing)
                    (keyBindings (s :: AppState))
                    Common.Navigation

          )
        $> ()
    Left e -> do
      flip runReaderT s $ logError $ "Error building app state: " <> T.pack (show e)
      exitWith $ ExitFailure 2
