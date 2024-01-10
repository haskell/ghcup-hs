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
A very simple information-only widget with no handler. 
-}

module GHCup.Brick.Widgets.KeyInfo where

import           GHCup.Types ( KeyBindings(..), KeyCombination(KeyCombination) )
import qualified GHCup.Brick.Common as Common


import Brick
    ( Padding(Max),
      Widget(..), 
      (<+>),
      (<=>))
import qualified Brick
import           Brick.Widgets.Border ( borderWithLabel)
import           Brick.Widgets.Border.Style ( unicode )
import           Brick.Widgets.Center ( center, centerLayer )
import           Data.List ( intercalate )
import           Prelude                 hiding ( appendFile )



draw :: KeyBindings -> Widget Common.Name
draw KeyBindings {..} =
  let
    mkTextBox = Brick.hLimitPercent 70 . Brick.vBox . fmap (Brick.padRight Brick.Max)
    keyToWidget (KeyCombination key mods) = Brick.str $ intercalate "+" (Common.showKey key : (Common.showMod <$> mods))
  in centerLayer
      $ Brick.hLimitPercent 75
      $ Brick.vLimitPercent 50
      $ Brick.withBorderStyle unicode
      $ borderWithLabel (Brick.txt "Key Actions")
      $ Brick.vBox [
        center $
         mkTextBox [
            Brick.hBox [
              Brick.txt "Press "
            , keyToWidget bUp, Brick.txt " and ", keyToWidget bDown
            , Brick.txtWrap " to navigate the list of tools"
            ]
          , Brick.hBox [
              Brick.txt "Press "
            , keyToWidget bInstall
            , Brick.txtWrap " to install the selected tool. Notice, you may need to set it as default afterwards"
            ]
          , Brick.hBox [
              Brick.txt "Press "
            , keyToWidget bSet
            , Brick.txtWrap " to set a tool as the one for use"
            ]
          , Brick.hBox [
              Brick.txt "Press "
            , keyToWidget bUninstall
            , Brick.txtWrap " to uninstall a tool"
            ]
          , Brick.hBox [
              Brick.txt "Press "
            , keyToWidget bChangelog
            , Brick.txtWrap " to open the tool's changelog. It will open a web browser"
            ]
          , Brick.hBox [
              Brick.txt "Press "
            , keyToWidget bShowAllVersions
            , Brick.txtWrap " to show older version of each tool"
            ]
          ]
        ]
      <=> Brick.hBox [Brick.txt "Press q to return to Navigation" <+> Brick.padRight Brick.Max (Brick.txt " ") <+> Brick.txt "Press Enter to go to the Tutorial"]
