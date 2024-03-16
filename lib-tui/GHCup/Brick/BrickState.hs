{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

{-
This module contains the BrickState. One could be tempted to include this data structure in GHCup.Brick.Common,
but it is better to make a separated module in order to avoid cyclic dependencies.

This happens because the BrickState is sort of a container for all widgets,
but widgets depends on common functionality, hence:

             BrickState `depends on` Widgets.XYZ `depends on` Common

The linear relation above breaks if BrickState is defined in Common.

-}

module GHCup.Brick.BrickState where

import GHCup.Types                    ( KeyBindings )
import GHCup.Brick.Common             ( BrickData(..), BrickSettings(..), Mode(..))
import GHCup.Brick.Widgets.Navigation ( BrickInternalState)
import GHCup.Brick.Widgets.Menus.Context (ContextMenu)
import GHCup.Brick.Widgets.Menus.AdvanceInstall (AdvanceInstallMenu)
import GHCup.Brick.Widgets.Menus.CompileGHC (CompileGHCMenu)
import Optics.TH                      (makeLenses)
import GHCup.Brick.Widgets.Menus.CompileHLS (CompileHLSMenu)


data BrickState = BrickState
  { _appData            :: BrickData
  , _appSettings        :: BrickSettings
  , _appState           :: BrickInternalState
  , _contextMenu        :: ContextMenu
  , _advanceInstallMenu :: AdvanceInstallMenu
  , _compileGHCMenu     :: CompileGHCMenu
  , _compileHLSMenu     :: CompileHLSMenu
  , _appKeys            :: KeyBindings
  , _mode               :: Mode
  }
  --deriving Show

makeLenses ''BrickState
