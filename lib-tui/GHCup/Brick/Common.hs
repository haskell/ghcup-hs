{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

{-
This module contains common values used across the library. Crucially it contains two important types for the brick app:

- Name: List all resources (widgets) used by the app. see https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#resource-names
- Mode: Use to dispatch events and drawings. see: https://github.com/jtdaugherty/brick/issues/476#issuecomment-1629151920

-}

module GHCup.Brick.Common where

import           GHCup.List ( ListResult )
import           GHCup.Types ( Tool )
import           Prelude                 hiding ( appendFile )
import qualified Graphics.Vty                  as Vty
import           Optics.TH (makeLenses)
import           Optics.Lens (toLensVL)
import qualified Brick

-- | Some verbosity. A FocusRing (to loop through advance options), needs an set of resource names to be able to 
-- dtermine focus. See https://hackage.haskell.org/package/brick-2.1.1/docs/Brick-Focus.html#t:FocusRing
{- data PopUpResources 
    = UrlEditBox
    | SetCheckBox
    | IsolateEditBox
    | ForceCheckBox
    | AdditionalEditBox
    | RegularInstallButton
    | AdvanceInstallButton
    | CancellInstallButton
    deriving (Eq, Ord, Show)
-}

-- | Name data type. Uniquely identifies each widget in the TUI. 
-- some constructors might end up unused, but still is a good practise
-- to have all of them defined, just in case
data Name = AllTools        -- ^ The main list widget
          | Singular Tool   -- ^ The particular list for each tool
          | KeyInfoBox      -- ^ The text box widget with action informacion
          | TutorialBox     -- ^ The tutorial widget
--           | PopUpBox        -- ^ The whole popUp widget
--           | PopUpElement PopUpResources -- ^ each element in the popUp
          deriving (Eq, Ord, Show)

-- | Mode type. It helps to dispatch events to different handlers.
data Mode = Navigation | KeyInfo | Tutorial deriving (Eq, Show, Ord)

installedSign :: String
#if IS_WINDOWS
installedSign = "I "
#else
installedSign = "✓ "
#endif

setSign :: String
#if IS_WINDOWS
setSign = "IS"
#else
setSign = "✔✔"
#endif

notInstalledSign :: String
#if IS_WINDOWS
notInstalledSign = "X "
#else
notInstalledSign = "✗ "
#endif

showKey :: Vty.Key -> String
showKey (Vty.KChar c) = [c]
showKey Vty.KUp = "↑"
showKey Vty.KDown = "↓"
showKey key = tail (show key)

showMod :: Vty.Modifier -> String
showMod = tail . show


-- I refuse to give this a type signature. 

-- | Given a lens, zoom on it. It is needed because Brick uses microlens but GHCup uses optics.
zoom l = Brick.zoom (toLensVL l)

data BrickData = BrickData
  { _lr    :: [ListResult]
  }
  deriving Show

makeLenses ''BrickData

data BrickSettings = BrickSettings { _showAllVersions :: Bool}
  --deriving Show

makeLenses ''BrickSettings

defaultAppSettings :: BrickSettings
defaultAppSettings = BrickSettings { _showAllVersions = False}
