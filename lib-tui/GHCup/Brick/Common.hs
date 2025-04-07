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
{-# LANGUAGE PatternSynonyms #-}

module GHCup.Brick.Common where

import           GHCup.Prelude ( isWindows )
import qualified GHCup.Brick.Attributes as Attributes
import           GHCup.Types ( Tool, KeyCombination (KeyCombination), KeyBindings(..) )
import Data.List (intercalate)
import           Prelude                 hiding ( appendFile )
import qualified Graphics.Vty                  as Vty
import           Optics.Lens (toLensVL)
import qualified Brick
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Border as Border
import Brick ((<+>))
import qualified Data.Text as T
import qualified Brick.Widgets.Center as Brick
import qualified Brick.Widgets.Border.Style as Border
import Optics.TH (makeLenses)

installedSign :: String
  | isWindows = "I "
  | otherwise = "✓ "

setSign :: String
  | isWindows = "IS"
  | otherwise = "✔✔"

notInstalledSign :: String
  | isWindows = "X "
  | otherwise = "✗ "

checkBoxSelectedSign :: String
  | isWindows = "Y "
  | otherwise = "✓ "


showKey :: Vty.Key -> String
showKey (Vty.KChar c) = [c]
showKey Vty.KUp = "↑"
showKey Vty.KDown = "↓"
showKey key = tail (show key)

showMod :: Vty.Modifier -> String
showMod = tail . show

-- | Given a KeyComb, produces a string widget with and user friendly text
keyToWidget :: KeyCombination -> Brick.Widget n
keyToWidget (KeyCombination key mods) = Brick.str $ intercalate "+" (showKey key : (showMod <$> mods))

-- | A section separator with max width. Looks like this:    -------- o --------
separator :: Brick.Widget n
separator = Border.hBorder <+> Brick.str " o " <+> Border.hBorder

-- | Used to create a layer on top of the main navigation widget (tutorial, info, menus...)
frontwardLayer :: T.Text -> Brick.Widget n -> Brick.Widget n
frontwardLayer layer_name =
    Brick.centerLayer
      . Brick.hLimitPercent 80
      . Brick.vLimitPercent 75
      . Brick.withBorderStyle Border.unicode
      . Border.borderWithLabel (Brick.txt layer_name)

-- | puts a cursor at the line beginning so It can be read by screen readers
enableScreenReader :: n -> Brick.Widget n -> Brick.Widget n
enableScreenReader n = Brick.putCursor n (Brick.Location (0,0))
--                     |- tip: when debugging, use Brick.showCursor instead

-- I refuse to give this a type signature.
-- | Given a lens, zoom on it. It is needed because Brick uses microlens but GHCup uses optics.
zoom l = Brick.zoom (toLensVL l)

data MenuKeyBindings = MenuKeyBindings
  { _mKbUp              :: KeyCombination
  , _mKbDown            :: KeyCombination
  , _mKbQuit            :: KeyCombination
  }
  deriving (Show)

makeLenses ''MenuKeyBindings

toMenuKeyBindings :: KeyBindings -> MenuKeyBindings
toMenuKeyBindings KeyBindings {..} = MenuKeyBindings { _mKbUp = bUp, _mKbDown = bDown, _mKbQuit = bQuit}

-- | highlights a widget (using List.listSelectedFocusedAttr)
highlighted :: Brick.Widget n -> Brick.Widget n
highlighted = Brick.withAttr L.listSelectedFocusedAttr

-- | Given a text, crates a highlighted label on focus. An amplifier can be passed
renderAslabel :: T.Text -> Bool -> Brick.Widget n
renderAslabel t focus =
  if focus
    then highlighted $ Brick.txt t
    else Brick.txt t

-- | Creates a left align column.
-- Example:       |- col2 is align dispite the length of col1
--   row1_col1         row1_col2
--   row2_col1_large   row2_col2
leftify :: Int -> Brick.Widget n -> Brick.Widget n
leftify i = Brick.hLimit i . Brick.padRight Brick.Max

-- | Creates a right align column.
-- Example:       |- col2 is align dispite the length of col1
--         row1_col1   row1_col2
--   row2_col1_large   row2_col2
rightify :: Int -> Brick.Widget n -> Brick.Widget n
rightify i = Brick.hLimit i . Brick.padLeft Brick.Max

-- | render some Text using helpMsgAttr
renderAsHelpMsg :: T.Text -> Brick.Widget n
renderAsHelpMsg = Brick.withAttr Attributes.helpMsgAttr . Brick.txt

-- | render some Text using errMsgAttr
renderAsErrMsg :: T.Text -> Brick.Widget n
renderAsErrMsg = Brick.withAttr Attributes.errMsgAttr . Brick.txt
