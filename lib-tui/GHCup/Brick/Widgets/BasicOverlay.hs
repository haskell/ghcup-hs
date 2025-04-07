{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCup.Brick.Widgets.BasicOverlay where

import qualified GHCup.Brick.Common as Common
import GHCup.Brick.Widgets.BaseWidget
import GHCup.Types (KeyCombination (KeyCombination))

import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..))
import qualified Brick

import Control.Monad
import Control.Monad.Reader
import           Data.List (find)
import Data.Some
import qualified Graphics.Vty as Vty
import Optics (Lens', (^.), (%))
import Optics.TH (makeLenses)

data BasicOverlay n a = BasicOverlay
  { _innerWidget :: a
  , _quitKey :: [KeyCombination]
  , _overlayLayer :: Brick.Widget n -> Brick.Widget n
  }

makeLenses ''BasicOverlay

instance (BaseWidget n a) => BaseWidget n (BasicOverlay n a) where
  draw (BasicOverlay { .. }) = _overlayLayer (draw _innerWidget)
  handleEvent ev = do
    (BasicOverlay { .. }) <- Brick.get
    case ev of
      VtyEvent (Vty.EvKey key mods) ->
        case find ((==) $ KeyCombination key mods) _quitKey of
          Just _ -> pure $ Just CloseOverlay
          _ -> Common.zoom innerWidget $ handleEvent ev
      _ -> Common.zoom innerWidget $ handleEvent ev
  hasOverlay (BasicOverlay { .. }) = case hasOverlay _innerWidget of
    Nothing -> Nothing
    Just (Some (IsSubWidget accessor)) -> Just $ Some (IsSubWidget (innerWidget % accessor))
  closeOverlay = Common.zoom innerWidget closeOverlay
