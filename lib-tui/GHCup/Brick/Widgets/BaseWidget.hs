{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module GHCup.Brick.Widgets.BaseWidget where

import qualified GHCup.Brick.Common as Common

import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..))
import qualified Brick

import Control.Monad
import Control.Monad.Reader
import Data.Some
import Optics (Lens', (^.), (%))

data HandleEventResult = CloseOverlay | CloseAllOverlays
  deriving (Eq, Show)

class BaseWidget n a | a -> n where
  draw :: a -> Widget n

  handleEvent
    :: BrickEvent n ()
    -> EventM n a (Maybe HandleEventResult)

  hasOverlay :: a -> Maybe (Some (IsSubWidget n a))
  hasOverlay _ = Nothing

  closeOverlay :: EventM n a ()
  closeOverlay = pure ()

  {-# MINIMAL (draw, handleEvent) #-}

drawBaseWidget :: (BaseWidget n a) => a -> [Widget n]
drawBaseWidget a = overlays ++ [draw a]
  where
    overlays = case hasOverlay a of
      Nothing -> []
      Just (Some (IsSubWidget accessor)) -> drawBaseWidget $ a ^. accessor

handleEventBaseWidget :: (BaseWidget n a) => BrickEvent n () -> EventM n a (Maybe HandleEventResult)
handleEventBaseWidget ev = do
  a <- Brick.get
  case hasOverlay a of
    Nothing -> handleEvent ev
    Just (Some (IsSubWidget accessor)) -> do
      res <- Common.zoom accessor $ handleEventBaseWidget ev
      case res of
        Just CloseOverlay -> do
          closeOverlay
          pure Nothing
        Just CloseAllOverlays -> do
          closeOverlay
          pure $ Just CloseAllOverlays
        Nothing -> pure Nothing

data IsSubWidget n a b where
  IsSubWidget :: (BaseWidget n b) => Lens' a b -> IsSubWidget n a b
