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

-- | This helps model a tree of widgets where each node has an instance of the BaseWidget
-- The root of the tree is the topmost widget.
-- Each BaseWidget can have zero or one of of its child widgets open at a given time
-- When a child widget is open, it will be rendered as an overlay on top of the
-- parent and the child's handleEvent will receive all the events.
-- The nesting can be arbitrarily deep.
class BaseWidget n a | a -> n where
  draw :: a -> Widget n

  -- | This will be invoked only if hasOverlay is Nothing
  -- in case of an overlay is open, the event will be passed to the child widget
  -- A widget need to handle their own events, decide whether it needs to open a
  -- child widget, or close itself.
  handleEvent :: BrickEvent n () -> EventM n a (Maybe HandleEventResult)

  -- | Indicates whether a child widget is open
  hasOverlay :: a -> Maybe (Some (IsSubWidget n a))
  hasOverlay _ = Nothing

  -- | This should modify 'a' to close any open overlay
  closeOverlay :: EventM n a ()
  closeOverlay = pure ()

  {-# MINIMAL (draw, handleEvent) #-}

-- | This is returned by the child to the parent widget and indicates whether
-- the child widget should be closed
data HandleEventResult
  -- | Closes child (the widget which returns this event)
  = CloseOverlay
  -- | Close all widgets till the root (top widget)
  | CloseAllOverlays
  deriving (Eq, Show)

-- | A widget 'b' which has 'a' as its parent, typically is structured in way
-- such that 'a' contains 'b' as a record field.
-- The `Lens' a b` allows us to `zoom`
data IsSubWidget n a b where
  IsSubWidget :: (BaseWidget n b) => Lens' a b -> IsSubWidget n a b

-- | Draw all the widgets, child is overlayed on top of parent widget
drawBaseWidget :: (BaseWidget n a) => a -> [Widget n]
drawBaseWidget a = overlays ++ [draw a]
  where
    overlays = case hasOverlay a of
      Nothing -> []
      Just (Some (IsSubWidget accessor)) -> drawBaseWidget $ a ^. accessor

-- | Pass through the event to the leaf widget (if it is open)
-- and does closing of overlay(s) by calling `closeOverlay` of parent widget(s)
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
