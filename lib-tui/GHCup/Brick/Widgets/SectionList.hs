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

{- A general system for lists with sections

Consider this code as private. GenericSectionList should not be used directly as the FocusRing should be align with the Vector containing 
the elements, otherwise you'd be focusing on a non-existent widget with unknown result (In theory the code is safe unless you have an empty section list). 

- To build a SectionList use the safe constructor sectionList
- To access sections use the lens provider sectionL and the name of the section you'd like to access
- You can modify Brick.Widget.List.GenericList within GenericSectionList via sectionL but do not
  modify the vector length

-}


module GHCup.Brick.Widgets.SectionList where


import Brick
    ( BrickEvent(VtyEvent, MouseDown),
      EventM,
      Size(..),
      Widget(..), 
      ViewportType (Vertical),
      (<=>))
import qualified Brick
import           Brick.Widgets.Border ( hBorder)
import qualified Brick.Widgets.List as L
import           Brick.Focus (FocusRing)
import qualified Brick.Focus as F
import           Data.Function ( (&))
import Data.Maybe ( fromMaybe )
import           Data.Vector ( Vector )
import qualified GHCup.Brick.Common as Common
import           Prelude                 hiding ( appendFile )

import qualified Graphics.Vty                  as Vty
import qualified Data.Vector                   as V

import           Optics.TH (makeLensesFor)
import           Optics.State (use)
import           Optics.State.Operators ( (%=), (<%=))
import           Optics.Operators ((.~), (^.))
import           Optics.Lens (Lens', lens)

data GenericSectionList n t e
    = GenericSectionList
    { sectionListFocusRing :: FocusRing n                   -- ^ The FocusRing for all sections
    , sectionListElements :: !(Vector (L.GenericList n t e)) -- ^ A vector of brick's built-in list
    , sectionListName :: n                                   -- ^ The section list name
    }

makeLensesFor [("sectionListFocusRing", "sectionListFocusRingL"), ("sectionListElements", "sectionListElementsL"), ("sectionListName", "sectionListNameL")] ''GenericSectionList

type SectionList n e = GenericSectionList n V.Vector e


-- | Build a SectionList from nonempty list. If empty we could not defined sectionL lenses. 
sectionList :: Foldable t 
            => n                     -- The name of the section list
            -> [(n, t e)]            -- a list of tuples (section name, collection of elements)
            -> Int
            -> GenericSectionList n t e
sectionList name elements height
  = GenericSectionList
  { sectionListFocusRing = F.focusRing [section_name | (section_name, _) <- elements]
  , sectionListElements  = V.fromList [L.list section_name els height | (section_name, els) <- elements]
  , sectionListName = name
  }
-- | This lens constructor, takes a name and looks if a section has such a name.
--   Used to dispatch events to sections. It is a partial function only meant to 
--   be used with the FocusRing inside GenericSectionList
sectionL :: Eq n => n -> Lens' (GenericSectionList n t e) (L.GenericList n t e)
sectionL section_name = lens g s
    where is_section_name = (== section_name) . L.listName
          g section_list =
            let elms   = section_list ^. sectionListElementsL
                zeroth = elms V.! 0 -- TODO: This crashes for empty vectors. 
            in fromMaybe zeroth (V.find is_section_name elms)
          s gl@(GenericSectionList _ elms _) list =
            case V.findIndex is_section_name elms of
                 Nothing -> gl
                 Just i  -> let new_elms = V.update elms (V.fromList [(i, list)])
                             in gl & sectionListElementsL .~ new_elms

moveDown :: (L.Splittable t, Ord n, Foldable t) => EventM n (GenericSectionList n t e) ()
moveDown = do 
    ring <- use sectionListFocusRingL
    case F.focusGetCurrent ring of 
        Nothing -> pure ()
        Just l  -> do      -- If it is the last element, move to the first element of the next focus; else, just handle regular list event.
            current_list <- use (sectionL l)
            let current_idx = L.listSelected current_list
                list_length = current_list & length
            if current_idx == Just (list_length - 1)
                then do 
                    new_focus <- sectionListFocusRingL <%= F.focusNext
                    case F.focusGetCurrent new_focus of
                        Nothing -> pure () -- |- Optic.Zoom.zoom doesn't typecheck but Lens.Micro.Mtl.zoom does. It is re-exported by Brick
                        Just new_l -> Common.zoom (sectionL new_l) (Brick.modify L.listMoveToBeginning)
                else Common.zoom  (sectionL l) $ Brick.modify L.listMoveDown

moveUp :: (L.Splittable t, Ord n, Foldable t) => EventM n (GenericSectionList n t e) ()
moveUp = do
    ring <- use sectionListFocusRingL
    case F.focusGetCurrent ring of
        Nothing -> pure ()
        Just l  -> do  -- If it is the first element, move to the last element of the prev focus; else, just handle regular list event.
            current_list <- use (sectionL l)
            let current_idx = L.listSelected current_list
            if current_idx == Just 0
                then do 
                    new_focus <- sectionListFocusRingL <%= F.focusPrev
                    case F.focusGetCurrent new_focus of
                        Nothing -> pure ()  
                        Just new_l -> Common.zoom (sectionL new_l) (Brick.modify L.listMoveToEnd)
                else Common.zoom (sectionL l) $ Brick.modify L.listMoveUp

-- | Handle events for list cursor movement.  Events handled are:
--
-- * Up (up arrow key). If first element of section, then jump prev section
-- * Down (down arrow key). If last element of section, then jump next section
-- * Page Up (PgUp)
-- * Page Down (PgDown)
-- * Go to next section (Tab)
-- * Go to prev section (BackTab)
handleGenericListEvent :: (Foldable t, L.Splittable t, Ord n)
                       => BrickEvent n a
                       -> EventM n (GenericSectionList n t e) ()
handleGenericListEvent (VtyEvent (Vty.EvResize _ _))              = pure ()
handleGenericListEvent (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = sectionListFocusRingL %= F.focusNext
handleGenericListEvent (VtyEvent (Vty.EvKey Vty.KBackTab []))     = sectionListFocusRingL %= F.focusPrev
handleGenericListEvent (MouseDown _ Vty.BScrollDown _ _)          = moveDown
handleGenericListEvent (MouseDown _ Vty.BScrollUp _ _)            = moveUp
handleGenericListEvent (VtyEvent (Vty.EvKey Vty.KDown []))        = moveDown
handleGenericListEvent (VtyEvent (Vty.EvKey Vty.KUp []))          = moveUp
handleGenericListEvent (VtyEvent ev) = do
    ring <- use sectionListFocusRingL
    case F.focusGetCurrent ring of
        Nothing -> pure ()
        Just l  -> Common.zoom (sectionL l) $ L.handleListEvent ev
handleGenericListEvent _ = pure ()

-- This re-uses Brick.Widget.List.renderList
renderSectionList :: forall n t e . (Traversable t, Ord n, Show n, Eq n, L.Splittable t, Semigroup (t e))
                  => (Bool -> e -> Widget n)             -- ^ Rendering function of the list element, True for the selected element
                  -> Bool                                -- ^ Whether the section list has focus
                  -> GenericSectionList n t e            -- ^ The section list to render
                  -> Widget n
renderSectionList renderElem sectionFocus ge@(GenericSectionList focus elms slName) =
    Brick.Widget Brick.Greedy Brick.Greedy $ Brick.render $ Brick.viewport slName Brick.Vertical $
      V.ifoldl' (\(!accWidget) !i list ->
                      let hasFocusList = sectionIsFocused list
                          makeVisible = if hasFocusList then Brick.visibleRegion (Brick.Location (c, r)) (1, 1) else id
                          appendBorder = if i == 0 then id else (hBorder <=>)
                          newWidget = appendBorder (makeVisible $ renderInnerList hasFocusList list)
                      in accWidget <=> newWidget
                      )
      Brick.emptyWidget
      elms
 where
  -- A section is focused if the whole thing is focused, and the inner list has focus
  sectionIsFocused :: L.GenericList n t e -> Bool
  sectionIsFocused l = sectionFocus && (Just (L.listName l) == F.focusGetCurrent focus)

  renderInnerList :: Bool -> L.GenericList n t e -> Widget n
  renderInnerList hasFocus l = Brick.vLimit (length l) $ L.renderList (\b -> renderElem (b && hasFocus)) hasFocus l

  -- compute the location to focus on within the active section
  (c, r) :: (Int, Int) = case sectionListSelectedElement ge of
                           Nothing -> (0, 0)
                           Just (selElIx, _) -> (0, selElIx)


-- | Equivalent to listSelectedElement
sectionListSelectedElement :: (Eq n, L.Splittable t, Traversable t, Semigroup (t e)) => GenericSectionList n t e -> Maybe (Int, e)
sectionListSelectedElement generic_section_list = do
  current_focus <- generic_section_list ^. sectionListFocusRingL & F.focusGetCurrent 
  let current_section = generic_section_list ^. sectionL current_focus
  L.listSelectedElement current_section 
