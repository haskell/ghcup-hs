{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCup.Brick.App.Navigation where

import GHCup.Brick.Actions
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.App.Common as Common
import GHCup.Brick.Widgets.BaseWidget
import qualified GHCup.Brick.Widgets.SectionList as SectionList

import GHCup.List ( ListResult(..) )
import GHCup.Types (KeyCombination (KeyCombination))

import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..))
import qualified Brick

import Control.Monad
import Control.Monad.Reader
import Data.Some
import Data.Vector ( Vector )
import qualified Graphics.Vty as Vty
import Optics (Lens', (^.), (%))
import Optics.TH (makeLenses)


data Navigation = Navigation
  { _sectionList :: NavigationList
  , _listResult    :: [ListResult]
  , _showAllVersions :: Bool
  }

makeLenses ''Navigation

-- | How to create a navigation widget
create :: Common.Name                         -- The name of the section list
       -> [(Common.Name, Vector ListResult)]  -- a list of tuples (section name, collection of elements)
       -> Int                                 -- The height of each item in a list. Commonly 1
       -> Navigation
create name elements height =
  let
  in Navigation
    { _sectionList = SectionList.sectionList name elements height
    , _listResult = []
    , _showAllVersions = False
    }

instance BaseWidget Common.Name Navigation where
  draw _ = Brick.txt "Navigation"

  handleEvent ev = do
    pure Nothing
