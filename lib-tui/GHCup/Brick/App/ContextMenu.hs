{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCup.Brick.App.ContextMenu where

import GHCup.List ( ListResult (..))
import           GHCup.Types ( KeyBindings(..), Tool(..) )
import GHCup.Brick.Widgets.BaseWidget
import GHCup.Brick.Widgets.BasicOverlay
import qualified GHCup.Brick.App.Common as Common
import qualified GHCup.Brick.Common as Common
import GHCup.Brick.App.Tutorial (Tutorial(..))


import Brick
    ( BrickEvent(..),
      Padding(Max),
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import           Brick.Widgets.Center ( center )
import Data.Some
import qualified Data.Text                     as T
import Data.Versions (prettyVer, Version)
import           Prelude                 hiding ( appendFile )
import qualified Graphics.Vty as Vty
import Optics.State.Operators ((.=), (?=))
import Optics.TH (makeLenses)

data ContextMenu = ContextMenu
  { _menuKeys :: Common.MenuKeyBindings
  , _listResult :: ListResult
  , _overlay :: Maybe (Some (IsSubWidget Common.Name ContextMenu))
  }

makeLenses ''ContextMenu

create :: KeyBindings -> ListResult -> ContextMenu
create kb lr = ContextMenu (Common.toMenuKeyBindings kb) lr Nothing

mkTitle :: ListResult -> T.Text
mkTitle (ListResult {..}) = "Context Menu for " <> tool_str <> " " <> prettyVer lVer
  where
    tool_str =
      case lTool of
        GHC -> "GHC"
        GHCup -> "GHCup"
        Cabal -> "Cabal"
        HLS -> "HLS"
        Stack -> "Stack"

instance BaseWidget Common.Name ContextMenu where
  draw (ContextMenu {..}) = Brick.txt "ContextMenu"
