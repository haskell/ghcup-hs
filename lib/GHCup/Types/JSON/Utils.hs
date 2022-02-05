{-|
Module      : GHCup.Types.JSON.Utils
Description : Utils for TH splices
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}

module GHCup.Types.JSON.Utils where

import qualified Data.Text                     as T

removeLensFieldLabel :: String -> String
removeLensFieldLabel str' =
  maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str'
