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

import Control.Applicative ( optional )
import Data.Aeson          ( FromJSON, Key, Object, (.:) )
import Data.Aeson.Types    ( Parser )
import Data.Foldable       ( asum )
import Data.Functor        ( (<&>) )

import qualified Data.Text as T

removeLensFieldLabel :: String -> String
removeLensFieldLabel str' =
  maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str'

(.::?) :: FromJSON a => Object -> [Key] -> Parser (Maybe a)
-- asum <$> traverse (o .:?) keys
(.::?) o keys = optional $ asum (keys <&> (o .:))

(.::) :: FromJSON a => Object -> [Key] -> Parser a
(.::) o keys = asum $ keys <&> (o .:)

