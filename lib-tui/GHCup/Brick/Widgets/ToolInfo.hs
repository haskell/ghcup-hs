{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module GHCup.Brick.Widgets.ToolInfo where

import GHCup.Brick.Widgets.Navigation ( BrickInternalState )
import GHCup.Prelude                  ( decUTF8Safe )
import GHCup.Types                    ( KeyBindings (..) )
import GHCup.Types.Optics

import qualified GHCup.Brick.Common as Common

import Brick
    ( Padding (Max), Widget (..), (<+>), (<=>) )
import Brick.Widgets.Center           ( center )
import Data.Maybe                     ( fromMaybe )
import Optics                         ( to, (%), (^.) )
import Prelude                        hiding ( appendFile )
import Text.PrettyPrint.HughesPJClass ( prettyShow )
import URI.ByteString                 ( serializeURIRef' )

import qualified Brick
import qualified Brick.Widgets.List as L
import qualified Data.Text          as T


mkTextBox :: [Widget Common.Name] -> Widget Common.Name
mkTextBox = Brick.hLimitPercent 70 . Brick.vBox . fmap (Brick.padRight Brick.Max)

draw :: BrickInternalState -> KeyBindings -> Widget Common.Name
draw (L.listSelectedElement -> (Just (_, (tool, (Just td, _))))) (KeyBindings {..}) =
  Common.frontwardLayer (T.pack (prettyShow tool) <> " details")
      $ Brick.vBox [
        center $ Brick.hLimitPercent 70 $
         (Brick.vBox . fmap (Brick.padRight (Brick.Pad 2))) [
            Brick.hBox [
              Brick.txt "Description: "
            ],
            Brick.hBox [
              Brick.txt "Homepage: "
            ],
            Brick.hBox [
              Brick.txt "Repository: "
            ],
            Brick.hBox [
              Brick.txt "Author: "
            ],
            Brick.hBox [
              Brick.txt "Maintainer: "
            ],
            Brick.hBox [
              Brick.txt "Contact: "
            ],
            Brick.hBox [
              Brick.txt "License: "
            ]
          ]
          <+>
         (Brick.vBox . fmap (Brick.padRight Brick.Max)) [
            Brick.hBox [
              Brick.txt (td ^. toolDescription)
            ],
            Brick.hBox [
              td ^. toolHomepage % to maybeURI
            ],
            Brick.hBox [
              td ^. toolRepository % to maybeURI
            ],
            Brick.hBox [
              Brick.txt (td ^. toolAuthor % to maybeString)
            ],
            Brick.hBox [
              Brick.txt (td ^. toolMaintainer % to maybeString)
            ],
            Brick.hBox [
              Brick.txt (td ^. toolContact % to maybeString)
            ],
            Brick.hBox [
              Brick.txt (td ^. toolLicense % to maybeString)
            ]
          ]
        ]
      <=> Brick.hBox [Brick.txt "Press " <+> Common.keyToWidget bQuit <+> Brick.txt " to return to Navigation"]
 where
  maybeString = T.pack . fromMaybe "-- Not specified --"
  maybeURI = maybe (Brick.txt "-- Not specified --")
               (\url -> let link = decUTF8Safe . serializeURIRef' $ url
                         in Brick.hyperlink link . Brick.txt $ link
               )
draw _ (KeyBindings {..}) =
  Common.frontwardLayer "Tool details"
      $ Brick.vBox [
        center $
         mkTextBox [
            Brick.hBox [
              Brick.txt "No details available"
            ]
          ]
        ] <=>
    Brick.hBox [Brick.txt "Press " <+> Common.keyToWidget bQuit <+> Brick.txt " to return to Navigation"]
