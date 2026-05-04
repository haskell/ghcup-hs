{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Warnings where

import GHCup.Prelude
import GHCup.Types
import GHCup.Types.Optics

import Control.Monad.Reader           ( MonadReader )
import Data.Versions                  ( Version )
import Text.PrettyPrint.HughesPJClass ( prettyShow )

import qualified Data.Text as T


warnAboutHlsCompatibility ::
  ( MonadReader env m
  , HasLog env
  , MonadIOish m
  )
  => Maybe Version
  -> Maybe Version
  -> [Version]
  -> m ()
warnAboutHlsCompatibility currentHLS currentGHC supportedGHC = do
  case (currentGHC, currentHLS) of
    (Just gv, Just hv) | gv `notElem` supportedGHC -> do
      logWarn $
        "GHC-" <> T.pack (prettyShow gv) <> " appears to have no corresponding HLS-" <> T.pack (prettyShow hv) <> " binary." <> "\n" <>
        "Haskell IDE support may not work." <> "\n" <>
        "You can try to either: " <> "\n" <>
        "  1. Install a different HLS version (e.g. downgrade for older GHCs)" <> "\n" <>
        "  2. Install and set one of the following GHCs: " <> T.pack (prettyShow supportedGHC) <> "\n" <>
        "  3. Let GHCup compile HLS for you, e.g. run: ghcup compile hls -g " <> T.pack (prettyShow hv) <> " --ghc " <> T.pack (prettyShow gv) <>
        "     (see https://www.haskell.org/ghcup/guide/#hls for more information)"

    _ -> return ()

