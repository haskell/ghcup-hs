{-# LANGUAGE CPP                   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : GHCup.Utils.URI
Description : GHCup domain specific URI utilities
Copyright   : (c) Julian Ospald, 2024
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

This module contains GHCup helpers specific to
URI handling.
-}
module GHCup.Utils.URI where

import           Data.ByteString
import           URI.ByteString hiding (parseURI)
import           System.URI.File

import qualified URI.ByteString                as URI


    -----------
    --[ URI ]--
    -----------


parseURI :: ByteString -> Either URIParseError (URIRef Absolute)
parseURI bs = case parseFile bs of
                Left _ -> case URI.parseURI strictURIParserOptions bs of
                            Right (URI { uriScheme = (Scheme "file") }) ->
#if defined(IS_WINDOWS)
                              Left (OtherError "Invalid file URI. File URIs must be absolute (start with a drive letter or UNC path) and not contain backslashes.")
#else
                              Left (OtherError "Invalid file URI. File URIs must be absolute.")
#endif
                            o -> o
                Right (FileURI (Just _) _) -> Left $ OtherError "File URIs with auth part are not supported!"
                Right (FileURI _ fp) -> Right $ URI (Scheme "file") Nothing fp (Query []) Nothing
 where
  parseFile
#if defined(IS_WINDOWS)
    = parseFileURI ExtendedWindows
#else
    = parseFileURI ExtendedPosix
#endif

