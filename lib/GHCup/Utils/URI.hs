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

import           GHCup.Prelude.Internal

import           Data.Bifunctor (first)
import           Data.Text                      ( Text )
import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.ByteString
import           URI.ByteString hiding (parseURI)
import           System.URI.File
import qualified Data.Text.Encoding            as E



    -----------
    --[ URI ]--
    -----------


parseURI :: ByteString -> Either URIParseError (URIRef Absolute)
parseURI = first OtherError . parseOnly parseURIP

parseURI' :: Text -> Either URIParseError (URIRef Absolute)
parseURI' = first OtherError . parseOnly parseURIP . E.encodeUtf8

parseURIP :: Parser (URIRef Absolute)
parseURIP = do
  ref <- (Right <$> parseFile) <|> (Left <$> uriParser laxURIParserOptions)
  case ref of
    Left (URI { uriScheme = (Scheme "file") }) ->
#if defined(IS_WINDOWS)
      fail "Invalid file URI. File URIs must be absolute (start with a drive letter or UNC path) and not contain backslashes."
#else
      fail "Invalid file URI. File URIs must be absolute."
#endif
    Left o -> pure o
    Right (FileURI (Just _) _) -> fail "File URIs with auth part are not supported!"
    Right (FileURI _ fp) -> pure $ URI (Scheme "file") Nothing fp (Query []) Nothing
 where
  parseFile
#if defined(IS_WINDOWS)
    = fileURIExtendedWindowsP
#else
    = fileURIExtendedPosixP
#endif
