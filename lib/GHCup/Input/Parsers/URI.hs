{-# LANGUAGE CPP                   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : GHCup.Input.Parsers.URI
Description : GHCup domain specific URI utilities
Copyright   : (c) Julian Ospald, 2024
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

This module contains GHCup helpers specific to
URI handling.
-}
module GHCup.Input.Parsers.URI where

import GHCup.Types.Optics
import GHCup.Errors

import           Data.Bifunctor (first)
import           Data.Text                      ( Text )
import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.ByteString
import           URI.ByteString hiding (parseURI)
import           System.URI.File
import Data.Variant.Excepts (Excepts, throwE)
import Optics (view, preview, (%), _Just)

import qualified Data.Text.Encoding            as E
import qualified Data.Binary.Builder           as B
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L



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

-- | Extracts from a URI type: (https?, host, path+query, port)
uriToQuadruple :: Monad m
               => URI
               -> Excepts
                    '[UnsupportedScheme]
                    m
                    (Bool, ByteString, ByteString, Maybe Int)
uriToQuadruple URI {..} = do
  let scheme = view schemeBSL' uriScheme

  host <- maybe (throwE UnsupportedScheme) pure $
    preview (_Just % authorityHostL' % hostBSL') uriAuthority

  https <- if
    | scheme == "https" -> pure True
    | scheme == "http"  -> pure False
    | otherwise         -> throwE UnsupportedScheme

  let queryBS =
        BS.intercalate "&"
          . fmap (\(x, y) -> encodeQuery x <> "=" <> encodeQuery y)
          $ queryPairs uriQuery
      port =
        preview (_Just % authorityPortL' % _Just % portNumberL') uriAuthority
      fullpath = if BS.null queryBS then uriPath else uriPath <> "?" <> queryBS
  pure (https, host, fullpath, port)
  where encodeQuery = L.toStrict . B.toLazyByteString . urlEncodeQuery
