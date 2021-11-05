{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module GHCup.Download.Common where


import           GHCup.Errors
import           GHCup.Types.Optics
import           GHCup.Types.JSON               ( )
import           GHCup.Prelude

import           Control.Applicative
import           Control.Monad
import           Data.ByteString                ( ByteString )
import           Data.Maybe
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           URI.ByteString

import qualified Data.Binary.Builder           as B
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L


-- | Extracts from a URI type: (https?, host, path+query, port)
uriToQuadruple :: Monad m
               => URI
               -> Excepts
                    '[UnsupportedScheme]
                    m
                    (Bool, ByteString, ByteString, Maybe Int)
uriToQuadruple URI {..} = do
  let scheme = view schemeBSL' uriScheme

  host <-
    preview (_Just % authorityHostL' % hostBSL') uriAuthority
      ?? UnsupportedScheme

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

