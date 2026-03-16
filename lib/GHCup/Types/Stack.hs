{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
#if defined(DHALL)
{-# LANGUAGE DeriveAnyClass #-}
#endif

{-|
Module      : GHCup.Types.Stack
Description : GHCup types.Stack
Copyright   : (c) Julian Ospald, 2023
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Types.Stack where

import GHCup.Types.JSON.Versions
    ()

import Control.Applicative
import Control.DeepSeq     ( NFData )
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString
import Data.Map.Strict     ( Map )
import Data.Text.Encoding
import Data.Versions
#if defined(DHALL)
import Dhall
#else
import Data.Text ( Text )
#endif

import qualified Data.Map as Map
#if defined(DHALL)
import Data.Bifunctor ( first )
import Data.Void      ( Void )
import Dhall.Core     ( Expr )
import Dhall.Src      ( Src )

import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T
import qualified Text.Megaparsec    as MP
#endif
import qualified GHC.Generics as GHC



-- copy-pasted from GHCup.Types.Dhall

#if defined(DHALL)
instance FromDhall Version where
  autoWith _ =
    Dhall.string
      { extract = extractParser version'
      }

extractParser :: forall a. MP.Parsec Void T.Text a -> Expr Src Void -> Extractor Src Void a
extractParser parser = extractParser' (MP.parse parser "FromDhall")

extractParser' :: forall a e. Show e => (T.Text -> Either e a) -> Expr Src Void -> Extractor Src Void a
extractParser' parse' expr = fromMonadic $ do
  t <- toMonadic $ extract Dhall.string expr
  first (DhallErrors . NE.singleton . ExtractError . T.pack . show) . parse' . T.pack $ t
#endif


    --------------------------------------
    --[ Stack download info copy pasta ]--
    --------------------------------------

data SetupInfo = SetupInfo
  { siSevenzExe :: Maybe DownloadInfo
  , siSevenzDll :: Maybe DownloadInfo
  , siMsys2 :: Map Text VersionedDownloadInfo
  , siGHCs :: Map Text (Map Version GHCDownloadInfo)
  , siStack :: Map Text (Map Version DownloadInfo)
  }
  deriving (Eq, GHC.Generic, Show
#if defined(DHALL)
           , FromDhall
#endif
           )

instance NFData SetupInfo

instance FromJSON SetupInfo where
  parseJSON = withObject "SetupInfo" $ \o -> do
    siSevenzExe <- o .:? "sevenzexe-info"
    siSevenzDll <- o .:? "sevenzdll-info"
    siMsys2     <- o .:? "msys2"          .!= mempty
    siGHCs      <- o .: "ghc"
    siStack     <- o .:? "stack"          .!= mempty
    pure SetupInfo {..}

instance ToJSON SetupInfo where
  toJSON (SetupInfo {..}) = object [ "sevenzexe-info" .= siSevenzExe
                                   , "sevenzdll-info" .= siSevenzDll
                                   , "msys2"          .= siMsys2
                                   , "ghc"            .= siGHCs
                                   , "stack"          .= siStack
                                   ]

-- | For the @siGHCs@ field maps are deeply merged. For all fields the values
-- from the first @SetupInfo@ win.
instance Semigroup SetupInfo where
  l <> r =
    SetupInfo
    { siSevenzExe = siSevenzExe l <|> siSevenzExe r
    , siSevenzDll = siSevenzDll l <|> siSevenzDll r
    , siMsys2     = siMsys2 l <> siMsys2 r
    , siGHCs      = Map.unionWith (<>) (siGHCs l) (siGHCs r)
    , siStack     = Map.unionWith (<>) (siStack l) (siStack r) }

instance Monoid SetupInfo where
  mempty =
    SetupInfo
    { siSevenzExe = Nothing
    , siSevenzDll = Nothing
    , siMsys2     = Map.empty
    , siGHCs      = Map.empty
    , siStack     = Map.empty
    }
  mappend = (<>)

-- | Build of the compiler distribution (e.g. standard, gmp4, tinfo6)
-- | Information for a file to download.
data DownloadInfo = DownloadInfo
  { downloadInfoUrl :: Text
    -- ^ URL or absolute file path
  , downloadInfoContentLength :: Maybe Int
  , downloadInfoSha1 :: Maybe ByteString
  , downloadInfoSha256 :: Maybe ByteString
  }
  deriving (Eq, GHC.Generic, Show
#if defined(DHALL)
           , FromDhall
#endif
           )

instance ToJSON DownloadInfo where
  toJSON (DownloadInfo {..}) = object [ "url"            .= downloadInfoUrl
                                      , "content-length" .= downloadInfoContentLength
                                      , "sha1"           .= (decodeUtf8 <$> downloadInfoSha1)
                                      , "sha256"         .= (decodeUtf8 <$> downloadInfoSha256)
                                      ]

instance NFData DownloadInfo

instance FromJSON DownloadInfo where
  parseJSON = withObject "DownloadInfo" parseDownloadInfoFromObject

-- | Parse JSON in existing object for 'DownloadInfo'
parseDownloadInfoFromObject :: Object -> Parser DownloadInfo
parseDownloadInfoFromObject o = do
  url           <- o .: "url"
  contentLength <- o .:? "content-length"
  sha1TextMay   <- o .:? "sha1"
  sha256TextMay <- o .:? "sha256"
  pure
    DownloadInfo
    { downloadInfoUrl           = url
    , downloadInfoContentLength = contentLength
    , downloadInfoSha1          = fmap encodeUtf8 sha1TextMay
    , downloadInfoSha256        = fmap encodeUtf8 sha256TextMay
    }

data VersionedDownloadInfo = VersionedDownloadInfo
  { vdiVersion :: Version
  , vdiDownloadInfo :: DownloadInfo
  }
  deriving (Eq, GHC.Generic, Show
#if defined(DHALL)
           , FromDhall
#endif
           )

instance ToJSON VersionedDownloadInfo where
  toJSON (VersionedDownloadInfo {vdiDownloadInfo = DownloadInfo{..}, ..})
    = object [ "version"        .= vdiVersion
             , "url"            .= downloadInfoUrl
             , "content-length" .= downloadInfoContentLength
             , "sha1"           .= (decodeUtf8 <$> downloadInfoSha1)
             , "sha256"         .= (decodeUtf8 <$> downloadInfoSha256)
             ]

instance NFData VersionedDownloadInfo

instance FromJSON VersionedDownloadInfo where
  parseJSON = withObject "VersionedDownloadInfo" $ \o -> do
    ver'         <- o .: "version"
    downloadInfo <- parseDownloadInfoFromObject o
    pure VersionedDownloadInfo
      { vdiVersion      = ver'
      , vdiDownloadInfo = downloadInfo
      }

data GHCDownloadInfo = GHCDownloadInfo
  { gdiConfigureOpts :: [Text]
  , gdiConfigureEnv :: Map Text Text
  , gdiDownloadInfo :: DownloadInfo
  }
  deriving (Eq, GHC.Generic, Show
#if defined(DHALL)
           , FromDhall
#endif
           )

instance NFData GHCDownloadInfo

instance ToJSON GHCDownloadInfo where
  toJSON (GHCDownloadInfo {gdiDownloadInfo = DownloadInfo {..}, ..})
    = object [ "configure-opts" .= gdiConfigureOpts
             , "configure-env"  .= gdiConfigureEnv
             , "url"            .= downloadInfoUrl
             , "content-length" .= downloadInfoContentLength
             , "sha1"           .= (decodeUtf8 <$> downloadInfoSha1)
             , "sha256"         .= (decodeUtf8 <$> downloadInfoSha256)
             ]

instance FromJSON GHCDownloadInfo where
  parseJSON = withObject "GHCDownloadInfo" $ \o -> do
    configureOpts <- o .:? "configure-opts" .!= mempty
    configureEnv  <- o .:? "configure-env"  .!= mempty
    downloadInfo  <- parseDownloadInfoFromObject o
    pure GHCDownloadInfo
      { gdiConfigureOpts = configureOpts
      , gdiConfigureEnv  = configureEnv
      , gdiDownloadInfo  = downloadInfo
      }

