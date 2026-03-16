{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}
#if defined(DHALL)
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
#endif

{-|
Module      : GHCup.Types.Dhall
Description : GHCup Dhall instances
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Types.Dhall () where

#if defined(DHALL)

import           Dhall
    ( DhallErrors (..)
    , ExtractError (..)
    , Extractor
    , FromDhall (autoWith)
    , InterpretOptions (fieldModifier)
    , defaultInterpretOptions
    , extract
    , fromMonadic
    , genericAutoWith
    , toMonadic
    )
import qualified Dhall
import           GHCup.Types
import           GHCup.Types.JSON
    ()
import           GHCup.Types.Stack
    ()

import           Data.Versions            ( PVP, Version, pvp', version' )
import           Data.Void                ( Void )
import           GHCup.Prelude.MegaParsec ( ghcTargetVerP, versionRangeP )
import qualified Text.Megaparsec          as MP

import           Control.Applicative     ( (<|>) )
import           Data.Aeson              ( decodeStrict )
import           Data.Bifunctor          ( first )
import           Data.Functor            ( ($>) )
import qualified Data.List.NonEmpty      as NE
import           Data.Maybe              ( fromJust )
import qualified Data.Text               as T
import           Data.Text.Encoding      as E
import           Dhall.Core              ( Expr )
import           Dhall.Parser            ( Src )
import           GHCup.Input.Parsers.URI ( parseURI' )
import           GHCup.Prelude.JSON      ( lower )
import           URI.ByteString          ( URI )


instance FromDhall VersionRange where
  autoWith _ =
    Dhall.string
      { extract = extractParser versionRangeP
      }

instance {-# OVERLAPPING #-} FromDhall Architecture where
  autoWith _ =
    Dhall.string
      { extract = extractParser' parseArch
      }
   where
    parseArch t
      | t == "x86_64"    = Right A_64
      | t == "i386"      = Right A_32
      | t == "powerpc"   = Right A_PowerPC
      | t == "powerpc64" = Right A_PowerPC64
      | t == "sparc"     = Right A_Sparc
      | t == "sparc64"   = Right A_Sparc64
      | t == "arm"       = Right A_ARM
      | t == "aarch64"   = Right A_ARM64
      | otherwise        = Left ("Unknown arch: " <> t)

instance {-# OVERLAPPING #-} FromDhall Platform where
  autoWith _ =
    Dhall.string
      { extract = extractParser' parsePlatform
      }
   where
    parsePlatform t
      | "Darwin"  == t = Right Darwin
      | "FreeBSD" == t = Right FreeBSD
      | "OpenBSD" == t = Right OpenBSD
      | "Windows" == t = Right Windows
      | "Linux_" `T.isPrefixOf` t = case
          T.stripPrefix (T.pack "Linux_") t
        of
          Just dstr ->
            case
                (decodeStrict (E.encodeUtf8 (T.pack "\"" <> dstr <> T.pack "\"")) :: Maybe
                    LinuxDistro
                )
              of
                Just d -> Right $ Linux d
                Nothing ->
                  Left
                    $  "Unexpected failure in decoding LinuxDistro: "
                    <> show dstr
          Nothing -> Left "Unexpected failure in Platform stripPrefix"
      | otherwise = Left "Failure in Platform (FromJSONKey)"


instance {-# OVERLAPPING #-} FromDhall (Maybe VersionRange) where
  autoWith _ =
    Dhall.string
      { extract = extractParser (MP.chunk "unknown_versioning" $> Nothing <|> (Just <$> versionRangeP))
      }

instance FromDhall PVP where
  autoWith _ =
    Dhall.string
      { extract = extractParser pvp'
      }

instance {-# OVERLAPPING #-} FromDhall (Maybe Version) where
  autoWith _ =
    Dhall.string
      { extract = extractParser (MP.chunk "unknown_version" $> Nothing <|> (Just <$> version'))
      }

instance FromDhall GHCTargetVersion where
  autoWith _ =
    Dhall.string
      { extract = extractParser ghcTargetVerP
      }

extractParser :: forall a. MP.Parsec Void T.Text a -> Expr Src Void -> Extractor Src Void a
extractParser parser = extractParser' (MP.parse parser "FromDhall")

extractParser' :: forall a e. Show e => (T.Text -> Either e a) -> Expr Src Void -> Extractor Src Void a
extractParser' parse expr = fromMonadic $ do
  t <- toMonadic $ extract Dhall.string expr
  first (DhallErrors . NE.singleton . ExtractError . T.pack . show) . parse . T.pack $ t

instance FromDhall Tag where
  autoWith _ = genericAutoWith defaultInterpretOptions

instance FromDhall URI where
  autoWith _ =
    Dhall.string
      { extract = extractParser' parseURI'
      }

fieldModifierLowerCase :: Int -> T.Text -> T.Text
fieldModifierLowerCase i = (\(c, t) -> T.singleton (lower c) <> t) . fromJust . T.uncons . T.drop i

instance FromDhall InstallFileRule where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 4 }

instance FromDhall (SymlinkSpec String) where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall SymlinkInputSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall ToolInfo where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance FromDhall ToolDescription where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance FromDhall EnvSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall ConfigSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall MakeSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall a => FromDhall (InstallationSpecGen a) where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall DownloadInfo where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance (Ord k, FromDhall k, FromDhall v) => FromDhall (MapIgnoreUnknownKeys k v) where
  autoWith opts = MapIgnoreUnknownKeys <$> autoWith opts

instance FromDhall VersionInfo where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance FromDhall Requirements where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance FromDhall GHCupInfo where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

#endif
