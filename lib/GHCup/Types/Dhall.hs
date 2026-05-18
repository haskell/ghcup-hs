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
    , ToDhall (injectWith)
    , InterpretOptions (fieldModifier)
    , defaultInterpretOptions
    , genericToDhallWithInputNormalizer
    , extract
    , fromMonadic
    , genericAutoWith
    , toMonadic
    )
import qualified Dhall
import           GHCup.Types
import           GHCup.Prelude
import           GHCup.Types.JSON
    (verRangeToText)
import           GHCup.Types.Stack
    ()

import           Data.Versions            ( PVP, Version, pvp', version', prettyPVP, prettyVer )
import           Data.Void                ( Void )
import           GHCup.Prelude.MegaParsec ( ghcTargetVerP, versionRangeP )
import qualified Text.Megaparsec          as MP

import           Control.Applicative     ( (<|>) )
import           Data.Aeson              ( decodeStrict )
import Data.Map.Strict                ( Map )
import           Data.Bifunctor          ( first )
import           Data.Functor            ( ($>) )
import qualified Data.List.NonEmpty      as NE
import           Data.Maybe              ( fromJust )
import qualified Data.Text               as T
import           Data.Text.Encoding      as E
import           Dhall.Core              ( Expr )
import qualified Dhall.Core
import           Dhall.Parser            ( Src )
import           GHCup.Input.Parsers.URI ( parseURI' )
import           URI.ByteString          ( URI, serializeURIRef' )
import Text.PrettyPrint.HughesPJClass (prettyShow)


dhallTextLit :: T.Text -> Expr s a
dhallTextLit t = Dhall.Core.TextLit (Dhall.Core.Chunks [] t)

dhallTextLit' :: String -> Expr s a
dhallTextLit' (T.pack -> t) = Dhall.Core.TextLit (Dhall.Core.Chunks [] t)

instance FromDhall VersionRange where
  autoWith _ =
    Dhall.string
      { extract = extractParser versionRangeP
      }

instance ToDhall VersionRange where
  injectWith _ = Dhall.Encoder
    { embed = \(verRangeToText -> t) -> dhallTextLit t
    , declared = Dhall.Core.Text
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

instance ToDhall Architecture where
  injectWith _ = Dhall.Encoder
    { embed = \(prettyShow -> t) -> dhallTextLit' t
    , declared = Dhall.Core.Text
    }

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

instance ToDhall Platform where
  injectWith _ = Dhall.Encoder
    { embed = \case
                 Darwin  -> dhallTextLit' "Darwin"
                 FreeBSD -> dhallTextLit' "FreeBSD"
                 Linux d -> dhallTextLit' ("Linux_" <> show d)
                 OpenBSD -> dhallTextLit' "OpenBSD"
                 Windows -> dhallTextLit' "Windows"
    , declared = Dhall.Core.Text
    }

instance {-# OVERLAPPING #-} FromDhall (Maybe VersionRange) where
  autoWith _ =
    Dhall.string
      { extract = extractParser (MP.chunk "unknown_versioning" $> Nothing <|> (Just <$> versionRangeP))
      }

instance {-# OVERLAPPING #-} ToDhall (Maybe VersionRange) where
  injectWith _ = Dhall.Encoder
    { embed = \case
                Just t -> Dhall.embed @VersionRange Dhall.inject $ t
                Nothing ->  dhallTextLit "unknown_versioning"
    , declared = Dhall.Core.Text
    }

instance FromDhall PVP where
  autoWith _ =
    Dhall.string
      { extract = extractParser pvp'
      }

instance ToDhall PVP where
  injectWith _ = Dhall.Encoder
    { embed = \(prettyPVP -> t) -> dhallTextLit t
    , declared = Dhall.Core.Text
    }

instance {-# OVERLAPPING #-} FromDhall (Maybe Version) where
  autoWith _ =
    Dhall.string
      { extract = extractParser (MP.chunk "unknown_version" $> Nothing <|> (Just <$> version'))
      }

instance ToDhall Version where
  injectWith _ = Dhall.Encoder
    { embed = \(prettyVer -> t) -> dhallTextLit t
    , declared = Dhall.Core.Text
    }

instance {-# OVERLAPPING #-} ToDhall (Maybe Version) where
  injectWith _ = Dhall.Encoder
    { embed = \case
                Just t -> Dhall.embed @Version Dhall.inject $ t
                Nothing -> dhallTextLit "unknown_version"
    , declared = Dhall.Core.Text
    }

instance FromDhall TargetVersion where
  autoWith _ =
    Dhall.string
      { extract = extractParser ghcTargetVerP
      }

instance ToDhall TargetVersion where
  injectWith _ = Dhall.Encoder
    { embed = \(prettyShow -> t) -> dhallTextLit' t
    , declared = Dhall.Core.Text
    }

extractParser :: forall a. MP.Parsec Void T.Text a -> Expr Src Void -> Extractor Src Void a
extractParser parser = extractParser' (MP.parse parser "FromDhall")

extractParser' :: forall a e. Show e => (T.Text -> Either e a) -> Expr Src Void -> Extractor Src Void a
extractParser' parse expr = fromMonadic $ do
  t <- toMonadic $ extract Dhall.string expr
  first (DhallErrors . NE.singleton . ExtractError . T.pack . show) . parse . T.pack $ t

instance FromDhall Tag where
  autoWith _ = genericAutoWith defaultInterpretOptions

instance ToDhall Tag where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions

instance FromDhall URI where
  autoWith _ =
    Dhall.string
      { extract = extractParser' parseURI'
      }

instance ToDhall URI where
  injectWith _ = Dhall.Encoder
    { embed = \(decUTF8Safe . serializeURIRef' -> uri) -> dhallTextLit uri
    , declared = Dhall.Core.Text
    }

fieldModifierLowerCase :: Int -> T.Text -> T.Text
fieldModifierLowerCase i = (\(c, t) -> T.singleton (lower c) <> t) . fromJust . T.uncons . T.drop i

instance FromDhall InstallFileRule where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 4 }

instance ToDhall InstallFileRule where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 4 }

instance FromDhall (SymlinkSpec String) where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance ToDhall (SymlinkSpec String) where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall SymlinkInputSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance ToDhall SymlinkInputSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall RevisionSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance ToDhall RevisionSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance FromDhall ToolInfo where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance ToDhall ToolInfo where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance FromDhall ToolDescription where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance ToDhall ToolDescription where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance FromDhall EnvSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance ToDhall EnvSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall ConfigSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance ToDhall ConfigSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall MakeSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance ToDhall MakeSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall a => FromDhall (InstallationSpecGen a) where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance ToDhall a => ToDhall (InstallationSpecGen a) where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance FromDhall DownloadInfo where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance ToDhall DownloadInfo where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance (Ord k, FromDhall k, FromDhall v) => FromDhall (MapIgnoreUnknownKeys k v) where
  autoWith opts = MapIgnoreUnknownKeys <$> autoWith opts

instance (Ord k, ToDhall k, ToDhall v) => ToDhall (MapIgnoreUnknownKeys k v) where
  injectWith opts = Dhall.Encoder
    { embed = \(MapIgnoreUnknownKeys m) -> Dhall.embed Dhall.inject $ m
    , declared = Dhall.declared enc
    }
   where
    enc = Dhall.injectWith opts :: Dhall.Encoder (Map k v)

instance FromDhall VersionInfo where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance ToDhall VersionInfo where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance FromDhall Requirements where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance ToDhall Requirements where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance FromDhall GHCupInfo where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance ToDhall GHCupInfo where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance FromDhall PlatformVersionSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions

instance ToDhall PlatformVersionSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions

instance FromDhall PlatformSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions

instance ToDhall PlatformSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions

instance FromDhall ArchitectureSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions

instance ToDhall ArchitectureSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions

instance FromDhall ToolVersionSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions

instance ToDhall ToolVersionSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions

instance FromDhall GHCupDownloads where
  autoWith _ = genericAutoWith defaultInterpretOptions

instance ToDhall GHCupDownloads where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions

instance FromDhall VersionMetadata where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

instance ToDhall VersionMetadata where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 1 }

#endif
