{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}
#if defined(DHALL)
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
    , InterpretOptions (fieldModifier, singletonConstructors)
    , defaultInterpretOptions
    , genericToDhallWithInputNormalizer
    , extract
    , fromMonadic
    , genericAutoWith
    , toMonadic, SingletonConstructors (Bare)
    )
import qualified Dhall
import           GHCup.Types
import           GHCup.Prelude
import           GHCup.Types.JSON
    (verRangeToText)
import           GHCup.Types.Stack
    ()

import           Data.Versions            ( PVP, Version, pvp', version', prettyPVP, prettyVer, pvp )
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
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M
import qualified Dhall.Map


pattern DhallString :: T.Text -> Expr s a
pattern DhallString t = Dhall.Core.TextLit (Dhall.Core.Chunks [] t)


instance FromDhall VersionRange where
  autoWith _ =
    Dhall.string
      { extract = extractParser versionRangeP
      }

instance ToDhall VersionRange where
  injectWith _ = Dhall.Encoder
    { embed = \(verRangeToText -> t) -> DhallString t
    , declared = Dhall.Core.Text
    }

instance FromDhall Architecture where
  autoWith _ = genericAutoWith defaultInterpretOptions

instance ToDhall Architecture where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions

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
                 Darwin  -> DhallString "Darwin"
                 FreeBSD -> DhallString "FreeBSD"
                 Linux d -> DhallString ("Linux_" <> T.pack (show d))
                 OpenBSD -> DhallString "OpenBSD"
                 Windows -> DhallString "Windows"
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
                Nothing ->  DhallString "unknown_versioning"
    , declared = Dhall.Core.Text
    }

instance FromDhall PVP where
  autoWith _ =
    Dhall.string
      { extract = extractParser pvp'
      }

instance ToDhall PVP where
  injectWith _ = Dhall.Encoder
    { embed = \(prettyPVP -> t) -> DhallString t
    , declared = Dhall.Core.Text
    }

instance {-# OVERLAPPING #-} FromDhall (Maybe Version) where
  autoWith _ =
    Dhall.string
      { extract = extractParser (MP.chunk "unknown_version" $> Nothing <|> (Just <$> version'))
      }

instance ToDhall Version where
  injectWith _ = Dhall.Encoder
    { embed = \(prettyVer -> t) -> DhallString t
    , declared = Dhall.Core.Text
    }

instance {-# OVERLAPPING #-} ToDhall (Maybe Version) where
  injectWith _ = Dhall.Encoder
    { embed = \case
                Just t -> Dhall.embed @Version Dhall.inject $ t
                Nothing -> DhallString "unknown_version"
    , declared = Dhall.Core.Text
    }

instance FromDhall TargetVersion where
  autoWith _ =
    Dhall.string
      { extract = extractParser ghcTargetVerP
      }

instance ToDhall TargetVersion where
  injectWith _ = Dhall.Encoder
    { embed = \(T.pack . prettyShow -> t) -> DhallString t
    , declared = Dhall.Core.Text
    }

extractParser :: forall a. MP.Parsec Void T.Text a -> Expr Src Void -> Extractor Src Void a
extractParser parser = extractParser' (MP.parse parser "FromDhall")

extractParser' :: forall a e. Show e => (T.Text -> Either e a) -> Expr Src Void -> Extractor Src Void a
extractParser' parse expr = fromMonadic $ do
  t <- toMonadic $ extract Dhall.string expr
  first (DhallErrors . NE.singleton . ExtractError . T.pack . show) . parse . T.pack $ t

instance FromDhall Tag where
  autoWith _ = Dhall.string
    { extract = \case
        DhallString "Latest"                             -> pure Latest
        DhallString "Recommended"                        -> pure Recommended
        DhallString "Prerelease"                         -> pure Prerelease
        DhallString "Nightly"                            -> pure Nightly
        DhallString "LatestPrerelease"                   -> pure LatestPrerelease
        DhallString "LatestNightly"                      -> pure LatestNightly
        DhallString "Experimental"                       -> pure Experimental
        DhallString "old"                                -> pure Old
        e@(DhallString (T.unpack -> 'b' : 'a' : 's' : 'e' : '-' : ver')) -> case pvp (T.pack ver') of
          Right x -> pure $ Base x
          Left  _ -> Dhall.typeError (Dhall.expected Dhall.string) e
        DhallString x -> pure (UnknownTag $ T.unpack x)
        e -> Dhall.typeError (Dhall.expected Dhall.string) e
    }

instance ToDhall Tag where
  injectWith _ = Dhall.Encoder
    { embed = \case
        Latest             -> DhallString "Latest"
        Recommended        -> DhallString "Recommended"
        Prerelease         -> DhallString "Prerelease"
        Nightly            -> DhallString "Nightly"
        Old                -> DhallString "old"
        (Base       pvp'') -> DhallString ("base-" <> prettyPVP pvp'')
        LatestPrerelease   -> DhallString "LatestPrerelease"
        LatestNightly      -> DhallString "LatestNightly"
        Experimental       -> DhallString "Experimental"
        (UnknownTag x    ) -> DhallString (T.pack x)
    , declared = Dhall.Core.Text
    }

instance FromDhall URI where
  autoWith _ =
    Dhall.string
      { extract = extractParser' parseURI'
      }

instance ToDhall URI where
  injectWith _ = Dhall.Encoder
    { embed = \(decUTF8Safe . serializeURIRef' -> uri) -> DhallString uri
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

instance FromDhall Rev where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { singletonConstructors = Bare }

instance ToDhall Rev where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { singletonConstructors = Bare }

instance FromDhall RevisionSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { singletonConstructors = Bare }

instance ToDhall RevisionSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { singletonConstructors = Bare }

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
    { singletonConstructors = Bare }

instance ToDhall PlatformVersionSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { singletonConstructors = Bare }

instance FromDhall PlatformSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { singletonConstructors = Bare }

instance ToDhall PlatformSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { singletonConstructors = Bare }

instance FromDhall ArchitectureSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { singletonConstructors = Bare }

instance ToDhall ArchitectureSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { singletonConstructors = Bare }

instance FromDhall ToolVersionSpec where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { singletonConstructors = Bare }

instance ToDhall ToolVersionSpec where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { singletonConstructors = Bare }

instance FromDhall GHCupDownloads where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { singletonConstructors = Bare }

instance ToDhall GHCupDownloads where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { singletonConstructors = Bare }

instance FromDhall VersionMetadata where
  autoWith _ = genericAutoWith defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

instance ToDhall VersionMetadata where
  injectWith = genericToDhallWithInputNormalizer defaultInterpretOptions
    { fieldModifier = fieldModifierLowerCase 3 }

#endif
