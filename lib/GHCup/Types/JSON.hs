{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : GHCup.Types.JSON
Description : GHCup JSON types/instances
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX
-}
module GHCup.Types.JSON where

import           GHCup.Types
import           GHCup.Utils.Prelude

import           Control.Applicative            ( (<|>) )
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Text.Encoding            as E
import           Data.Versions
import           Data.Word8
import           HPath
import           URI.ByteString

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T


deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } { fieldLabelModifier = removeLensFieldLabel } ''Architecture
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''LinuxDistro
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Mess
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Platform
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''SemVer
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Tool
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''VSep
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''VUnit
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''VersionInfo
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''DownloadInfo
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''GHCupInfo
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Requirements

instance ToJSON Tag where
  toJSON Latest             = String "Latest"
  toJSON Recommended        = String "Recommended"
  toJSON Prerelease         = String "Prerelease"
  toJSON Old                = String "old"
  toJSON (Base       pvp'') = String ("base-" <> prettyPVP pvp'')
  toJSON (UnknownTag x    ) = String (T.pack x)

instance FromJSON Tag where
  parseJSON = withText "Tag" $ \t -> case T.unpack t of
    "Latest"                             -> pure Latest
    "Recommended"                        -> pure Recommended
    "Prerelease"                         -> pure Prerelease
    "old"                                -> pure Old
    ('b' : 'a' : 's' : 'e' : '-' : ver') -> case pvp (T.pack ver') of
      Right x -> pure $ Base x
      Left  e -> fail . show $ e
    x -> pure (UnknownTag x)

instance ToJSON URI where
  toJSON = toJSON . decUTF8Safe . serializeURIRef'

instance FromJSON URI where
  parseJSON = withText "URL" $ \t ->
    case parseURI strictURIParserOptions (encodeUtf8 t) of
      Right x -> pure x
      Left  e -> fail . show $ e

instance ToJSON Versioning where
  toJSON = toJSON . prettyV

instance FromJSON Versioning where
  parseJSON = withText "Versioning" $ \t -> case versioning t of
    Right x -> pure x
    Left  e -> fail $ "Failure in Version (FromJSON)" <> show e

instance ToJSONKey Versioning where
  toJSONKey = toJSONKeyText $ \x -> prettyV x

instance FromJSONKey Versioning where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case versioning t of
    Right x -> pure x
    Left  e -> fail $ "Failure in Versioning (FromJSONKey)" <> show e

instance ToJSONKey (Maybe Versioning) where
  toJSONKey = toJSONKeyText $ \case
    Just x  -> prettyV x
    Nothing -> T.pack "unknown_versioning"

instance FromJSONKey (Maybe Versioning) where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    if t == T.pack "unknown_versioning" then pure Nothing else pure $ just t
   where
    just t = case versioning t of
      Right x -> pure x
      Left  e -> fail $ "Failure in (Maybe Versioning) (FromJSONKey)" <> show e

instance ToJSONKey Platform where
  toJSONKey = toJSONKeyText $ \case
    Darwin  -> T.pack "Darwin"
    FreeBSD -> T.pack "FreeBSD"
    Linux d -> T.pack ("Linux_" <> show d)

instance FromJSONKey Platform where
  fromJSONKey = FromJSONKeyTextParser $ \t -> if
    | T.pack "Darwin" == t -> pure Darwin
    | T.pack "FreeBSD" == t -> pure FreeBSD
    | T.pack "Linux_" `T.isPrefixOf` t -> case
        T.stripPrefix (T.pack "Linux_") t
      of
        Just dstr ->
          case
              (decodeStrict (E.encodeUtf8 (T.pack "\"" <> dstr <> T.pack "\"")) :: Maybe
                  LinuxDistro
              )
            of
              Just d -> pure $ Linux d
              Nothing ->
                fail
                  $  "Unexpected failure in decoding LinuxDistro: "
                  <> show dstr
        Nothing -> fail "Unexpected failure in Platform stripPrefix"
    | otherwise -> fail $ "Failure in Platform (FromJSONKey)"

instance ToJSONKey Architecture where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey Architecture where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

instance ToJSONKey (Maybe Version) where
  toJSONKey = toJSONKeyText $ \case
    Just x  -> prettyVer x
    Nothing -> T.pack "unknown_version"

instance FromJSONKey (Maybe Version) where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    if t == T.pack "unknown_version" then pure Nothing else pure $ just t
   where
    just t = case version t of
      Right x -> pure x
      Left  e -> fail $ "Failure in (Maybe Version) (FromJSONKey)" <> show e

instance ToJSON Version where
  toJSON = toJSON . prettyVer

instance FromJSON Version where
  parseJSON = withText "Version" $ \t -> case version t of
    Right x -> pure x
    Left  e -> fail $ "Failure in Version (FromJSON)" <> show e

instance ToJSONKey Version where
  toJSONKey = toJSONKeyText $ \x -> prettyVer x

instance FromJSONKey Version where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case version t of
    Right x -> pure x
    Left  e -> fail $ "Failure in Version (FromJSONKey)" <> show e

instance ToJSON PVP where
  toJSON = toJSON . prettyPVP

instance FromJSON PVP where
  parseJSON = withText "PVP" $ \t -> case pvp t of
    Right x -> pure x
    Left  e -> fail $ "Failure in PVP (FromJSON)" <> show e

instance ToJSONKey Tool where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey Tool where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

instance ToJSON (Path Rel) where
  toJSON p = case and . fmap isAscii . BS.unpack $ fp of
    True  -> toJSON . decUTF8Safe $ fp
    False -> String "/not/a/valid/path"
    where fp = toFilePath p

instance FromJSON (Path Rel) where
  parseJSON = withText "HPath Rel" $ \t -> do
    let d = encodeUtf8 t
    case parseRel d of
      Right x -> pure x
      Left  e -> fail $ "Failure in HPath Rel (FromJSON)" <> show e


instance ToJSON TarDir where
  toJSON (RealDir  p) = toJSON p
  toJSON (RegexDir r) = object ["RegexDir" .= r]

instance FromJSON TarDir where
  parseJSON v = realDir v <|> regexDir v
   where
    realDir = withText "TarDir" $ \t -> do
      fp <- parseJSON (String t)
      pure (RealDir fp)
    regexDir = withObject "TarDir" $ \o -> do
      r <- o .: "RegexDir"
      pure $ RegexDir r
