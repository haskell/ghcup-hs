{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module GHCup.Types.JSON where

import           GHCup.Types
import           GHCup.Utils.Prelude
import           GHCup.Utils.String.QQ

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Text.Encoding             ( decodeUtf8 )
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
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Tag
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''DownloadInfo


instance ToJSON URI where
  toJSON = toJSON . decodeUtf8 . serializeURIRef'

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
    Nothing -> T.pack "unknown_version"

instance FromJSONKey (Maybe Versioning) where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    if t == T.pack "unknown_version" then pure Nothing else pure $ just t
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

instance ToJSONKey Tool where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey Tool where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

instance ToJSON (Path Rel) where
  toJSON p = case and . fmap isAscii . BS.unpack $ fp of
    True  -> toJSON . E.decodeUtf8 $ fp
    False -> String [s|/not/a/valid/path|]
    where fp = toFilePath p

instance FromJSON (Path Rel) where
  parseJSON = withText "HPath Rel" $ \t -> do
    let d = encodeUtf8 t
    case parseRel d of
      Right x -> pure x
      Left  e -> fail $ "Failure in HPath Rel (FromJSON)" <> show e
