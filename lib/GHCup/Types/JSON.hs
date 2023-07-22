{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : GHCup.Types.JSON
Description : GHCup JSON types/instances
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Types.JSON where

import           GHCup.Types
import           GHCup.Types.JSON.Utils
import           GHCup.Prelude.MegaParsec

import           Control.Applicative            ( (<|>) )
import           Data.Aeson              hiding (Key)
import           Data.Aeson.TH
import           Data.Aeson.Types        hiding (Key)
import           Data.Bifunctor
import           Data.ByteString                ( ByteString )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Text.Encoding            as E
import           Data.Versions
import           Data.Void
import           URI.ByteString
import           Text.Casing

import qualified Autodocodec                   as Codec
import qualified Data.List.NonEmpty            as NE
import qualified Data.Scientific               as Scientific
import qualified Data.Text                     as T
import qualified Data.Text.Encoding.Error      as E
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC


deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''MetaMode
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Architecture
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''LinuxDistro
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''VSep
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''VUnit
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''MChunk
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Platform
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Mess
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''SemVer
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Tool
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''GlobalTool
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''KeepDirs
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Downloader
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''GPGSetting
deriveJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' T.unpack . T.stripPrefix (T.pack "r-") . T.pack . kebab . tail $ str' } ''PlatformRequest

instance Codec.HasCodec Tag where
  codec = Codec.named "Tag" $ Codec.bimapCodec
    (\t -> case T.unpack t of
      "Latest"                             -> pure Latest
      "Recommended"                        -> pure Recommended
      "Prerelease"                         -> pure Prerelease
      "Nightly"                            -> pure Nightly
      "LatestPrerelease"                   -> pure LatestPrerelease
      "LatestNightly"                      -> pure LatestNightly
      "old"                                -> pure Old
      ('b' : 'a' : 's' : 'e' : '-' : ver') -> case pvp (T.pack ver') of
        Right x -> pure $ Base x
        Left  e -> Left . show $ e
      x -> pure (UnknownTag x))
    (\tag -> case tag of
      Latest             -> "Latest"
      Recommended        -> "Recommended"
      Prerelease         -> "Prerelease"
      Nightly            -> "Nightly"
      Old                -> "old"
      (Base       pvp'') -> ("base-" <> prettyPVP pvp'')
      LatestPrerelease   -> "LatestPrerelease"
      LatestNightly      -> "LatestNightly"
      (UnknownTag x    ) -> (T.pack x))
    Codec.textCodec

deriving via Codec.Autodocodec Tag instance FromJSON Tag

deriving via Codec.Autodocodec Tag instance ToJSON Tag

instance Codec.HasCodec URI where
  codec = Codec.named "URI" $ Codec.bimapCodec
    (first show . parseURI strictURIParserOptions . encodeUtf8)
    (E.decodeUtf8With E.lenientDecode . serializeURIRef')
    Codec.textCodec

deriving via Codec.Autodocodec URI instance FromJSON URI

deriving via Codec.Autodocodec URI instance ToJSON URI

instance ToJSON GHCTargetVersion where
  toJSON = toJSON . tVerToText

instance FromJSON GHCTargetVersion where
  parseJSON = withText "GHCTargetVersion" $ \t -> case MP.parse ghcTargetVerP "" t of
    Right x -> pure x
    Left  e -> fail $ "Failure in GHCTargetVersion (FromJSON)" <> show e

instance ToJSONKey GHCTargetVersion where
  toJSONKey = toJSONKeyText $ \x -> tVerToText x

instance FromJSONKey GHCTargetVersion where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case MP.parse ghcTargetVerP "" t of
    Right x -> pure x
    Left  e -> fail $ "Failure in GHCTargetVersion (FromJSONKey)" <> show e

instance ToJSON Versioning where
  toJSON = toJSON . prettyV

instance FromJSON Versioning where
  parseJSON = withText "Versioning" $ \t -> case versioning t of
    Right x -> pure x
    Left  e -> fail $ "Failure in GHCTargetVersion (FromJSON)" <> show e

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
    if t == T.pack "unknown_versioning" then pure Nothing else just t
   where
    just t = case versioning t of
      Right x -> pure $ Just x
      Left  e -> fail $ "Failure in (Maybe Versioning) (FromJSONKey)" <> show e

instance ToJSONKey Platform where
  toJSONKey = toJSONKeyText $ \case
    Darwin  -> T.pack "Darwin"
    FreeBSD -> T.pack "FreeBSD"
    Linux d -> T.pack ("Linux_" <> show d)
    Windows -> T.pack "Windows"

instance FromJSONKey Platform where
  fromJSONKey = FromJSONKeyTextParser $ \t -> if
    | T.pack "Darwin" == t -> pure Darwin
    | T.pack "FreeBSD" == t -> pure FreeBSD
    | T.pack "Windows" == t -> pure Windows
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
    | otherwise -> fail "Failure in Platform (FromJSONKey)"

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
    if t == T.pack "unknown_version" then pure Nothing else just t
   where
    just t = case version t of
      Right x -> pure $ Just x
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

instance ToJSONKey GlobalTool where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey GlobalTool where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

instance Codec.HasCodec TarDir where
  codec = Codec.named "TarDir" $ Codec.dimapCodec
    (either (RealDir . T.unpack) RegexDir)
    (\tarDir -> case tarDir of
      RealDir realDir -> Left $ T.pack realDir
      RegexDir regexDir -> Right regexDir)
    $ Codec.disjointEitherCodec Codec.textCodec (Codec.object "RegexDir" $ Codec.requiredField' "RegexDir" Codec..= id)

deriving via Codec.Autodocodec TarDir instance FromJSON TarDir

deriving via Codec.Autodocodec TarDir instance ToJSON TarDir

instance ToJSON VersionCmp where
  toJSON = String . versionCmpToText

instance FromJSON VersionCmp where
  parseJSON = withText "VersionCmp" $ \t -> do
    case MP.parse versionCmpP "" t of
      Right r -> pure r
      Left  e -> fail (MP.errorBundlePretty e)

instance ToJSON ByteString where
  toJSON = toJSON . E.decodeUtf8With E.lenientDecode

instance FromJSON ByteString where
  parseJSON = withText "ByteString" $ \t -> pure $ E.encodeUtf8 t

versionCmpToText :: VersionCmp -> T.Text
versionCmpToText (VR_gt   ver') = "> " <> prettyV ver'
versionCmpToText (VR_gteq ver') = ">= " <> prettyV ver'
versionCmpToText (VR_lt   ver') = "< " <> prettyV ver'
versionCmpToText (VR_lteq ver') = "<= " <> prettyV ver'
versionCmpToText (VR_eq   ver') = "== " <> prettyV ver'

versionCmpP :: MP.Parsec Void T.Text VersionCmp
versionCmpP =
  fmap VR_gt (MP.try $ MPC.space *> MP.chunk ">" *> MPC.space *> versioningEnd)
    <|> fmap
          VR_gteq
          (MP.try $ MPC.space *> MP.chunk ">=" *> MPC.space *> versioningEnd)
    <|> fmap
          VR_lt
          (MP.try $ MPC.space *> MP.chunk "<" *> MPC.space *> versioningEnd)
    <|> fmap
          VR_lteq
          (MP.try $ MPC.space *> MP.chunk "<=" *> MPC.space *> versioningEnd)
    <|> fmap
          VR_eq
          (MP.try $ MPC.space *> MP.chunk "==" *> MPC.space *> versioningEnd)
    <|> fmap
          VR_eq
          (MP.try $ MPC.space *> versioningEnd)

instance ToJSON VersionRange where
  toJSON = String . verRangeToText

verRangeToText :: VersionRange -> T.Text
verRangeToText  (SimpleRange cmps) =
  let inner = foldr1 (\x y -> x <> " && " <> y)
                     (versionCmpToText <$> NE.toList cmps)
  in  "( " <> inner <> " )"
verRangeToText (OrRange cmps range) =
  let left  = verRangeToText (SimpleRange cmps)
      right = verRangeToText range
  in  left <> " || " <> right

instance FromJSON VersionRange where
  parseJSON = withText "VersionRange" $ \t -> do
    case MP.parse versionRangeP "" t of
      Right r -> pure r
      Left  e -> fail (MP.errorBundlePretty e)

versionRangeP :: MP.Parsec Void T.Text VersionRange
versionRangeP = go <* MP.eof
 where
  go =
    MP.try orParse
      <|> MP.try (fmap SimpleRange andParse)
      <|> fmap (SimpleRange . pure) versionCmpP

  orParse :: MP.Parsec Void T.Text VersionRange
  orParse =
    (\a o -> OrRange a o)
      <$> (MP.try andParse <|> fmap pure versionCmpP)
      <*> (MPC.space *> MP.chunk "||" *> MPC.space *> go)

  andParse :: MP.Parsec Void T.Text (NonEmpty VersionCmp)
  andParse =
    fmap (\h t -> h :| t)
         (MPC.space *> MP.chunk "(" *> MPC.space *> versionCmpP)
      <*> MP.try (MP.many (MPC.space *> MP.chunk "&&" *> MPC.space *> versionCmpP))
      <*  MPC.space
      <*  MP.chunk ")"
      <*  MPC.space

versioningEnd :: MP.Parsec Void T.Text Versioning
versioningEnd =
  MP.try (verP (MP.chunk " " <|> MP.chunk ")" <|> MP.chunk "&&") <* MPC.space)
    <|> versioning'

instance ToJSONKey (Maybe VersionRange) where
  toJSONKey = toJSONKeyText $ \case
    Just x -> verRangeToText x
    Nothing -> "unknown_versioning"

instance FromJSONKey (Maybe VersionRange)  where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    if t == T.pack "unknown_versioning" then pure Nothing else just t
   where
    just t = case MP.parse versionRangeP "" t of
      Right x -> pure $ Just x
      Left  e -> fail $ "Failure in (Maybe VersionRange) (FromJSONKey)" <> MP.errorBundlePretty e



instance Codec.HasCodec Requirements where
  codec = Codec.named "Requirements" . Codec.object "Requirements" $ Requirements
    <$> Codec.requiredField' "distroPKGs" Codec..= _distroPKGs
    <*> Codec.requiredField' "notes" Codec..= _notes

deriving via Codec.Autodocodec Requirements instance FromJSON Requirements

deriving via Codec.Autodocodec Requirements instance ToJSON Requirements

instance Codec.HasCodec DownloadInfo where
  codec = Codec.named "DownloadInfo" . Codec.object "DownloadInfo" $ DownloadInfo
    <$> Codec.requiredField' "uri" Codec..= _dlUri
    <*> Codec.optionalField' "subdir" Codec..= _dlSubdir
    <*> Codec.requiredField' "hash" Codec..= _dlHash
    <*> Codec.optionalFieldWith' "cSize" integerCodec Codec..= _dlCSize
    <*> Codec.optionalField' "output" Codec..= _dlOutput
    where
      integerCodec = Codec.bimapCodec
        (first (const "invalid integer") . Scientific.floatingOrInteger @Double)
        (flip Scientific.scientific 0)
        Codec.scientificCodec

deriving via Codec.Autodocodec DownloadInfo instance FromJSON DownloadInfo

deriving via Codec.Autodocodec DownloadInfo instance ToJSON DownloadInfo

instance Codec.HasCodec VersionInfo where
  codec = Codec.named "VersionInfo" . Codec.object "VersionInfo" $ VersionInfo
    <$> Codec.requiredField' "tags" Codec..= _viTags
    <*> Codec.optionalField' "releaseDay" Codec..= _viReleaseDay
    <*> Codec.optionalField' "changeLog" Codec..= _viChangeLog
    <*> Codec.optionalField' "sourceDL" Codec..= _viSourceDL
    <*> Codec.optionalField' "testDL" Codec..= _viTestDL
    <*> Codec.requiredField' "arch" Codec..= _viArch
    <*> Codec.optionalField' "postInstall" Codec..= _viPostInstall
    <*> Codec.optionalField' "postRemove" Codec..= _viPostRemove
    <*> Codec.optionalField' "preCompile" Codec..= _viPreCompile

deriving via Codec.Autodocodec VersionInfo instance FromJSON VersionInfo

deriving via Codec.Autodocodec VersionInfo instance ToJSON VersionInfo

instance Codec.HasCodec GHCupInfo where
  codec = Codec.named "GHCupInfo" . Codec.object "GHCupInfo" $ GHCupInfo
    <$> Codec.requiredField' "toolRequirements" Codec..= _toolRequirements
    <*> Codec.requiredField' "ghcupDownloads" Codec..= _ghcupDownloads
    <*> Codec.requiredField' "globalTools" Codec..= _globalTools

deriving via Codec.Autodocodec GHCupInfo instance FromJSON GHCupInfo

deriving via Codec.Autodocodec GHCupInfo instance ToJSON GHCupInfo

deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''URLSource
deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Key
deriveJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' T.unpack . T.stripPrefix (T.pack "k-") . T.pack . kebab $ str' } ''UserKeyBindings
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel, unwrapUnaryRecords = True } ''Port
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel, unwrapUnaryRecords = True } ''Host
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''UserInfo
deriveJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' (T.unpack . T.toLower) . T.stripPrefix (T.pack "authority") . T.pack $ str' } ''Authority
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''DownloadMirror
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''DownloadMirrors
deriveToJSON defaultOptions { fieldLabelModifier = kebab } ''Settings
deriveToJSON defaultOptions { fieldLabelModifier = drop 2 . kebab } ''KeyBindings -- move under key-bindings key

instance FromJSON URLSource where
  parseJSON v =
        parseGHCupURL v
    <|> parseOwnSourceLegacy v
    <|> parseOwnSourceNew1 v
    <|> parseOwnSourceNew2 v
    <|> parseOwnSpec v
    <|> legacyParseAddSource v
    <|> newParseAddSource v
   where
    parseOwnSourceLegacy = withObject "URLSource" $ \o -> do
      r :: URI <- o .: "OwnSource"
      pure (OwnSource [Right r])
    parseOwnSourceNew1 = withObject "URLSource" $ \o -> do
      r :: [URI] <- o .: "OwnSource"
      pure (OwnSource (fmap Right r))
    parseOwnSourceNew2 = withObject "URLSource" $ \o -> do
      r :: [Either GHCupInfo URI] <- o .: "OwnSource"
      pure (OwnSource r)
    parseOwnSpec = withObject "URLSource" $ \o -> do
      r :: GHCupInfo <- o .: "OwnSpec"
      pure (OwnSpec r)
    parseGHCupURL = withObject "URLSource" $ \o -> do
      _ :: [Value] <- o .: "GHCupURL"
      pure GHCupURL
    legacyParseAddSource = withObject "URLSource" $ \o -> do
      r :: Either GHCupInfo URI <- o .: "AddSource"
      pure (AddSource [r])
    newParseAddSource = withObject "URLSource" $ \o -> do
      r :: [Either GHCupInfo URI] <- o .: "AddSource"
      pure (AddSource r)

deriveJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' T.unpack . T.stripPrefix (T.pack "u-") . T.pack . kebab $ str' } ''UserSettings
