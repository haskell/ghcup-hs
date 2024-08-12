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
Portability : portable
-}
module GHCup.Types.JSON where

import           GHCup.Types
import           GHCup.Types.Stack (SetupInfo)
import           GHCup.Types.JSON.Utils
import           GHCup.Types.JSON.Versions ()
import           GHCup.Prelude.MegaParsec
import           GHCup.Utils.URI

import           Control.Applicative            ( (<|>) )
import           Data.Aeson              hiding (Key)
import           Data.Aeson.TH
import           Data.Aeson.Types        hiding (Key)
import           Data.ByteString                ( ByteString )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Maybe
import           Data.Text.Encoding            as E
import           Data.Foldable
import           Data.Versions
import           Data.Void
import           URI.ByteString hiding (parseURI)
import           Text.Casing

import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Data.Text.Encoding.Error      as E
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC


deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''MetaMode
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Architecture
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''LinuxDistro
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''VSep
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''MChunk
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Platform
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Mess
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Chunk
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Release
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''SemVer
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Tool
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''KeepDirs
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Downloader
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''GPGSetting
deriveJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' T.unpack . T.stripPrefix (T.pack "r-") . T.pack . kebab . tail $ str' } ''PlatformRequest

instance ToJSON Tag where
  toJSON Latest             = String "Latest"
  toJSON Recommended        = String "Recommended"
  toJSON Prerelease         = String "Prerelease"
  toJSON Nightly            = String "Nightly"
  toJSON Old                = String "old"
  toJSON (Base       pvp'') = String ("base-" <> prettyPVP pvp'')
  toJSON LatestPrerelease   = String "LatestPrerelease"
  toJSON LatestNightly      = String "LatestNightly"
  toJSON (UnknownTag x    ) = String (T.pack x)

instance FromJSON Tag where
  parseJSON = withText "Tag" $ \t -> case T.unpack t of
    "Latest"                             -> pure Latest
    "Recommended"                        -> pure Recommended
    "Prerelease"                         -> pure Prerelease
    "Nightly"                            -> pure Nightly
    "LatestPrerelease"                   -> pure LatestPrerelease
    "LatestNightly"                      -> pure LatestNightly
    "old"                                -> pure Old
    ('b' : 'a' : 's' : 'e' : '-' : ver') -> case pvp (T.pack ver') of
      Right x -> pure $ Base x
      Left  e -> fail . show $ e
    x -> pure (UnknownTag x)

instance ToJSON URI where
  toJSON = toJSON . E.decodeUtf8With E.lenientDecode . serializeURIRef'


instance FromJSON URI where
  parseJSON = withText "URL" $ \t ->
    case parseURI (encodeUtf8 t) of
      Right x -> pure x
      Left  e -> fail . show $ e

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

instance ToJSONKey Tool where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey Tool where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

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


deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Requirements
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''DownloadInfo
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''VersionInfo

instance FromJSON GHCupInfo where
  parseJSON = withObject "GHCupInfo" $ \o -> do
    toolRequirements' <- o .:? "toolRequirements"
    metadataUpdate    <- o .:? "metadataUpdate"
    ghcupDownloads'   <- o .:  "ghcupDownloads"
    pure (GHCupInfo (fromMaybe mempty toolRequirements') ghcupDownloads' metadataUpdate)

deriveToJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''GHCupInfo

instance ToJSON NewURLSource where
  toJSON NewGHCupURL       = String "GHCupURL"
  toJSON NewStackSetupURL  = String "StackSetupURL"
  toJSON (NewGHCupInfo gi) = object [ "ghcup-info" .= gi ]
  toJSON (NewSetupInfo si) = object [ "setup-info" .= si ]
  toJSON (NewURI uri)      = toJSON uri

instance ToJSON URLSource where
  toJSON = toJSON . fromURLSource

deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Key
deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Modifier
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel, unwrapUnaryRecords = True } ''Port
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel, unwrapUnaryRecords = True } ''Host
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''UserInfo
deriveJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' (T.unpack . T.toLower) . T.stripPrefix (T.pack "authority") . T.pack $ str' } ''Authority
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''DownloadMirror
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''DownloadMirrors

instance FromJSON URLSource where
  parseJSON v =
        parseGHCupURL v
    <|> parseStackURL v
    <|> parseOwnSourceLegacy v
    <|> parseOwnSourceNew1 v
    <|> parseOwnSourceNew2 v
    <|> parseOwnSpec v
    <|> legacyParseAddSource v
    <|> newParseAddSource v
    -- new since Stack SetupInfo
    <|> parseOwnSpecNew v
    <|> parseOwnSourceNew3 v
    <|> newParseAddSource2 v
    -- more lenient versions
    <|> parseOwnSpecLenient v
    <|> parseOwnSourceLenient v
    <|> parseAddSourceLenient v
    -- simplified list
    <|> parseNewUrlSource v
    <|> parseNewUrlSource' v
   where
    convert'' :: Either GHCupInfo URI -> Either (Either GHCupInfo SetupInfo) URI
    convert'' (Left gi)  = Left (Left gi)
    convert'' (Right uri) = Right uri

    parseOwnSourceLegacy = withObject "URLSource" $ \o -> do
      r :: URI <- o .: "OwnSource"
      pure (OwnSource [Right r])
    parseOwnSourceNew1 = withObject "URLSource" $ \o -> do
      r :: [URI] <- o .: "OwnSource"
      pure (OwnSource (fmap Right r))
    parseOwnSourceNew2 = withObject "URLSource" $ \o -> do
      r :: [Either GHCupInfo URI] <- o .: "OwnSource"
      pure (OwnSource (convert'' <$> r))
    parseOwnSpec = withObject "URLSource" $ \o -> do
      r :: GHCupInfo <- o .: "OwnSpec"
      pure (OwnSpec $ Left r)
    parseGHCupURL = withObject "URLSource" $ \o -> do
      _ :: [Value] <- o .: "GHCupURL"
      pure GHCupURL
    parseStackURL = withObject "URLSource" $ \o -> do
      _ :: [Value] <- o .: "StackSetupURL"
      pure StackSetupURL
    legacyParseAddSource = withObject "URLSource" $ \o -> do
      r :: Either GHCupInfo URI <- o .: "AddSource"
      pure (AddSource [convert'' r])
    newParseAddSource = withObject "URLSource" $ \o -> do
      r :: [Either GHCupInfo URI] <- o .: "AddSource"
      pure (AddSource (convert'' <$> r))

    -- new since Stack SetupInfo
    parseOwnSpecNew = withObject "URLSource" $ \o -> do
      r :: Either GHCupInfo SetupInfo <- o .: "OwnSpec"
      pure (OwnSpec r)
    parseOwnSourceNew3 = withObject "URLSource" $ \o -> do
      r :: [Either (Either GHCupInfo SetupInfo) URI] <- o .: "OwnSource"
      pure (OwnSource r)
    newParseAddSource2 = withObject "URLSource" $ \o -> do
      r :: [Either (Either GHCupInfo SetupInfo) URI] <- o .: "AddSource"
      pure (AddSource r)

    -- more lenient versions
    parseOwnSpecLenient = withObject "URLSource" $ \o -> do
      spec :: Object <- o .: "OwnSpec"
      OwnSpec <$> lenientInfoParser spec
    parseOwnSourceLenient = withObject "URLSource" $ \o -> do
      mown :: Array <- o .: "OwnSource"
      OwnSource . toList <$> mapM lenientInfoUriParser mown
    parseAddSourceLenient = withObject "URLSource" $ \o -> do
      madd :: Array <- o .: "AddSource"
      AddSource . toList <$> mapM lenientInfoUriParser madd

    -- simplified
    parseNewUrlSource = withArray "URLSource" $ \a -> do
      SimpleList . toList <$> mapM parseJSON a
    parseNewUrlSource' v' = SimpleList .(:[]) <$> parseJSON v'


lenientInfoUriParser :: Value -> Parser (Either (Either GHCupInfo SetupInfo) URI)
lenientInfoUriParser (Object o) = Left <$> lenientInfoParser o
lenientInfoUriParser v@(String _) = Right <$> parseJSON v
lenientInfoUriParser _ = fail "Unexpected json in lenientInfoUriParser"


lenientInfoParser :: Object -> Parser (Either GHCupInfo SetupInfo)
lenientInfoParser o = do
  setup_info :: Maybe Object <- o .:? "setup-info"
  case setup_info of
    Nothing -> do
      r <- parseJSON (Object o)
      pure $ Left r
    Just setup_info' -> do
      r <- parseJSON (Object setup_info')
      pure $ Right r

instance FromJSON NewURLSource where
  parseJSON v = uri v <|> url v <|> gi v <|> si v
   where
    uri = withText "NewURLSource" $ \t -> NewURI <$> parseJSON (String t)
    url = withText "NewURLSource" $ \t -> case T.unpack t of
                                            "GHCupURL" -> pure NewGHCupURL
                                            "StackSetupURL" -> pure NewStackSetupURL
                                            t' -> fail $ "Unexpected text value in NewURLSource: " <> t'
    gi = withObject "NewURLSource" $ \o -> do
       ginfo :: GHCupInfo <- o .: "ghcup-info"
       pure $ NewGHCupInfo ginfo

    si = withObject "NewURLSource" $ \o -> do
       sinfo :: SetupInfo <- o .: "setup-info"
       pure $ NewSetupInfo sinfo


instance FromJSON KeyCombination where
  parseJSON v = proper v <|> simple v
   where
    simple = withObject "KeyCombination" $ \o -> do
      k <- parseJSON (Object o)
      pure (KeyCombination k [])
    proper = withObject "KeyCombination" $ \o -> do
      k <- o .: "Key"
      m <- o .: "Mods"
      pure $ KeyCombination k m

instance ToJSON KeyCombination where
  toJSON (KeyCombination k m) = object ["Key" .= k, "Mods" .= m]

instance FromJSON PagerConfig where
  parseJSON v = p1 v <|> p2 v <|> p3 v
   where
    p2 = withBool "PagerConfig" $ \b -> pure $ PagerConfig b Nothing
    p3 = withText "PagerConfig" $ \t -> pure $ allPagerConfig (T.unpack t)
    p1 = withObject "PagerConfig" $ \o -> do
       list <- o .:  "list"
       cmd  <- o .:? "cmd"
       pure $ PagerConfig list cmd


deriveToJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' T.unpack . T.stripPrefix (T.pack "pager-") . T.pack . kebab $ str' } ''PagerConfig
deriveToJSON defaultOptions { fieldLabelModifier = drop 2 . kebab } ''KeyBindings -- move under key-bindings key
deriveJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' T.unpack . T.stripPrefix (T.pack "k-") . T.pack . kebab $ str' } ''UserKeyBindings
deriveJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' T.unpack . T.stripPrefix (T.pack "u-") . T.pack . kebab $ str' } ''UserSettings
deriveToJSON defaultOptions { fieldLabelModifier = kebab } ''Settings
