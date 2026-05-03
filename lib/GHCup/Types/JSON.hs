{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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

import GHCup.Input.Parsers.URI
import GHCup.Prelude.JSON
import GHCup.Prelude.MegaParsec
import GHCup.Types
import GHCup.Types.JSON.MapIgnoreUnknownKeys
    ()
import GHCup.Types.JSON.Utils
import GHCup.Types.JSON.Versions
    ()
import GHCup.Types.Stack                     ( SetupInfo )

import Control.Applicative ( (<|>) )
import Control.Monad
import Data.Aeson          hiding ( Key )
import Data.Aeson.TH
import Data.Aeson.Types    hiding ( Key )
import Data.ByteString     ( ByteString )
import Data.Foldable
import Data.Maybe
import Data.Scientific (toBoundedInteger)
import Data.Text.Encoding  as E
import Data.Versions
import System.FilePath     ( hasDrive, isAbsolute, splitPath )
import Text.Casing
import URI.ByteString      hiding ( parseURI )

import qualified Data.List.NonEmpty       as NE
import qualified Data.Text                as T
import qualified Data.Text.Encoding.Error as E
import qualified Text.Megaparsec          as MP

safePath :: FilePath -> Bool
safePath fp
  | "." `notElem` splitPath fp
  , ".." `notElem` splitPath fp
  , not (hasDrive fp)
  , not (isAbsolute fp)
  = True
  | otherwise = False

checkSafePath :: MonadFail m => FilePath -> m ()
checkSafePath fp = unless (safePath fp) $ fail "'..' or '.' are not allowed"

safeFilename :: FilePath -> Bool
safeFilename fp
  | length (splitPath fp) == 1
  , safePath fp
  = True
  | otherwise = False

checkSafeFilename :: MonadFail m => FilePath -> m ()
checkSafeFilename fp = unless (safeFilename fp) $ fail "'..' or '.' are not allowed and filepath must have no path separators"

safeVersion :: TargetVersion -> Bool
safeVersion TargetVersion{..}
  | prettyVer _tvVersion `notElem` ["db", "set", "ghc", "cabal", "stack", "hls", "ghcup"]
  , T.unpack (prettyVer _tvVersion) `notElem` cabalBadNames
  , safeFilename (T.unpack $ prettyVer _tvVersion)
  , maybe True (`notElem` ["db", "set", "ghc", "cabal", "stack", "hls", "ghcup"]) _tvTarget
  , maybe True (safeFilename . T.unpack) _tvTarget
  = True
  | otherwise = False

-- This is sad, but our version parsers are too lax,
-- so we need to make sure that e.g. 'cabal-audit'
-- is not parser as cabal with version 'audit'.
-- Backwards compatibility is a b*tch.
cabalBadNames :: [String]
cabalBadNames =
  [ "plan"
  , "add"
  , "audit"
  , "bounds"
  , "bundler"
  , "cache"
  , "clean"
  , "core-inspection"
  , "deps"
  , "diff"
  , "docspec"
  , "doctest"
  , "edit"
  , "env"
  , "fmt"
  , "haddock-server"
  , "hasklint"
  , "helper"
  , "hie"
  , "hoogle"
  , "ifacy-query"
  , "progdeps"
  , "sort"
  , "store-check"
  , "store-gc"
  ]


checkSafeVersion :: MonadFail m => TargetVersion -> m ()
checkSafeVersion v' = unless (safeVersion v') $ fail "Unsafe version, try something more vanilla like '1.2.3'"

safeToolname :: Tool -> Bool
safeToolname (Tool t)
  | t `notElem` ["db", "set", "bin", "cache", "env", "config.yaml", "tmp", "trash", "logs"]
  , safeFilename t
  = True
  | otherwise = False

checkSafeToolname :: MonadFail m => Tool -> m ()
checkSafeToolname t = unless (safeToolname t) $ fail "Unsafe tool name, try something more vanilla"

instance ToJSON LinuxDistro where
  toJSON = String . T.pack . show

instance FromJSON LinuxDistro where
  parseJSON = withText "LinuxDistro" $ \t -> case T.unpack (T.toLower t) of
    "debian"       -> pure Debian
    "ubuntu"       -> pure Ubuntu
    "mint"         -> pure Mint
    "fedora"       -> pure Fedora
    "centos"       -> pure CentOS
    "redhat"       -> pure RedHat
    "alpine"       -> pure Alpine
    "amazonlinux"  -> pure AmazonLinux
    "rocky"        -> pure Rocky
    "void"         -> pure Void
    "gentoo"       -> pure Gentoo
    "exherbo"      -> pure Exherbo
    "opensuse"     -> pure OpenSUSE
    "unknownlinux" -> pure UnknownLinux
    _              -> fail "Unknown Linux distro"

deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''ProcessSpec
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''MetaMode
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Architecture
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''VSep
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''MChunk
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Platform
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Mess
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Chunk
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Release
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''SemVer
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''KeepDirs
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Downloader
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''GPGSetting
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''EnvUnion
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
  toJSON Experimental       = String "Experimental"
  toJSON (UnknownTag x    ) = String (T.pack x)

instance FromJSON Tag where
  parseJSON = withText "Tag" $ \t -> case T.unpack t of
    "Latest"                             -> pure Latest
    "Recommended"                        -> pure Recommended
    "Prerelease"                         -> pure Prerelease
    "Nightly"                            -> pure Nightly
    "LatestPrerelease"                   -> pure LatestPrerelease
    "LatestNightly"                      -> pure LatestNightly
    "Experimental"                       -> pure Experimental
    "old"                                -> pure Old
    ('b' : 'a' : 's' : 'e' : '-' : ver') -> case pvp (T.pack ver') of
      Right x -> pure $ Base x
      Left  e -> fail . show $ e
    x -> pure (UnknownTag x)

instance ToJSON Tool where
  toJSON (Tool t) = String (T.toLower . T.pack $ t)

instance FromJSON Tool where
  parseJSON = withText "Tool" $ \t -> do
    let tool = Tool . T.unpack . T.toLower $ t
    checkSafeToolname tool
    pure tool

instance ToJSON URI where
  toJSON = toJSON . E.decodeUtf8With E.lenientDecode . serializeURIRef'


instance FromJSON URI where
  parseJSON = withText "URL" $ \t ->
    case parseURI (encodeUtf8 t) of
      Right x -> pure x
      Left  e -> fail . show $ e

instance ToJSON TargetVersion where
  toJSON = toJSON . tVerToText

instance FromJSON TargetVersion where
  parseJSON = withText "TargetVersion" $ \t -> case MP.parse ghcTargetVerP "" t of
    Right x -> do
      checkSafeVersion x
      pure x
    Left  e -> fail $ "Failure in TargetVersion (FromJSON)" <> show e

instance ToJSONKey TargetVersion where
  toJSONKey = toJSONKeyText $ \x -> tVerToText x

instance FromJSONKey TargetVersion where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case MP.parse ghcTargetVerP "" t of
    Right x -> do
      checkSafeVersion x
      pure x
    Left  e -> fail $ "Failure in TargetVersion (FromJSONKey)" <> show e


instance ToJSONKey Platform where
  toJSONKey = toJSONKeyText $ \case
    Darwin  -> T.pack "Darwin"
    FreeBSD -> T.pack "FreeBSD"
    Linux d -> T.pack ("Linux_" <> show d)
    OpenBSD -> T.pack "OpenBSD"
    Windows -> T.pack "Windows"

instance FromJSONKey Platform where
  fromJSONKey = FromJSONKeyTextParser $ \t -> if
    | T.pack "Darwin" == t -> pure Darwin
    | T.pack "FreeBSD" == t -> pure FreeBSD
    | T.pack "OpenBSD" == t -> pure OpenBSD
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
  toJSONKey = toJSONKeyText $ \(Tool t) -> T.toLower . T.pack $ t

instance FromJSONKey Tool where
  fromJSONKey = FromJSONKeyTextParser $ \(T.unpack . T.toLower -> t) -> pure (Tool t)

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

deriveToJSON defaultOptions { fieldLabelModifier = mapHead lower . drop 3 } ''SymlinkSpec
deriveToJSON defaultOptions { fieldLabelModifier = mapHead lower . drop 3
                            , sumEncoding = UntaggedValue
                            } ''SymlinkInputSpec
deriveToJSON defaultOptions { fieldLabelModifier = mapHead lower . drop 4
                            , sumEncoding = UntaggedValue
                            } ''InstallFileRule
deriveJSON defaultOptions { fieldLabelModifier = mapHead lower . drop 3 } ''EnvSpec
deriveToJSON defaultOptions { fieldLabelModifier = mapHead lower . drop 3 } ''ConfigSpec
deriveToJSON defaultOptions { fieldLabelModifier = mapHead lower . drop 3 } ''MakeSpec

instance FromJSON SymlinkFileSpec where
  parseJSON = withObject "SymlinkSpec" $ \o -> do
    _slTarget <-   o .:  "target"
    _slLinkName <- o .: "linkName"
    _slPVPMajorLinks <- fromMaybe False <$> o .:? "pVPMajorLinks"
    _slSetName <- o .:? "setName"
    checkSafePath _slTarget
    checkSafeFilename _slLinkName
    forM_ _slSetName checkSafeFilename
    pure SymlinkSpec{..}

instance FromJSON SymlinkInputSpec where
  parseJSON = withObject "SymlinkInputSpec" $ \o -> do
    mTarget <- o .:?  "target"
    case mTarget of
      Just _slTarget -> do
        _slLinkName <- o .: "linkName"
        _slPVPMajorLinks <- o .:? "pVPMajorLinks" .!= False
        _slSetName <- o .:? "setName"
        checkSafePath _slTarget
        checkSafeFilename _slLinkName
        forM_ _slSetName checkSafeFilename
        pure SymlinkInputSpec{..}
      Nothing -> do
        _slTargetPattern <- o .: "targetPattern"
        _slTargetPatternIgnore <- o .:? "targetPatternIgnore" .!= []
        _slLinkName <- o .: "linkName"
        _slPVPMajorLinks <- o .:? "pVPMajorLinks" .!= False
        _slSetName <- o .:? "setName"
        forM_ _slTargetPattern checkSafePath
        forM_ _slTargetPatternIgnore checkSafePath
        checkSafeFilename _slLinkName
        forM_ _slSetName checkSafeFilename
        pure SymlinkPatternSpec{..}

instance FromJSON InstallFileRule where
  parseJSON = withObject "InstallFileRule" $ \o -> do
    installPattern   <- o .:? "installPattern"
    case installPattern of
      Just iPs -> do
        forM_ iPs checkSafePath
        pure $ InstallFilePatternRule iPs
      Nothing -> do
        installSource <- o .: "installSource"
        checkSafePath installSource
        installDest   <- o .:? "installDest"
        checkSafePath (fromMaybe "" installDest)
        pure $ InstallFileRule installSource installDest


instance FromJSON ConfigSpec where
  parseJSON = withObject "ConfigSpec" $ \o -> do
    _csConfigArgs     <- o .:  "configArgs"
    _csConfigEnv      <- o .:? "configEnv"
    _csConfigFile     <- o .:? "configFile"

    pure $ ConfigSpec {..}

instance FromJSON MakeSpec where
  parseJSON = withObject "MakeSpec" $ \o -> do
    _msMakeArgs       <- o .:  "makeArgs"
    _msMakeEnv        <- o .:? "makeEnv"

    pure $ MakeSpec {..}

instance FromJSON a => FromJSON (InstallationSpecGen a) where
  parseJSON = withObject "InstallationSpec" $ \o -> do
    _isExeRules       <- o .:? "exeRules" .!= []
    _isDataRules      <- o .:? "dataRules" .!= []
    _isExeSymLinked   <- o .:? "exeSymLinked" .!= []
    _isConfigure      <- o .:? "configure"
    _isMake           <- o .:? "make"
    _isPreserveMtimes <- o .:? "preserveMtimes" .!= False

    pure $ InstallationSpec {..}

instance ToJSON a => ToJSON (InstallationSpecGen a) where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = mapHead lower . drop 3 }


deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''Requirements
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''DownloadInfo
deriveJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''VersionInfo
deriveJSON defaultOptions { fieldLabelModifier = mapHead lower . drop 1 } ''ToolDescription

instance FromJSON InstallMetadata where
  parseJSON v = newParse v <|> legacyParse v
   where
    legacyParse o = do
      v'@DownloadInfo{..} <- parseJSON o
      case _dlInstallSpec of
        Nothing -> fail "No install metadata in legacy parser"
        Just InstallationSpec{..} -> do
          isExeSymLinked' <- forM _isExeSymLinked toSymlSpec
          pure $ InstallMetadata v' InstallationSpec{ _isExeSymLinked = isExeSymLinked', ..} Nothing

    toSymlSpec SymlinkInputSpec{..} = pure SymlinkSpec{..}
    toSymlSpec _ = fail "Can't handle SymlinkPatternSpec in legacy parser"

    newParse = do
      withObject "InstallMetadata" $ \o -> do
        _imDownloadInfo <- o .: "downloadInfo"
        _imResolvedInstallSpec <- o .: "resolvedInstallSpec"
        _imToolDescription <- o .:? "toolDescription"
        pure InstallMetadata{..}

instance FromJSON ToolInfo where
  parseJSON v = newParse v <|> legacyParse v
   where
    legacyParse o = do
      v' <- parseJSON o
      pure $ ToolInfo v' Nothing
    newParse = do
      withObject "ToolInfo" $ \o -> do
        _toolVersions <- o .: "toolVersions"
        _toolDetails <- o .: "toolDetails"
        pure ToolInfo{..}

deriveToJSON defaultOptions { fieldLabelModifier = mapHead lower . drop 3 } ''InstallMetadata
deriveToJSON defaultOptions { fieldLabelModifier = mapHead lower . drop 1 } ''ToolInfo

instance FromJSON GHCupInfo where
  parseJSON = withObject "GHCupInfo" $ \o -> do
    toolRequirements' <- o .:? "toolRequirements" .!= mempty
    metadataUpdate    <- o .:? "metadataUpdate"
    ghcupDownloads'   <- o .:  "ghcupDownloads"
    pure (GHCupInfo toolRequirements' ghcupDownloads' metadataUpdate)

deriveToJSON defaultOptions { fieldLabelModifier = removeLensFieldLabel } ''GHCupInfo

instance ToJSON NewURLSource where
  toJSON NewGHCupURL         = String "GHCupURL"
  toJSON NewStackSetupURL    = String "StackSetupURL"
  toJSON (NewGHCupInfo gi)   = object [ "ghcup-info" .= gi ]
  toJSON (NewSetupInfo si)   = object [ "setup-info" .= si ]
  toJSON (NewURI uri)        = toJSON uri
  toJSON (NewChannelAlias c) = toJSON c

instance ToJSON URLSource where
  toJSON = toJSON . fromURLSource

instance ToJSON ChannelAlias where
  toJSON = String . channelAliasText

instance FromJSON ChannelAlias where
  parseJSON = withText "ChannelAlias" $ \t ->
    let aliases = map (\c -> (channelAliasText c, c)) [minBound..maxBound]
    in case lookup t aliases of
      Just c  -> pure c
      Nothing -> fail $ "Unexpected ChannelAlias: " <> T.unpack t

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
    convert'' (Left gi)   = Left (Left gi)
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
  parseJSON v = uri v <|> url v <|> alias v <|> gi v <|> si v
   where
    alias = withText "NewURLSource" $ \t -> NewChannelAlias <$> parseJSON (String t)
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

instance FromJSON Verbosity where
  parseJSON v = new v <|> legacy v
   where
    legacy = withBool "Verbosity" $ \b -> do
      pure $ Verbosity (if b then 1 else 0)
    new = withScientific "Verbosity" $ \s -> do
      int <- maybe (fail "Verbosity integer out of bounds") pure $ toBoundedInteger s
      pure $ Verbosity int

deriveToJSON defaultOptions { unwrapUnaryRecords = True } ''Verbosity
deriveToJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' T.unpack . T.stripPrefix (T.pack "pager-") . T.pack . kebab $ str' } ''PagerConfig
deriveToJSON defaultOptions { fieldLabelModifier = drop 2 . kebab } ''KeyBindings -- move under key-bindings key
deriveJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' T.unpack . T.stripPrefix (T.pack "k-") . T.pack . kebab $ str' } ''UserKeyBindings
deriveJSON defaultOptions { fieldLabelModifier = \str' -> maybe str' T.unpack . T.stripPrefix (T.pack "u-") . T.pack . kebab $ str' } ''UserSettings
deriveToJSON defaultOptions { fieldLabelModifier = kebab } ''Settings
