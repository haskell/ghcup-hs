{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
#if defined(DHALL)
{-# LANGUAGE DeriveAnyClass #-}
#endif

{-|
Module      : GHCup.Types
Description : GHCup types
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Types
  ( module GHCup.Types
#if defined(BRICK)
  , Key(..)
  , Modifier(..)
#endif
  , ArchiveResult(..)
  )
  where

import {-# SOURCE #-} GHCup.Query.GHCupDirs ( GHCupPath, fromGHCupPath )
import                GHCup.Types.Stack     ( SetupInfo )
import                GHCup.Types.Tar       ( ArchiveResult (..) )

import Control.DeepSeq              ( NFData, rnf )
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
#if defined(DHALL)
import Dhall ( FromDhall )
#endif
import Data.List.NonEmpty             ( NonEmpty (..) )
import Data.Map.Strict                ( Map )
import Data.Text                      ( Text )
import Data.Time.Calendar             ( Day )
import Data.Versions
import GHC.IO.Exception               ( ExitCode )
import Text.PrettyPrint.HughesPJClass ( Pretty, pPrint, text )
import URI.ByteString
#if defined(BRICK)
import Graphics.Vty ( Key (..), Modifier (..) )
#endif
import System.FilePattern

import qualified Data.ByteString.Lazy  as BL
import qualified Data.List.NonEmpty    as NE
import qualified Data.Text             as T
import qualified GHC.Generics          as GHC
import           System.FilePath       ( takeFileName, (<.>) )
import qualified System.FilePath.Posix as Posix


    -------------------------
    --[ Typeclass aliases ]--
    -------------------------



type MonadIOish m = ( MonadMask m
                    , MonadCatch m
                    , MonadIO m
                    , MonadUnliftIO m
                    , MonadFail m
                    , MonadThrow m
                    )

    --------------------
    --[ Brick compat ]--
    --------------------

#if !defined(BRICK)
data Key
  = KEsc
  | KChar Char
  | KBS
  | KEnter
  | KLeft
  | KRight
  | KUp
  | KDown
  | KUpLeft
  | KUpRight
  | KDownLeft
  | KDownRight
  | KCenter
  | KFun Int
  | KBackTab
  | KPrtScr
  | KPause
  | KIns
  | KHome
  | KPageUp
  | KDel
  | KEnd
  | KPageDown
  | KBegin
  | KMenu
  deriving (Eq, GHC.Generic, Ord, Read, Show)

data Modifier
  = MShift
  | MCtrl
  | MMeta
  | MAlt
  deriving (Eq, GHC.Generic, Ord, Read, Show)
#endif

data KeyCombination = KeyCombination
  { key :: Key
  , mods :: [Modifier]
  }
  deriving (Eq, GHC.Generic, Ord, Read, Show)



    --------------------
    --[ GHCInfo Tree ]--
    --------------------


data GHCupInfo = GHCupInfo
  { _toolRequirements :: ToolRequirements
  , _ghcupDownloads :: GHCupDownloads
  , _metadataUpdate :: Maybe URI
  }
  deriving (Eq, GHC.Generic, Show)

instance NFData GHCupInfo



    -------------------------
    --[ Requirements Tree ]--
    -------------------------


type ToolRequirements = Map Tool ToolReqVersionSpec
type ToolReqVersionSpec = Map (Maybe Version) PlatformReqSpec
type PlatformReqSpec = MapIgnoreUnknownKeys Platform PlatformReqVersionSpec
type PlatformReqVersionSpec = Map (Maybe VersionRange) Requirements


data Requirements = Requirements
  { _distroPKGs :: [Text]
  , _notes :: Text
  }
  deriving (Eq, GHC.Generic, Show)

instance NFData Requirements





    ---------------------
    --[ Download Tree ]--
    ---------------------


-- | Description of all binary and source downloads. This is a tree
-- of nested maps.
type GHCupDownloads = Map Tool ToolInfo
type ToolVersionSpec = Map TargetVersion VersionInfo
type ArchitectureSpec = MapIgnoreUnknownKeys Architecture PlatformSpec
type PlatformSpec = MapIgnoreUnknownKeys Platform PlatformVersionSpec
type PlatformVersionSpec = Map (Maybe VersionRange) DownloadInfo

data ToolInfo = ToolInfo {
    _toolVersions :: ToolVersionSpec
  , _toolDetails :: Maybe ToolDescription
  }
  deriving (Eq, GHC.Generic, Show)

instance NFData ToolInfo

data ToolDescription = ToolDescription {
    _toolHomepage    :: Maybe URI
  , _toolRepository  :: Maybe URI
  , _toolDescription :: Text
  , _toolAuthor      :: Maybe String
  , _toolMaintainer  :: Maybe String
  , _toolLicense     :: Maybe String
  , _toolContact     :: Maybe String
  }
  deriving (Eq, GHC.Generic, Show, Ord)

instance NFData ToolDescription

-- | An installable tool.
newtype Tool = Tool String
  deriving (Eq, GHC.Generic, Show, Ord
#if defined(DHALL)
           , FromDhall
#endif
           )

toolPriority :: Tool -> Maybe Int
toolPriority (Tool "ghc")   = Just 1
toolPriority (Tool "cabal") = Just 2
toolPriority (Tool "hls")   = Just 3
toolPriority (Tool "stack") = Just 4
toolPriority (Tool "ghcup") = Just 5
toolPriority (Tool _)       = Nothing


ghc, hls, stack, cabal, ghcup :: Tool
ghc = Tool "ghc"
hls = Tool "hls"
stack = Tool "stack"
cabal = Tool "cabal"
ghcup = Tool "ghcup"

instance Pretty Tool where
  pPrint (Tool t) = text t

instance NFData Tool


-- | All necessary information of a tool version, including
-- source download and per-architecture downloads.
data VersionInfo = VersionInfo
  { _viTags :: [Tag]
    -- ^ version specific tag
  , _viReleaseDay :: Maybe Day
  , _viChangeLog :: Maybe URI
  , _viSourceDL :: Maybe DownloadInfo
    -- ^ source tarball
  , _viTestDL :: Maybe DownloadInfo
    -- ^ test tarball
  , _viArch :: ArchitectureSpec
    -- ^ descend for binary downloads per arch
    -- informative messages
  , _viPreInstall :: Maybe Text
  , _viPostInstall :: Maybe Text
  , _viPostRemove :: Maybe Text
  , _viPreCompile :: Maybe Text
  }
  deriving (Eq, GHC.Generic, Show)

instance NFData VersionInfo


-- | A tag. These are currently attached to a version of a tool.
data Tag
  = Latest -- ^ the latest version of a tool (unique per tool)
  | Recommended -- ^ the recommended version of a tool (unique per tool)
  | Prerelease -- ^ denotes a prerelease version
  --   (a version should either be 'Prerelease' or
  --   'LatestPrerelease', but not both)
  | LatestPrerelease -- ^ the latest prerelease (unique per tool)
  | Nightly -- ^ denotes a nightly version
  --   (a version should either be 'Nightly' or
  --   'LatestNightly', but not both)
  | LatestNightly -- ^ the latest nightly (unique per tool)
  | Base PVP -- ^ the base version shipped with GHC
  | Old -- ^ old versions are hidden by default in TUI
  | Experimental -- ^ an experimental version/bindist
  | UnknownTag String -- ^ used for upwardscompat
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData Tag

tagToString :: Tag -> String
tagToString Recommended        = "recommended"
tagToString Latest             = "latest"
tagToString Prerelease         = "prerelease"
tagToString Nightly            = "nightly"
tagToString (Base       pvp'') = "base-" ++ T.unpack (prettyPVP pvp'')
tagToString (UnknownTag t    ) = t
tagToString LatestPrerelease   = "latest-prerelease"
tagToString LatestNightly      = "latest-nightly"
tagToString Experimental       = "experimental"
tagToString Old                = ""

instance Pretty Tag where
  pPrint Recommended        = text "recommended"
  pPrint Latest             = text "latest"
  pPrint Prerelease         = text "prerelease"
  pPrint Nightly            = text "nightly"
  pPrint (Base       pvp'') = text ("base-" ++ T.unpack (prettyPVP pvp''))
  pPrint (UnknownTag t    ) = text t
  pPrint LatestPrerelease   = text "latest-prerelease"
  pPrint LatestNightly      = text "latest-prerelease"
  pPrint Experimental       = text "experimental"
  pPrint Old                = mempty

data Architecture
  = A_64
  | A_32
  | A_PowerPC
  | A_PowerPC64
  | A_Sparc
  | A_Sparc64
  | A_ARM
  | A_ARM64
  deriving (Bounded, Enum, Eq, GHC.Generic, Ord, Show)

instance NFData Architecture

archToString :: Architecture -> String
archToString A_64        = "x86_64"
archToString A_32        = "i386"
archToString A_PowerPC   = "powerpc"
archToString A_PowerPC64 = "powerpc64"
archToString A_Sparc     = "sparc"
archToString A_Sparc64   = "sparc64"
archToString A_ARM       = "arm"
archToString A_ARM64     = "aarch64"

instance Pretty Architecture where
  pPrint = text . archToString

data Platform
  = Linux LinuxDistro
  -- ^ must exit
  | Darwin
  -- ^ must exit
  | FreeBSD
  | OpenBSD
  | Windows
  -- ^ must exit
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData Platform

platformToString :: Platform -> String
platformToString (Linux distro) = "linux-" ++ distroToString distro
platformToString Darwin         = "darwin"
platformToString FreeBSD        = "freebsd"
platformToString OpenBSD        = "openbsd"
platformToString Windows        = "windows"

instance Pretty Platform where
  pPrint = text . platformToString

data LinuxDistro = Debian
                 | Ubuntu
                 | Mint
                 | Fedora
                 | CentOS
                 | RedHat
                 | Alpine
                 | AmazonLinux
                 | Rocky
                 | Void
                 -- rolling
                 | Gentoo
                 | Exherbo
                 | OpenSUSE
                 -- not known
                 | UnknownLinux
  deriving (Bounded, Eq, Enum, GHC.Generic, Ord, Show
#if defined(DHALL)
           , FromDhall
#endif
           )

allDistros :: [LinuxDistro]
allDistros = enumFromTo minBound maxBound

instance NFData LinuxDistro

distroToString :: LinuxDistro -> String
distroToString Debian       = "debian"
distroToString Ubuntu       = "ubuntu"
distroToString Mint         = "mint"
distroToString Fedora       = "fedora"
distroToString CentOS       = "centos"
distroToString RedHat       = "redhat"
distroToString Alpine       = "alpine"
distroToString AmazonLinux  = "amazon"
distroToString Rocky        = "rocky"
distroToString Void         = "void"
distroToString Gentoo       = "gentoo"
distroToString Exherbo      = "exherbo"
distroToString OpenSUSE     = "opensuse"
distroToString UnknownLinux = "unknown"

instance Pretty LinuxDistro where
  pPrint = text . distroToString


-- TODO: rename
-- | An encapsulation of a download. This can be used
-- to download, extract and install a tool.
data DownloadInfo = DownloadInfo
  { _dlUri :: Text
  , _dlSubdir :: Maybe TarDir
  , _dlHash :: Text
  , _dlCSize :: Maybe Integer
  , _dlOutput :: Maybe FilePath
  , _dlTag :: Maybe [Tag]
  , _dlInstallSpec :: Maybe InstallationSpecInput
  }
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData DownloadInfo

data InstallMetadata = InstallMetadata {
    _imDownloadInfo :: DownloadInfo
  , _imResolvedInstallSpec :: InstallationSpecResolved
  , _imToolDescription :: Maybe ToolDescription
  }
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData InstallMetadata

defaultGHCExeSymLinked :: PlatformRequest -> TargetVersion -> [String] -> [SymlinkSpec String]
defaultGHCExeSymLinked pfreq tver binaries =
  (\b -> SymlinkSpec (b <.> exeExt)
                     (takeFileName b <> "-${PKGVER}" <.> exeExt)
                     True
                     (Just $ takeFileName b <.> exeExt)
  ) . (\b -> "bin" Posix.</> maybe "" (\t -> T.unpack t <> "-") (_tvTarget tver) <> b) <$> binaries
 where
  exeExt
    | _rPlatform pfreq == Windows = ".exe"
    | otherwise = ""


defaultGHCInstallSpec :: PlatformRequest -> TargetVersion -> InstallationSpecResolved
defaultGHCInstallSpec pfreq@(PlatformRequest { _rPlatform = Windows }) tver =
  InstallationSpec {
    _isExeRules = [InstallFilePatternRule ["bin/**"]]
  , _isDataRules = [InstallFilePatternRule ["doc/**", "lib/**", "man/**", "mingw/**"]]
  , _isConfigure = Nothing
  , _isMake = Nothing
  , _isExeSymLinked = defaultGHCExeSymLinked pfreq tver (ghcBinaries pfreq tver)
  , _isPreserveMtimes = True
  }
defaultGHCInstallSpec pfreq tver =
  InstallationSpec {
    _isExeRules = []
  , _isDataRules = []
  , _isConfigure = Just ConfigSpec {
      _csConfigArgs = ["--prefix=${PREFIX}"]
    , _csConfigEnv  = Nothing
    , _csConfigFile = Just "configure"
    }
  , _isMake = Just MakeSpec {
      _msMakeArgs   = ["DESTDIR=${TMPDIR}", "install"]
    , _msMakeEnv    = Nothing
    }
  , _isPreserveMtimes = True
  , _isExeSymLinked = defaultGHCExeSymLinked pfreq tver (ghcBinaries pfreq tver)
  }

ghcBinaries :: PlatformRequest -> TargetVersion -> [FilePath]
ghcBinaries _pfreq _tver = ["ghc", "haddock", "hpc", "hsc2hs", "ghci", "ghc-pkg", "hp2ps", "runhaskell", "runghc"]

defaultCabalInstallSpec :: PlatformRequest -> TargetVersion -> InstallationSpecResolved
defaultCabalInstallSpec pfreq _tver = InstallationSpec
  { _isExeRules = [InstallFileRule ("cabal" <.> exeExt) Nothing]
  , _isDataRules = []
  , _isExeSymLinked =
    [SymlinkSpec {
      _slTarget = "cabal" <.> exeExt
    , _slLinkName = "cabal-${PKGVER}" <.> exeExt
    , _slPVPMajorLinks = False
    , _slSetName = Just $ "cabal" <.> exeExt
    }]
  , _isConfigure = Nothing
  , _isMake = Nothing
  , _isPreserveMtimes = False
  }
 where
  exeExt
    | _rPlatform pfreq == Windows = ".exe"
    | otherwise = ""

defaultHLSInstallSpec :: PlatformRequest -> TargetVersion -> InstallationSpecResolved
defaultHLSInstallSpec _pfreq _tver =
  InstallationSpec {
    _isExeRules = []
  , _isDataRules = []
  , _isConfigure = Nothing
  , _isMake = Just MakeSpec {
      _msMakeArgs   = ["DESTDIR=${TMPDIR}", "PREFIX=${PREFIX}", "install"]
    , _msMakeEnv    = Nothing
    }
  , _isPreserveMtimes = False
  , _isExeSymLinked = [] -- we can't figure it out
  }

defaultStackInstallSpec :: PlatformRequest -> TargetVersion -> InstallationSpecResolved
defaultStackInstallSpec pfreq _tver = InstallationSpec
  { _isExeRules = [InstallFileRule ("stack" <.> exeExt) Nothing]
  , _isDataRules = []
  , _isExeSymLinked =
    [SymlinkSpec {
      _slTarget = "stack" <.> exeExt
    , _slLinkName = "stack-${PKGVER}" <.> exeExt
    , _slPVPMajorLinks = False
    , _slSetName = Just $ "stack" <.> exeExt
    }]
  , _isConfigure = Nothing
  , _isMake = Nothing
  , _isPreserveMtimes = False
  }
 where
  exeExt
    | _rPlatform pfreq == Windows = ".exe"
    | otherwise = ""

emptyInstallSpec :: InstallationSpecResolved
emptyInstallSpec = InstallationSpec {
    _isExeRules = []
  , _isDataRules = []
  , _isExeSymLinked = []
  , _isConfigure = Nothing
  , _isMake = Nothing
  , _isPreserveMtimes = False
  }

defaultToolInstallSpec :: Tool -> PlatformRequest -> TargetVersion -> Maybe InstallationSpecResolved
defaultToolInstallSpec (Tool "ghc") pfreq tver = Just $ defaultGHCInstallSpec pfreq tver
defaultToolInstallSpec (Tool "stack") pfreq tver = Just $ defaultStackInstallSpec pfreq tver
defaultToolInstallSpec (Tool "cabal") pfreq tver = Just $ defaultCabalInstallSpec pfreq tver
defaultToolInstallSpec (Tool "hls") pfreq tver = Just $ defaultHLSInstallSpec pfreq tver
defaultToolInstallSpec _ _ _ = Nothing

-- | How to union two environments. Either
-- we prefer the system environment in case
-- of duplicate keys or the spec.
data EnvUnion = PreferSystem
              | PreferSpec
              | OnlySpec
  deriving (Eq, Ord, GHC.Generic, Show
#if defined(DHALL)
           , FromDhall
#endif
           )

instance NFData EnvUnion

data EnvSpec = EnvSpec
  { _esEnv :: [(String, String)]
  , _esUnion :: EnvUnion
  }
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData EnvSpec

data ConfigSpec = ConfigSpec
  { _csConfigFile :: Maybe FilePath
  , _csConfigEnv :: Maybe EnvSpec
  , _csConfigArgs :: [String]
  }
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData ConfigSpec

data MakeSpec = MakeSpec
  { _msMakeEnv :: Maybe EnvSpec
  , _msMakeArgs :: [String]
  }
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData MakeSpec

-- | A specification of how to install the tool.
data InstallationSpecGen a = InstallationSpec
  { _isExeRules :: [InstallFileRule]
    -- ^ binaries to install
  , _isDataRules :: [InstallFileRule]
    -- ^ data files to install
  , _isExeSymLinked :: [a]
    -- ^ which binaries to symlink into ~/.ghcup/bin
  , _isConfigure :: Maybe ConfigSpec
  , _isMake :: Maybe MakeSpec
  , _isPreserveMtimes :: Bool
  }
  deriving (Eq, GHC.Generic, Ord, Show)

type InstallationSpecInput = InstallationSpecGen SymlinkInputSpec
type InstallationSpecResolved = InstallationSpecGen SymlinkFileSpec

resolveInstallationSpec :: [SymlinkFileSpec] -> InstallationSpecInput -> InstallationSpecResolved
resolveInstallationSpec symls InstallationSpec{..} =
  InstallationSpec{ _isExeSymLinked = symls, .. }

toInstallationInputSpec :: InstallationSpecResolved -> InstallationSpecInput
toInstallationInputSpec InstallationSpec{..} =
  InstallationSpec { _isExeSymLinked = toInputSpec <$> _isExeSymLinked, .. }

instance NFData a => NFData (InstallationSpecGen a)

data InstallFileRule
  = InstallFileRule
  { _ibrInstallSource :: FilePath
    -- ^ binary to install
  , _ibrInstallDest :: Maybe FilePath
    -- ^ install destination name (must not contain path separators)
  }
  | InstallFilePatternRule
  { _ibrInstallPattern :: [FilePattern]
  }
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData InstallFileRule

data SymlinkSpec a = SymlinkSpec
  { _slTarget :: a
    -- ^ where the symlink will point to
  , _slLinkName :: a
    -- ^ this is the symlink file we will create in @~/.ghcup/bin@
  , _slPVPMajorLinks :: Bool
    -- ^ also create @<tool>-x.y@ on installation
  , _slSetName :: Maybe a
    -- ^ what is the binary name if we "set" the tool
    --   if 'Nothing', then this binary will not be available unqualified
  }
  deriving (Eq, GHC.Generic, Ord, Show)

type SymlinkFileSpec = SymlinkSpec FilePath

instance NFData SymlinkFileSpec

toInputSpec :: SymlinkFileSpec -> SymlinkInputSpec
toInputSpec SymlinkSpec{..} = SymlinkInputSpec{..}

-- | Will be resolced to 'SymlinkSpec'.
data SymlinkInputSpec = SymlinkInputSpec
  { _slTarget :: FilePath
    -- ^ where the symlink will point to (this file exists already)
  , _slLinkName :: FilePath
    -- ^ this is the symlink file we will create in @~/.ghcup/bin@
  , _slPVPMajorLinks :: Bool
    -- ^ also create @<tool>-x.y@ on installation
  , _slSetName :: Maybe FilePath
    -- ^ what is the binary name if we "set" the tool
    --   if 'Nothing', then this binary will not be available unqualified
  } | SymlinkPatternSpec
  { _slTargetPattern :: [FilePattern]
    -- ^ where the symlink will point to (these files exist already)
  , _slTargetPatternIgnore :: [FilePattern]
  , _slLinkName :: FilePath
    -- ^ this is the symlink file we will create in @~/.ghcup/bin@
  , _slPVPMajorLinks :: Bool
    -- ^ also create @<tool>-x.y@ on installation
  , _slSetName :: Maybe FilePath
    -- ^ what is the binary name if we "set" the tool
    --   if 'Nothing', then this binary will not be available unqualified
  }
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData SymlinkInputSpec



    --------------
    --[ Others ]--
    --------------

data DownloadMirror = DownloadMirror
  { authority :: Authority
  , pathPrefix :: Maybe Text
  }
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData DownloadMirror

newtype DownloadMirrors
  = DM (Map Text DownloadMirror)
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData DownloadMirrors

instance NFData UserInfo
instance NFData Host
instance NFData Port
instance NFData Authority


-- | How to descend into a tar archive.
data TarDir = RealDir FilePath
            | RegexDir String     -- ^ will be compiled to regex, the first match will "win"
            deriving (Eq, Ord, GHC.Generic, Show
#if defined(DHALL)
           , FromDhall
#endif
           )

instance NFData TarDir

instance Pretty TarDir where
  pPrint (RealDir path)   = text path
  pPrint (RegexDir regex) = text regex


-- | Where to fetch GHCupDownloads from.
data URLSource
  = GHCupURL
  | StackSetupURL
  | OwnSource [Either (Either GHCupInfo SetupInfo) URI] -- ^ complete source list
  | OwnSpec (Either GHCupInfo SetupInfo)
  | AddSource [Either (Either GHCupInfo SetupInfo) URI] -- ^ merge with GHCupURL
  | SimpleList [NewURLSource]
  deriving (Eq, GHC.Generic, Show)

data NewURLSource
  = NewGHCupURL
  | NewStackSetupURL
  | NewGHCupInfo GHCupInfo
  | NewSetupInfo SetupInfo
  | NewURI URI
  | NewChannelAlias ChannelAlias
  deriving (Eq, GHC.Generic, Show)

instance NFData NewURLSource

-- | Alias for ease of URLSource selection
data ChannelAlias
  = DefaultChannel
  | StackChannel
  | CrossChannel
  | PrereleasesChannel
  | VanillaChannel
  | ThirdPartyChannel
  deriving (Bounded, Enum, Eq, GHC.Generic, Show)

channelAliasText :: ChannelAlias -> Text
channelAliasText DefaultChannel     = "default"
channelAliasText StackChannel       = "stack"
channelAliasText CrossChannel       = "cross"
channelAliasText PrereleasesChannel = "prereleases"
channelAliasText VanillaChannel     = "vanilla"
channelAliasText ThirdPartyChannel  = "3rdparty"

fromURLSource :: URLSource -> [NewURLSource]
fromURLSource GHCupURL             = [NewGHCupURL]
fromURLSource StackSetupURL        = [NewStackSetupURL]
fromURLSource (OwnSource arr)      = convert' <$> arr
fromURLSource (AddSource arr)      = NewGHCupURL:(convert' <$> arr)
fromURLSource (SimpleList arr)     = arr
fromURLSource (OwnSpec (Left gi))  = [NewGHCupInfo gi]
fromURLSource (OwnSpec (Right si)) = [NewSetupInfo si]

convert' :: Either (Either GHCupInfo SetupInfo) URI -> NewURLSource
convert' (Left (Left gi))  = NewGHCupInfo gi
convert' (Left (Right si)) = NewSetupInfo si
convert' (Right uri)       = NewURI uri

instance NFData URLSource
instance NFData ChannelAlias
instance NFData (URIRef Absolute) where
  rnf (URI !_ !_ !_ !_ !_) = ()


data MetaMode
  = Strict
  | Lax
  deriving (Eq, GHC.Generic, Read, Show)

instance NFData MetaMode

newtype Verbosity = Verbosity Int
  deriving (Eq, GHC.Generic, Show)

instance NFData Verbosity

-- If you add, remove, or rename any fields,
-- make sure to update the GHCup.OptParse.Reset module as well.
data UserSettings = UserSettings
  { uCache :: Maybe Bool
  , uMetaCache :: Maybe Integer
  , uMetaMode :: Maybe MetaMode
  , uNoVerify :: Maybe Bool
  , uVerbose :: Maybe Verbosity
  , uKeepDirs :: Maybe KeepDirs
  , uDownloader :: Maybe Downloader
  , uKeyBindings :: Maybe UserKeyBindings
  , uUrlSource :: Maybe URLSource
  , uNoNetwork :: Maybe Bool
  , uGPGSetting :: Maybe GPGSetting
  , uPlatformOverride :: Maybe PlatformRequest
  , uMirrors :: Maybe DownloadMirrors
  , uDefGHCConfOptions :: Maybe [String]
  , uPager :: Maybe PagerConfig
  , uGuessVersion :: Maybe Bool
  , uBuildWrapper :: Maybe ProcessSpec
  }
  deriving (Eq, GHC.Generic, Show)

defaultUserSettings :: UserSettings
defaultUserSettings = UserSettings Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

fromSettings :: Settings -> Maybe KeyBindings -> UserSettings
fromSettings Settings{..} Nothing =
  UserSettings {
      uCache = Just cache
    , uMetaCache = Just metaCache
    , uMetaMode = Just metaMode
    , uNoVerify = Just noVerify
    , uVerbose = Just verbose
    , uKeepDirs = Just keepDirs
    , uDownloader = Just downloader
    , uNoNetwork = Just noNetwork
    , uKeyBindings = Nothing
    , uUrlSource = Just (SimpleList urlSource)
    , uGPGSetting = Just gpgSetting
    , uPlatformOverride = platformOverride
    , uMirrors = Just mirrors
    , uDefGHCConfOptions = Just defGHCConfOptions
    , uPager = Just pager
    , uGuessVersion = Just guessVersion
    , uBuildWrapper = buildWrapper
  }
fromSettings Settings{..} (Just KeyBindings{..}) =
  let ukb = UserKeyBindings
            { kUp           = Just bUp
            , kDown         = Just bDown
            , kQuit         = Just bQuit
            , kInstall      = Just bInstall
            , kUninstall    = Just bUninstall
            , kSet          = Just bSet
            , kChangelog    = Just bChangelog
            , kShowAll      = Just bShowAllVersions
            }
  in UserSettings {
      uCache = Just cache
    , uMetaCache = Just metaCache
    , uMetaMode = Just metaMode
    , uNoVerify = Just noVerify
    , uVerbose = Just verbose
    , uKeepDirs = Just keepDirs
    , uDownloader = Just downloader
    , uNoNetwork = Just noNetwork
    , uKeyBindings = Just ukb
    , uUrlSource = Just (SimpleList urlSource)
    , uGPGSetting = Just gpgSetting
    , uPlatformOverride = platformOverride
    , uMirrors = Just mirrors
    , uDefGHCConfOptions = Just defGHCConfOptions
    , uPager = Just pager
    , uGuessVersion = Just guessVersion
    , uBuildWrapper = buildWrapper
  }

data UserKeyBindings = UserKeyBindings
  { kUp :: Maybe KeyCombination
  , kDown :: Maybe KeyCombination
  , kQuit :: Maybe KeyCombination
  , kInstall :: Maybe KeyCombination
  , kUninstall :: Maybe KeyCombination
  , kSet :: Maybe KeyCombination
  , kChangelog :: Maybe KeyCombination
  , kShowAll :: Maybe KeyCombination
  }
  deriving (Eq, GHC.Generic, Show)

data KeyBindings = KeyBindings
  { bUp :: KeyCombination
  , bDown :: KeyCombination
  , bQuit :: KeyCombination
  , bInstall :: KeyCombination
  , bUninstall :: KeyCombination
  , bSet :: KeyCombination
  , bChangelog :: KeyCombination
  , bShowAllVersions :: KeyCombination
  }
  deriving (GHC.Generic, Show)

instance NFData KeyBindings
#if !defined(BRICK)
instance NFData Key

instance NFData Modifier

#endif
instance NFData KeyCombination

defaultKeyBindings :: KeyBindings
defaultKeyBindings = KeyBindings
  { bUp              = KeyCombination { key = KUp      , mods = [] }
  , bDown            = KeyCombination { key = KDown    , mods = [] }
  , bQuit            = KeyCombination { key = KChar 'q', mods = [] }
  , bInstall         = KeyCombination { key = KChar 'i', mods = [] }
  , bUninstall       = KeyCombination { key = KChar 'u', mods = [] }
  , bSet             = KeyCombination { key = KChar 's', mods = [] }
  , bChangelog       = KeyCombination { key = KChar 'c', mods = [] }
  , bShowAllVersions = KeyCombination { key = KChar 'a', mods = [] }
  }

data AppState = AppState
  { settings :: Settings
  , dirs :: Dirs
  , keyBindings :: KeyBindings
  , ghcupInfo :: GHCupInfo
  , pfreq :: PlatformRequest
  , loggerConfig :: LoggerConfig
  }
  deriving (GHC.Generic, Show)

instance NFData AppState

fromAppState :: AppState -> LeanAppState
fromAppState AppState {..} = LeanAppState {..}

data LeanAppState = LeanAppState
  { settings :: Settings
  , dirs :: Dirs
  , keyBindings :: KeyBindings
  , pfreq :: PlatformRequest
  , loggerConfig :: LoggerConfig
  }
  deriving (GHC.Generic, Show)

instance NFData LeanAppState


data Settings = Settings
  { cache :: Bool
  , metaCache :: Integer
  , metaMode :: MetaMode
  , noVerify :: Bool
  , keepDirs :: KeepDirs
  , downloader :: Downloader
  , verbose :: Verbosity
  , urlSource :: [NewURLSource]
  , noNetwork :: Bool
  , gpgSetting :: GPGSetting
  , noColor :: Bool
    -- this also exists in LoggerConfig
  , platformOverride :: Maybe PlatformRequest
  , mirrors :: DownloadMirrors
  , defGHCConfOptions :: [String]
  , pager :: PagerConfig
  , guessVersion :: Bool
  , buildWrapper :: Maybe ProcessSpec
  }
  deriving (GHC.Generic, Show)

data ProcessSpec = ProcessSpec
  { cmd :: String
  , cmdArgs :: [String]
  }
  deriving (Eq, GHC.Generic, Show)

instance NFData ProcessSpec

data PagerConfig = PagerConfig
  { pagerList :: Bool
  , pagerCmd :: Maybe String
  }
  deriving (Eq, GHC.Generic, Show)

instance NFData PagerConfig

defaultPagerConfig :: PagerConfig
defaultPagerConfig = PagerConfig False Nothing

allPagerConfig :: String -> PagerConfig
allPagerConfig cmd = PagerConfig True (Just cmd)

defaultMetaCache :: Integer
defaultMetaCache = 300 -- 5 minutes

defaultSettings :: Settings
defaultSettings = Settings False defaultMetaCache Lax False Never Curl (Verbosity 0) [NewGHCupURL] False GPGNone False Nothing (DM mempty) [] defaultPagerConfig True Nothing

instance NFData Settings

data Dirs = Dirs
  { baseDir :: GHCupPath
  , binDir :: FilePath
  , cacheDir :: GHCupPath
  , logsDir :: GHCupPath
  , confDir :: GHCupPath
  , dbDir :: GHCupPath
  , recycleDir :: GHCupPath
    -- mainly used on windows
  , tmpDir :: GHCupPath
  , msys2Dir :: FilePath
  }
  deriving (GHC.Generic, Show)

instance NFData Dirs

data MSYS2Env
  = MSYS
  | UCRT64
  | CLANG64
  | CLANGARM64
  | CLANG32
  | MINGW64
  | MINGW32
  deriving (Eq, GHC.Generic, Ord, Read, Show)

instance NFData MSYS2Env

data KeepDirs
  = Always
  | Errors
  | Never
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData KeepDirs

data Downloader = Curl
                | Wget
#if defined(INTERNAL_DOWNLOADER)
                | Internal
#endif
  deriving (Eq, Show, Ord, GHC.Generic)

instance NFData Downloader

data GPGSetting
  = GPGStrict
  | GPGLax
  | GPGNone
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData GPGSetting

data DebugInfo = DebugInfo
  { diDirs :: Dirs
  , diArch :: Architecture
  , diPlatform :: PlatformResult
  , diChannels :: [(ChannelAlias, URI)]
  , diShimGenURL :: URI
  }
  deriving (Show)


data SetGHC
  = SetGHCOnly -- ^ unversioned 'ghc'
  | SetGHC_XY -- ^ ghc-x.y
  | SetGHC_XYZ -- ^ ghc-x.y.z
  deriving (Eq, Show)

data SetHLS
  = SetHLSOnly -- ^ unversioned 'hls'
  | SetHLS_XYZ -- ^ haskell-language-server-a.b.c~x.y.z, where a.b.c is GHC version and x.y.z is HLS version
  deriving (Eq, Show)


data PlatformResult = PlatformResult
  { _platform :: Platform
  , _distroVersion :: Maybe Versioning
  }
  deriving (Eq, GHC.Generic, Show)

instance NFData PlatformResult

platResToString :: PlatformResult -> String
platResToString PlatformResult { _platform = plat, _distroVersion = Just v' }
  = show plat <> ", " <> T.unpack (prettyV v')
platResToString PlatformResult { _platform = plat, _distroVersion = Nothing }
  = show plat

instance Pretty PlatformResult where
  pPrint = text . platResToString

data PlatformRequest = PlatformRequest
  { _rArch :: Architecture
  , _rPlatform :: Platform
  , _rVersion :: Maybe Versioning
  }
  deriving (Eq, GHC.Generic, Show)

instance NFData PlatformRequest

pfReqToString :: PlatformRequest -> String
pfReqToString (PlatformRequest arch plat ver) =
  archToString arch ++ "-" ++ platformToString plat ++ pver
 where
  pver = case ver of
           Just v' -> "-" ++ T.unpack (prettyV v')
           Nothing -> ""

instance Pretty PlatformRequest where
  pPrint = text . pfReqToString

-- | A GHC identified by the target platform triple
-- and the version.
data TargetVersion = TargetVersion
  { _tvTarget :: Maybe Text
  , _tvVersion :: Version
  }
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData TargetVersion

data GitBranch = GitBranch
  { ref :: String
  , repo :: Maybe String
  }
  deriving (Eq, Ord, Show)

mkTVer :: Version -> TargetVersion
mkTVer = TargetVersion Nothing

tVerToText :: TargetVersion -> Text
tVerToText (TargetVersion (Just t) v') = t <> "-" <> prettyVer v'
tVerToText (TargetVersion Nothing  v') = prettyVer v'

-- | Assembles a path of the form: <target-triple>-<version>
instance Pretty TargetVersion where
  pPrint = text . T.unpack . tVerToText


-- | A comparator and a version.
data VersionCmp
  = VR_gt Versioning
  | VR_gteq Versioning
  | VR_lt Versioning
  | VR_lteq Versioning
  | VR_eq Versioning
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData VersionCmp

-- | A version range. Supports && and ||, but not  arbitrary
-- combinations. This is a little simplified.
data VersionRange
  = SimpleRange (NonEmpty VersionCmp) -- And
  | OrRange (NonEmpty VersionCmp) VersionRange
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData VersionRange

instance Pretty VersionCmp where
  pPrint (VR_gt v)   = text "> " <> pPrint v
  pPrint (VR_gteq v) = text ">= " <> pPrint v
  pPrint (VR_lt v)   = text "< " <> pPrint v
  pPrint (VR_lteq v) = text "<= " <> pPrint v
  pPrint (VR_eq v)   = text "= " <> pPrint v

instance Pretty VersionRange where
  pPrint (SimpleRange xs) = foldl1 (\x y -> x <> text " && " <> y) $ NE.map pPrint xs
  pPrint (OrRange xs vr) = foldMap pPrint xs <> " || " <> pPrint vr

instance Pretty Versioning where
  pPrint = text . T.unpack . prettyV

instance Pretty Version where
  pPrint = text . T.unpack . prettyVer

instance Show (a -> b) where
  show _ = "<function>"

instance Show (IO ()) where
  show _ = "<io>"


data LogLevel
  = Warn
  | Info
  | Debug Int
  | Error
  deriving (Eq, Ord, Show)

data LoggerConfig = LoggerConfig
  { lcPrintDebugLvl :: Maybe Int
    -- ^ whether to print debug in colorOutter
  , consoleOutter :: T.Text -> IO ()
    -- ^ how to write the console output
  , fileOutter :: T.Text -> IO ()
    -- ^ how to write the file output
  , fancyColors :: Bool
  }
  deriving (Show)

instance NFData LoggerConfig where
  rnf (LoggerConfig !lcPrintDebug !_ !_ !fancyColors) = rnf (lcPrintDebug, fancyColors)

data ProcessError
  = NonZeroExit Int FilePath [String]
  | PTerminated FilePath [String]
  | PStopped FilePath [String]
  | NoSuchPid FilePath [String]
  deriving (Show)


data CapturedProcess = CapturedProcess
  { _exitCode :: ExitCode
  , _stdOut :: BL.ByteString
  , _stdErr :: BL.ByteString
  }
  deriving (Eq, Show)



data InstallDir
  = IsolateDir FilePath
  | GHCupInternal
  deriving (Eq, Show)

data InstallDirResolved
  = IsolateDirResolved FilePath
  | GHCupDir GHCupPath
  | GHCupBinDir FilePath
  deriving (Eq, Show)

fromInstallDir :: InstallDirResolved -> FilePath
fromInstallDir (IsolateDirResolved fp) = fp
fromInstallDir (GHCupDir fp)           = fromGHCupPath fp
fromInstallDir (GHCupBinDir fp)        = fp


isSafeDir :: InstallDirResolved -> Bool
isSafeDir (IsolateDirResolved _) = False
isSafeDir (GHCupDir _)           = True
isSafeDir (GHCupBinDir _)        = False

type PromptQuestion = Text

data PromptResponse
  = PromptYes
  | PromptNo
  deriving (Eq, Show)

data ToolVersion
  = GHCVersion TargetVersion
  | ToolVersion Version
  | ToolTag Tag
  | ToolDay Day
  deriving (Eq, Show)

instance Pretty ToolVersion where
  pPrint (GHCVersion v)  = pPrint v
  pPrint (ToolVersion v) = pPrint v
  pPrint (ToolTag t)     = pPrint t
  pPrint (ToolDay d)     = text (show d)



data BuildSystem
  = Hadrian
  | Make
  deriving (Eq, Show)


data VersionPattern
  = CabalVer
  | GitHashShort
  | GitHashLong
  | GitDescribe
  | GitBranchName
  | S String
  deriving (Eq, Show)

-- | Map with custom FromJSON instance which ignores unknown keys
newtype MapIgnoreUnknownKeys k v
  = MapIgnoreUnknownKeys { unMapIgnoreUnknownKeys :: Map k v }
  deriving (Eq, GHC.Generic, Show)

instance (NFData k, NFData v) => NFData (MapIgnoreUnknownKeys k v)

-- | Type representing our guessing modes when e.g. "incomplete" PVP version
-- is specified, such as @ghcup set ghc 9.12@.
data GuessMode
  = GStrict -- ^ don't guess the proper tool version
  | GLax -- ^ guess by using the metadata
  | GLaxWithInstalled -- ^ guess by using metadata and installed versions
  deriving (Eq, Show)

