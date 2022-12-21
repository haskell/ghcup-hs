{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
#endif
  )
  where

import {-# SOURCE #-} GHCup.Utils.Dirs          ( fromGHCupPath, GHCupPath )

import           Control.DeepSeq                ( NFData, rnf )
import           Data.Map.Strict                ( Map )
import           Data.List.NonEmpty             ( NonEmpty (..) )
import           Data.Text                      ( Text )
import           Data.Versions
import           GHC.IO.Exception               ( ExitCode )
import           Optics                         ( makeLenses )
import           Text.PrettyPrint.HughesPJClass (Pretty, pPrint, text, (<+>))
import           URI.ByteString
#if defined(BRICK)
import           Graphics.Vty                   ( Key(..) )
#endif

import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified GHC.Generics                  as GHC


#if !defined(BRICK)
data Key = KEsc  | KChar Char | KBS | KEnter
         | KLeft | KRight | KUp | KDown
         | KUpLeft | KUpRight | KDownLeft | KDownRight | KCenter
         | KFun Int | KBackTab | KPrtScr | KPause | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KBegin | KMenu
    deriving (Eq,Show,Read,Ord,GHC.Generic)
#endif


    --------------------
    --[ GHCInfo Tree ]--
    --------------------


data GHCupInfo = GHCupInfo
  { _toolRequirements :: ToolRequirements
  , _ghcupDownloads   :: GHCupDownloads
  , _globalTools      :: Map GlobalTool DownloadInfo
  }
  deriving (Show, GHC.Generic)

instance NFData GHCupInfo



    -------------------------
    --[ Requirements Tree ]--
    -------------------------


type ToolRequirements = Map Tool ToolReqVersionSpec
type ToolReqVersionSpec = Map (Maybe Version) PlatformReqSpec
type PlatformReqSpec = Map Platform PlatformReqVersionSpec
type PlatformReqVersionSpec = Map (Maybe VersionRange) Requirements


data Requirements = Requirements
  { _distroPKGs :: [Text]
  , _notes      :: Text
  }
  deriving (Show, GHC.Generic)

instance NFData Requirements





    ---------------------
    --[ Download Tree ]--
    ---------------------


-- | Description of all binary and source downloads. This is a tree
-- of nested maps.
type GHCupDownloads = Map Tool ToolVersionSpec
type ToolVersionSpec = Map Version VersionInfo
type ArchitectureSpec = Map Architecture PlatformSpec
type PlatformSpec = Map Platform PlatformVersionSpec
type PlatformVersionSpec = Map (Maybe VersionRange) DownloadInfo


-- | An installable tool.
data Tool = GHC
          | Cabal
          | GHCup
          | HLS
          | Stack
  deriving (Eq, GHC.Generic, Ord, Show, Enum, Bounded)

instance Pretty Tool where
  pPrint GHC = text "ghc"
  pPrint Cabal = text "cabal"
  pPrint GHCup = text "ghcup"
  pPrint HLS = text "hls"
  pPrint Stack = text "stack"

instance NFData Tool

data GlobalTool = ShimGen
  deriving (Eq, GHC.Generic, Ord, Show, Enum, Bounded)

instance NFData GlobalTool


-- | All necessary information of a tool version, including
-- source download and per-architecture downloads.
data VersionInfo = VersionInfo
  { _viTags        :: [Tag]              -- ^ version specific tag
  , _viChangeLog   :: Maybe URI
  , _viSourceDL    :: Maybe DownloadInfo -- ^ source tarball
  , _viArch        :: ArchitectureSpec   -- ^ descend for binary downloads per arch
  -- informative messages
  , _viPostInstall :: Maybe Text
  , _viPostRemove  :: Maybe Text
  , _viPreCompile  :: Maybe Text
  }
  deriving (Eq, GHC.Generic, Show)

instance NFData VersionInfo


-- | A tag. These are currently attached to a version of a tool.
data Tag = Latest
         | Recommended
         | Prerelease
         | Base PVP
         | Old                -- ^ old versions are hidden by default in TUI
         | UnknownTag String  -- ^ used for upwardscompat
         deriving (Ord, Eq, GHC.Generic, Show) -- FIXME: manual JSON instance

instance NFData Tag

tagToString :: Tag -> String
tagToString Recommended        = "recommended"
tagToString Latest             = "latest"
tagToString Prerelease         = "prerelease"
tagToString (Base       pvp'') = "base-" ++ T.unpack (prettyPVP pvp'')
tagToString (UnknownTag t    ) = t
tagToString Old                = ""

instance Pretty Tag where
  pPrint Recommended        = text "recommended"
  pPrint Latest             = text "latest"
  pPrint Prerelease         = text "prerelease"
  pPrint (Base       pvp'') = text ("base-" ++ T.unpack (prettyPVP pvp''))
  pPrint (UnknownTag t    ) = text t
  pPrint Old                = mempty

data Architecture = A_64
                  | A_32
                  | A_PowerPC
                  | A_PowerPC64
                  | A_Sparc
                  | A_Sparc64
                  | A_ARM
                  | A_ARM64
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData Architecture

archToString :: Architecture -> String
archToString A_64 = "x86_64"
archToString A_32 = "i386"
archToString A_PowerPC = "powerpc"
archToString A_PowerPC64 = "powerpc64"
archToString A_Sparc = "sparc"
archToString A_Sparc64 = "sparc64"
archToString A_ARM = "arm"
archToString A_ARM64 = "aarch64"

instance Pretty Architecture where
  pPrint = text . archToString

data Platform = Linux LinuxDistro
              -- ^ must exit
              | Darwin
              -- ^ must exit
              | FreeBSD
              | Windows
              -- ^ must exit
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData Platform

platformToString :: Platform -> String
platformToString (Linux distro) = "linux-" ++ distroToString distro
platformToString Darwin = "darwin"
platformToString FreeBSD = "freebsd"
platformToString Windows = "windows"

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
                 -- rolling
                 | Gentoo
                 | Exherbo
                 -- not known
                 | UnknownLinux
                 -- ^ must exit
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData LinuxDistro

distroToString :: LinuxDistro -> String
distroToString Debian = "debian"
distroToString Ubuntu = "ubuntu"
distroToString Mint = "mint"
distroToString Fedora = "fedora"
distroToString CentOS = "centos"
distroToString RedHat = "redhat"
distroToString Alpine = "alpine"
distroToString AmazonLinux = "amazon"
distroToString Gentoo = "gentoo"
distroToString Exherbo = "exherbo"
distroToString UnknownLinux = "unknown"

instance Pretty LinuxDistro where
  pPrint = text . distroToString


-- | An encapsulation of a download. This can be used
-- to download, extract and install a tool.
data DownloadInfo = DownloadInfo
  { _dlUri    :: URI
  , _dlSubdir :: Maybe TarDir
  , _dlHash   :: Text
  , _dlCSize  :: Maybe Integer
  }
  deriving (Eq, Ord, GHC.Generic, Show)

instance NFData DownloadInfo



    --------------
    --[ Others ]--
    --------------


-- | How to descend into a tar archive.
data TarDir = RealDir FilePath
            | RegexDir String     -- ^ will be compiled to regex, the first match will "win"
            deriving (Eq, Ord, GHC.Generic, Show)

instance NFData TarDir

instance Pretty TarDir where
  pPrint (RealDir path) = text path
  pPrint (RegexDir regex) = text regex


-- | Where to fetch GHCupDownloads from.
data URLSource = GHCupURL
               | OwnSource [Either GHCupInfo URI] -- ^ complete source list
               | OwnSpec GHCupInfo
               | AddSource [Either GHCupInfo URI] -- ^ merge with GHCupURL
               deriving (GHC.Generic, Show)

instance NFData URLSource
instance NFData (URIRef Absolute) where
  rnf (URI !_ !_ !_ !_ !_) = ()


data UserSettings = UserSettings
  { uCache       :: Maybe Bool
  , uMetaCache   :: Maybe Integer
  , uNoVerify    :: Maybe Bool
  , uVerbose     :: Maybe Bool
  , uKeepDirs    :: Maybe KeepDirs
  , uDownloader  :: Maybe Downloader
  , uKeyBindings :: Maybe UserKeyBindings
  , uUrlSource   :: Maybe URLSource
  , uNoNetwork   :: Maybe Bool
  , uGPGSetting  :: Maybe GPGSetting
  , uPlatformOverride    :: Maybe PlatformRequest
  }
  deriving (Show, GHC.Generic)

defaultUserSettings :: UserSettings
defaultUserSettings = UserSettings Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

fromSettings :: Settings -> Maybe KeyBindings -> UserSettings
fromSettings Settings{..} Nothing =
  UserSettings {
      uCache = Just cache
    , uMetaCache = Just metaCache
    , uNoVerify = Just noVerify
    , uVerbose = Just verbose
    , uKeepDirs = Just keepDirs
    , uDownloader = Just downloader
    , uNoNetwork = Just noNetwork
    , uKeyBindings = Nothing
    , uUrlSource = Just urlSource
    , uGPGSetting = Just gpgSetting
    , uPlatformOverride = platformOverride
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
            , kShowAllTools = Just bShowAllTools
            }
  in UserSettings {
      uCache = Just cache
    , uMetaCache = Just metaCache
    , uNoVerify = Just noVerify
    , uVerbose = Just verbose
    , uKeepDirs = Just keepDirs
    , uDownloader = Just downloader
    , uNoNetwork = Just noNetwork
    , uKeyBindings = Just ukb
    , uUrlSource = Just urlSource
    , uGPGSetting = Just gpgSetting
    , uPlatformOverride = platformOverride
  }

data UserKeyBindings = UserKeyBindings
  { kUp        :: Maybe Key
  , kDown      :: Maybe Key
  , kQuit      :: Maybe Key
  , kInstall   :: Maybe Key
  , kUninstall :: Maybe Key
  , kSet       :: Maybe Key
  , kChangelog :: Maybe Key
  , kShowAll   :: Maybe Key
  , kShowAllTools :: Maybe Key
  }
  deriving (Show, GHC.Generic)

data KeyBindings = KeyBindings
  { bUp        :: Key
  , bDown      :: Key
  , bQuit      :: Key
  , bInstall   :: Key
  , bUninstall :: Key
  , bSet       :: Key
  , bChangelog :: Key
  , bShowAllVersions :: Key
  , bShowAllTools :: Key
  }
  deriving (Show, GHC.Generic)

instance NFData KeyBindings
instance NFData Key

defaultKeyBindings :: KeyBindings
defaultKeyBindings = KeyBindings
  { bUp = KUp
  , bDown = KDown
  , bQuit = KChar 'q'
  , bInstall = KChar 'i'
  , bUninstall = KChar 'u'
  , bSet = KChar 's'
  , bChangelog = KChar 'c'
  , bShowAllVersions = KChar 'a'
  , bShowAllTools = KChar 't'
  }

data AppState = AppState
  { settings :: Settings
  , dirs :: Dirs
  , keyBindings :: KeyBindings
  , ghcupInfo :: GHCupInfo
  , pfreq :: PlatformRequest
  , loggerConfig :: LoggerConfig
  } deriving (Show, GHC.Generic)

instance NFData AppState

fromAppState :: AppState -> LeanAppState
fromAppState AppState {..} = LeanAppState {..}

data LeanAppState = LeanAppState
  { settings :: Settings
  , dirs :: Dirs
  , keyBindings :: KeyBindings
  , loggerConfig :: LoggerConfig
  } deriving (Show, GHC.Generic)

instance NFData LeanAppState


data Settings = Settings
  { cache            :: Bool
  , metaCache        :: Integer
  , noVerify         :: Bool
  , keepDirs         :: KeepDirs
  , downloader       :: Downloader
  , verbose          :: Bool
  , urlSource        :: URLSource
  , noNetwork        :: Bool
  , gpgSetting       :: GPGSetting
  , noColor          :: Bool -- this also exists in LoggerConfig
  , platformOverride :: Maybe PlatformRequest
  }
  deriving (Show, GHC.Generic)

defaultMetaCache :: Integer
defaultMetaCache = 300 -- 5 minutes

defaultSettings :: Settings
defaultSettings = Settings False defaultMetaCache False Never Curl False GHCupURL False GPGNone False Nothing

instance NFData Settings

data Dirs = Dirs
  { baseDir    :: GHCupPath
  , binDir     :: FilePath
  , cacheDir   :: GHCupPath
  , logsDir    :: GHCupPath
  , confDir    :: GHCupPath
  , dbDir      :: GHCupPath
  , recycleDir :: GHCupPath -- mainly used on windows
  , tmpDir     :: GHCupPath
  }
  deriving (Show, GHC.Generic)

instance NFData Dirs

data KeepDirs = Always
              | Errors
              | Never
  deriving (Eq, Show, Ord, GHC.Generic)

instance NFData KeepDirs

data Downloader = Curl
                | Wget
#if defined(INTERNAL_DOWNLOADER)
                | Internal
#endif
  deriving (Eq, Show, Ord, GHC.Generic)

instance NFData Downloader

data GPGSetting = GPGStrict
                | GPGLax
                | GPGNone
  deriving (Eq, Show, Ord, GHC.Generic)

instance NFData GPGSetting

data DebugInfo = DebugInfo
  { diBaseDir  :: FilePath
  , diBinDir   :: FilePath
  , diGHCDir   :: FilePath
  , diCacheDir :: FilePath
  , diArch     :: Architecture
  , diPlatform :: PlatformResult
  }
  deriving Show


data SetGHC = SetGHCOnly  -- ^ unversioned 'ghc'
            | SetGHC_XY   -- ^ ghc-x.y
            | SetGHC_XYZ  -- ^ ghc-x.y.z
            deriving (Eq, Show)

data SetHLS = SetHLSOnly  -- ^ unversioned 'hls'
            | SetHLS_XYZ  -- ^ haskell-language-server-a.b.c~x.y.z, where a.b.c is GHC version and x.y.z is HLS version
            deriving (Eq, Show)


data PlatformResult = PlatformResult
  { _platform      :: Platform
  , _distroVersion :: Maybe Versioning
  }
  deriving (Eq, Show, GHC.Generic)

instance NFData PlatformResult

platResToString :: PlatformResult -> String
platResToString PlatformResult { _platform = plat, _distroVersion = Just v' }
  = show plat <> ", " <> T.unpack (prettyV v')
platResToString PlatformResult { _platform = plat, _distroVersion = Nothing }
  = show plat

instance Pretty PlatformResult where
  pPrint = text . platResToString

data PlatformRequest = PlatformRequest
  { _rArch     :: Architecture
  , _rPlatform :: Platform
  , _rVersion  :: Maybe Versioning
  }
  deriving (Eq, Show, GHC.Generic)

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
data GHCTargetVersion = GHCTargetVersion
  { _tvTarget  :: Maybe Text
  , _tvVersion :: Version
  }
  deriving (Ord, Eq, Show)

data GitBranch = GitBranch
  { ref  :: String
  , repo :: Maybe String
  }
  deriving (Ord, Eq, Show)

mkTVer :: Version -> GHCTargetVersion
mkTVer = GHCTargetVersion Nothing

tVerToText :: GHCTargetVersion -> Text
tVerToText (GHCTargetVersion (Just t) v') = t <> "-" <> prettyVer v'
tVerToText (GHCTargetVersion Nothing  v') = prettyVer v'

-- | Assembles a path of the form: <target-triple>-<version>
instance Pretty GHCTargetVersion where
  pPrint = text . T.unpack . tVerToText


-- | A comparator and a version.
data VersionCmp = VR_gt Versioning
                | VR_gteq Versioning
                | VR_lt Versioning
                | VR_lteq Versioning
                | VR_eq Versioning
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData VersionCmp


-- | A version range. Supports && and ||, but not  arbitrary
-- combinations. This is a little simplified.
data VersionRange = SimpleRange (NonEmpty VersionCmp) -- And
                  | OrRange (NonEmpty VersionCmp) VersionRange
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData VersionRange

instance Pretty Versioning where
  pPrint = text . T.unpack . prettyV

instance Pretty Version where
  pPrint = text . T.unpack . prettyVer

instance Show (a -> b) where
  show _ = "<function>"

instance Show (IO ()) where
  show _ = "<io>"


data LogLevel = Warn
              | Info
              | Debug
              | Error
  deriving (Eq, Ord, Show)

data LoggerConfig = LoggerConfig
  { lcPrintDebug   :: Bool            -- ^ whether to print debug in colorOutter
  , consoleOutter  :: T.Text -> IO () -- ^ how to write the console output
  , fileOutter     :: T.Text -> IO () -- ^ how to write the file output
  , fancyColors    :: Bool
  }
  deriving Show

instance NFData LoggerConfig where
  rnf (LoggerConfig !lcPrintDebug !_ !_ !fancyColors) = rnf (lcPrintDebug, fancyColors)

data ProcessError = NonZeroExit Int FilePath [String]
                  | PTerminated FilePath [String]
                  | PStopped FilePath [String]
                  | NoSuchPid FilePath [String]
                  deriving Show

instance Pretty ProcessError where
  pPrint (NonZeroExit e exe args) =
    text "Process" <+> pPrint exe <+> text "with arguments" <+> pPrint args <+> text "failed with exit code" <+> text (show e <> ".")
  pPrint (PTerminated exe args) =
    text "Process" <+> pPrint exe <+> text "with arguments" <+> pPrint args <+> text "terminated."
  pPrint (PStopped exe args) =
    text "Process" <+> pPrint exe <+> text "with arguments" <+> pPrint args <+> text "stopped."
  pPrint (NoSuchPid exe args) =
    text "Could not find PID for process running " <+> pPrint exe <+> text " with arguments " <+> text (show args) <+> text "."
data CapturedProcess = CapturedProcess
  { _exitCode :: ExitCode
  , _stdOut   :: BL.ByteString
  , _stdErr   :: BL.ByteString
  }
  deriving (Eq, Show)

makeLenses ''CapturedProcess


data InstallDir = IsolateDir FilePath
                | GHCupInternal
  deriving (Eq, Show)

data InstallDirResolved = IsolateDirResolved FilePath
                        | GHCupDir GHCupPath
                        | GHCupBinDir FilePath
  deriving (Eq, Show)

fromInstallDir :: InstallDirResolved -> FilePath
fromInstallDir (IsolateDirResolved fp) = fp
fromInstallDir (GHCupDir fp) = fromGHCupPath fp
fromInstallDir (GHCupBinDir fp) = fp


isSafeDir :: InstallDirResolved -> Bool
isSafeDir (IsolateDirResolved _) = False
isSafeDir (GHCupDir _)           = True
isSafeDir (GHCupBinDir _)        = False

type PromptQuestion = Text

data PromptResponse = PromptYes | PromptNo
  deriving (Show, Eq)
